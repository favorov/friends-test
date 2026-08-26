#' Normalise a BiocParallel backend parameter
#'
#' Validates and configures a \code{BiocParallelParam} object for use in
#' \code{friends_test_ks} and \code{friends_test_bic}.  When \code{BPPARAM} is
#' \code{NULL} the function falls back to
#' \code{\link[BiocParallel]{SerialParam}}
#' so that callers always receive a usable backend.  The progress bar is
#' enabled or disabled according to \code{.progress}.
#'
#' @param BPPARAM A \code{\link[BiocParallel]{BiocParallelParam}} object, or
#'   \code{NULL} (default) to use \code{SerialParam}.
#' @param .progress Logical scalar.  \code{TRUE} enables the backend's built-in
#'   progress bar; \code{FALSE} (default) disables it.
#'
#' @return The (possibly newly created) \code{BiocParallelParam} object with the
#'   progress bar set as requested.
#'
#' @keywords internal
#' @noRd
# cli progress bar format for a finished bar: show the elapsed time
# instead of the "ETA: 0s" the default format ends with.
ft_pb_format_done <- paste0(
    "{cli::pb_name}{cli::pb_bar} ",
    "{cli::pb_percent} | {cli::pb_elapsed}"
)


ft_bpparam <- function(BPPARAM = NULL, .progress = FALSE) {
    if (is.null(BPPARAM)) {
        BPPARAM <- BiocParallel::SerialParam()
    }
    BiocParallel::bpprogressbar(BPPARAM) <- FALSE
    BPPARAM
}


#' Run a per-row function over the rows of a rank matrix
#'
#' The single place that knows how a row-wise stage is executed.  With a
#' progress bar requested on a serial backend it draws a \code{cli} bar;
#' otherwise it goes through \code{\link[BiocParallel]{bpmapply}}.
#'
#' \code{FUN} is called as \code{FUN(row, i, ...)}, where the extra arguments
#' come from \code{MoreArgs}.  It must take everything it needs through its
#' arguments: its environment is replaced by the global one so that a
#' \code{SnowParam} worker can deserialize it without loading the
#' \code{friends.test} namespace.  The library paths of the parent are
#' forwarded and applied on the worker before \code{FUN} runs, because
#' \code{R CMD build} installs the package into a temporary directory that is
#' not on a fresh worker's \code{.libPaths()}.  They are only set when they
#' actually differ: setting them is two orders of magnitude dearer than
#' reading them, and the row-wise stages run this once per row.
#'
#' Constants belong in \code{MoreArgs}: BiocParallel hands them to a worker
#' once per chunk rather than rebuilding them for every row.
#'
#' @param FUN A function of \code{(row, i, ...)}.
#' @param rows A list of rank rows.
#' @param idx A vector of row indices, the same length as \code{rows}.
#' @param MoreArgs A named list of constants for \code{FUN}.
#' @param BPPARAM A \code{BiocParallelParam} object.
#' @param .progress Logical; draw a progress bar when the backend is serial.
#' @param label Bar or step label.
#'
#' @return An unnamed list of the same length as \code{rows}.
#'
#' @keywords internal
#' @noRd
.ft_map_rows <- function(
    FUN, rows, idx, MoreArgs = list(), BPPARAM,
    .progress = FALSE, label = NULL
) {
    environment(FUN) <- globalenv()

    if (.progress && is(BPPARAM, "SerialParam")) {
        cli::cli_progress_done()   # close whatever step is open
        along <- cli::cli_progress_along(
            rows,
            name = label,
            clear = FALSE,
            format_done = ft_pb_format_done
        )
        # do.call costs about 0.8 us a row, and only this path pays it; the
        # bar's own rendering is far more expensive than that.
        return(lapply(
            along,
            function(j) do.call(FUN, c(list(rows[[j]], idx[[j]]), MoreArgs))
        ))
    }

    if (.progress) cli::cli_progress_step(paste0(label, "..."))
    # The worker also carries the global environment, so what travels to a
    # SnowParam process is FUN and plain data, never the package namespace.
    worker <- function(row, i, .fun, .libs, ...) {
        # Setting the library paths costs about 34 us against 0.2 us for
        # reading them, so do it only where it is actually needed: on a fresh
        # SnowParam worker the first row sets them, every later row skips.
        if (!identical(.libPaths(), .libs)) .libPaths(.libs)
        .fun(row, i, ...)
    }
    environment(worker) <- globalenv()
    out <- BiocParallel::bpmapply(
        FUN = worker,
        rows, idx,
        MoreArgs = c(list(.fun = FUN, .libs = .libPaths()), MoreArgs),
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE,
        BPPARAM = BPPARAM
    )
    if (.progress) cli::cli_progress_done()
    out
}


#' The prologue shared by the two main functions
#'
#' Validates the arguments both branches have in common, gives the matrix
#' default dimnames, normalises the backend and ranks the matrix.  Each branch
#' is then left with only the checks of its own parameters.
#'
#' @param A The association matrix.
#' @param max.friends.n The caller's value; \code{"all"} and \code{NULL} both
#'   mean \code{ncol(A)}.  The caller is responsible for
#'   \code{cli.progress_show_after}: the option has to outlive this call, so it
#'   cannot be restored from here.
#' @param .progress Logical scalar.
#' @param BPPARAM A \code{BiocParallelParam} object or \code{NULL}.
#'
#' @return A list with \code{A} (named), \code{max.friends.n} (resolved to a
#'   number), \code{BPPARAM}, \code{ranks} and \code{rows}.
#'
#' @keywords internal
#' @noRd
.ft_prepare <- function(A, max.friends.n, .progress, BPPARAM) {
    if (is.null(A) || (length(dim(A)) != 2)) {
        stop("The first parameter must be a non-empty 2D matrix-like object.")
    }

    if (is.null(max.friends.n) || identical(max.friends.n, "all")) {
        max.friends.n <- ncol(A)
    } else if (!is.numeric(max.friends.n) ||
            length(max.friends.n) != 1L || is.na(max.friends.n)) {
        stop("max.friends.n must be a single number, \"all\", or NULL.")
    }
    if (max.friends.n < 1 || max.friends.n > ncol(A)) {
        stop("max.friends.n must be between 1 and the number of columns.")
    }

    if (is.null(dimnames(A)[[1]])) rownames(A) <- seq_len(nrow(A))
    if (is.null(dimnames(A)[[2]])) colnames(A) <- seq_len(ncol(A))

    BPPARAM <- ft_bpparam(BPPARAM = BPPARAM, .progress = .progress)

    if (.progress) cli::cli_progress_step("Ranking...")
    ranks <- row_int_ranks(A)

    list(
        A = A,
        max.friends.n = max.friends.n,
        BPPARAM = BPPARAM,
        ranks = ranks,
        rows = purrr::array_branch(ranks, 1)
    )
}
