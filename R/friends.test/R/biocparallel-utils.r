#' Normalise a BiocParallel backend parameter
#'
#' Validates and configures a \code{BiocParallelParam} object for use in
#' \code{friends.test} and \code{friends.test.bic}.  When \code{BPPARAM} is
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
ft_bpparam <- function(BPPARAM = NULL, .progress = FALSE) {
    if (is.null(BPPARAM)) {
        BPPARAM <- BiocParallel::SerialParam()
    }
    # For SnowParam use BiocParallel's own progress bar; for other backends
    # (MulticoreParam, SerialParam) progressr + handler_cli() is used instead.
    use_bp_bar <- isTRUE(.progress) && class(BPPARAM) == "SnowParam"
    BiocParallel::bpprogressbar(BPPARAM) <- use_bp_bar
    BPPARAM
}


#' Parallel lapply returning a numeric vector
#'
#' A thin wrapper around \code{\link[BiocParallel]{bplapply}} that unlists the
#' result into a plain (unnamed) numeric vector.  Intended for row-wise scoring
#' where each worker returns a single double value.
#'
#' @param X A vector or list to iterate over.
#' @param FUN A function to apply to each element of \code{X}.
#' @param ... Additional arguments passed to \code{FUN} via
#'   \code{bplapply}.
#' @param BPPARAM A \code{\link[BiocParallel]{BiocParallelParam}} object
#'   (required; obtain one from \code{\link{ft_bpparam}}).
#'
#' @return A numeric vector of length \code{length(X)}, without names.
#'
#' @keywords internal
#' @noRd
ft_bplapply_dbl <- function(X, FUN, ..., BPPARAM) {
    unlist(
        BiocParallel::bplapply(
            X, FUN, ...,
            BPPARAM = BPPARAM
        ),
        use.names = FALSE
    )
}


#' Parallel mapply returning a list
#'
#' A thin wrapper around \code{\link[BiocParallel]{bpmapply}} with
#' \code{SIMPLIFY = FALSE} and \code{USE.NAMES = FALSE}, so the result is
#' always an unnamed list regardless of what \code{FUN} returns.  Used to
#' iterate over pairs of rows and columns in the step-fit stage.
#'
#' @param FUN A function whose first arguments are taken from the parallel
#'   vectors in \code{...}.
#' @param ... Parallel vectors or lists passed as positional arguments to
#'   \code{FUN}.
#' @param MoreArgs A named list of additional constant arguments passed to
#'   \code{FUN} (forwarded to \code{bpmapply}).
#' @param BPPARAM A \code{\link[BiocParallel]{BiocParallelParam}} object
#'   (required; obtain one from \code{\link{ft_bpparam}}).
#'
#' @return An unnamed list of the same length as the shortest element of
#'   \code{...}.
#'
#' @keywords internal
#' @noRd
ft_bpmapply_list <- function(FUN, ..., MoreArgs = NULL, BPPARAM) {
    BiocParallel::bpmapply(
        FUN = FUN,
        ...,
        MoreArgs = MoreArgs,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE,
        BPPARAM = BPPARAM
    )
}
