#'
#' friends_test
#'
#' We have two sets: T (rows) and C (columns) and
#' A real matrix A(t,c) that describes the strength of association
#' between each t and each c; t is an element of T and c is an element of C.
#' For each t we want to identify whether it is significantly more
#' relevant for some c's than for the remaining c's.
#' If it does, those c for which the t is relevant,
#' are the t's friend. And, the t is the c's marker.
#'
#' Two ways of deciding whether a row really has friends are provided, and
#' they ask different questions.  \code{mode = "ks"} (the default) runs
#' [friends_test_ks], which tests whether the row's ranks are uniformly
#' spread over the columns, wherever that spread happens to sit.
#' \code{mode = "bic"} runs [friends_test_bic], which compares a step model
#' against a uniform one over the whole rank scale.  Both return the same
#' structure, so a pipeline can switch between them.
#'
#' Arguments other than the ones listed below are passed to the function
#' selected by \code{mode}; see its documentation for what each mode accepts.
#' Passing an argument that belongs to the other mode is an error.
#'
#' If you want to run the row-wise calculations in parallel,
#' pass a [BiocParallel::BiocParallelParam-class] object via \code{BPPARAM},
#' for instance \code{BiocParallel::MulticoreParam(workers = 4)} on Unix-like
#' systems or \code{BiocParallel::SnowParam(workers = 4)} on all platforms.
#'
#' @param A original association matrix
#' @param mode which test decides whether a row has friends: \code{"ks"}
#' (default) for the Kolmogorov-Smirnov branch, [friends_test_ks], or
#' \code{"bic"} for the Bayesian one, [friends_test_bic].
#' @param ... further arguments for the function selected by \code{mode}.
#' @param max.friends.n The maximal number of friends for a marker.
#' A value $n$ means that we filter out a row if it has more
#' than $n$ friendly columns. 1 means we look only for unique (best) friends.
#' The string "all" (default) means the same as \code{ncols(A)} value,
#' do not filter markers by this parameter.
#' @param .progress if \code{TRUE}, show simple progress messages and enable
#' the text progress bar of the selected \code{BPPARAM}. The default is
#' \code{FALSE}.
#' @param BPPARAM a [BiocParallel::BiocParallelParam-class] instance that
#' controls whether the row-wise work is run serially or in parallel. The
#' default is \code{BiocParallel::SerialParam()}.
#' @return \code{list}; each element represents a marker, *e.g.*,
#' a matrix row that has friend(s). Each element of the return list
#' is also a list, one element per friend, and the 2-nd level element
#' is an integer vector with three numbers, that are:
#' the marker coordinate (\code{marker}),
#' the friend coordinate (\code{friend}), and
#' the the rank of the friend for the marker (\code{rank}).
#' So, it is list of lists of simple integer vectors, each
#' vector represents a marker+friend pair,
#' the inner lists enumerate friends,
#' the outer (return) list enumerate markers.
#' @seealso [friends_test_ks], [friends_test_bic]
#' @examples
#' A <- matrix(
#'     c(
#'         10, 6, 7, 8, 9,
#'         9, 10, 6, 7, 8,
#'         8, 9, 10, 6, 7,
#'         7, 8, 9, 10, 6,
#'         6, 7, 8, 9, 10,
#'         20, 0, 0, 0, 0
#'     ),
#'     nrow = 6, ncol = 5, byrow = TRUE
#' )
#' A
#' friends_test(A, threshold = .05)
#' friends_test(A, mode = "bic", prior.to.have.friends = 0.5)
#'
#' @export
#'
friends_test <- function(
    A = NULL,
    mode = c("ks", "bic"),
    ...,
    max.friends.n = "all",
    .progress = FALSE,
    BPPARAM = NULL
) {
    mode <- match.arg(mode)
    fun <- switch(mode, ks = friends_test_ks, bic = friends_test_bic)
    other <- switch(mode, ks = friends_test_bic, bic = friends_test_ks)

    # An argument belonging to the branch we are not running is a mistake about
    # the mode rather than a typo, and the message should say so.  Names that
    # belong to neither branch are left to the callee, which reports them as
    # unused arguments, and abbreviated names are left to R's own matching.
    misplaced <- intersect(
        ...names(),
        setdiff(names(formals(other)), names(formals(fun)))
    )
    if (length(misplaced) > 0L) {
        stop(
            sprintf(
                "argument%s %s belong%s to mode \"%s\", not to mode \"%s\".",
                if (length(misplaced) > 1L) "s" else "",
                paste(sQuote(misplaced), collapse = ", "),
                if (length(misplaced) > 1L) "" else "s",
                switch(mode, ks = "bic", bic = "ks"),
                mode
            ),
            call. = FALSE
        )
    }

    fun(
        A, ...,
        max.friends.n = max.friends.n,
        .progress = .progress,
        BPPARAM = BPPARAM
    )
}
