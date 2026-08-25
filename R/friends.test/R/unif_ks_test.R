#'
#' unif_ks_test
#'
#' Kolmogorov-Smirnov p-value for the uniformity of a row's ranks across the
#' columns.  Rejecting uniformity is what makes a row a marker.
#'
#' The ranks are integers in \eqn{1 \ldots N}, but \code{\link[stats]{ks.test}}
#' needs a continuous null, so a support has to be named.  Three conventions
#' are offered and they answer slightly different questions.
#'
#' \describe{
#'   \item{\code{"observed"}}{(default) the support is the observed range of
#'     the row, \code{min(ranks)} to \code{max(ranks)}.  The test is then
#'     invariant to where the profile sits and how wide it is, so a row whose
#'     ranks are spread evenly over part of the scale counts as uniform.  That
#'     is deliberate: the method looks for a structural break, not for a shift.
#'     The price is that both endpoints are fitted to the sample, so the
#'     p-value is conservative -- with three columns the test cannot reject at
#'     all.}
#'   \item{\code{"continuity"}}{the support is \eqn{[0.5, N + 0.5]}, spreading
#'     each integer \eqn{i} over \eqn{[i - 0.5, i + 0.5]}.  Nothing is fitted,
#'     so the p-value is calibrated, but concentration into part of the scale
#'     now counts against the null.}
#'   \item{\code{"randomized"}}{the same support, reached exactly rather than
#'     by approximation: each rank is displaced by \eqn{U(-1/2, 1/2)}, and
#'     \eqn{X + U} is exactly uniform on \eqn{[0.5, N + 0.5]} when \eqn{X} is
#'     uniform on the integers.  The cost is that two runs on the same data
#'     give different p-values.}
#' }
#'
#' \code{"continuity"} and \code{"randomized"} need to know \eqn{N}, which is
#' \code{max.possible.rank}.
#'
#' See [friends_test] documentation for details.
#'
#' @param ranks vector of ranks of a row in different columns, \eqn{1 .. N})
#' @param uniform.null how the support of the uniform null is chosen; one of
#' \code{"observed"} (default), \code{"continuity"} or \code{"randomized"},
#' described above.
#' @param max.possible.rank the largest rank a row can take, \eqn{N}, that is
#' the number of rows of the matrix the ranks came from. Required unless
#' \code{uniform.null} is \code{"observed"}, which does not use it.
#' @param simulate.p.value K-S by Monte-Carlo if \code{TRUE};
#' default is \code{FALSE}, see [stats::ks.test()]
#' @param B number of or replicates if \code{simulate.p.value=TRUE}
#' default is 2000, see [stats::ks.test()]
#' @return p-value for the KS test comparing the ranks distribution with uniform
#' @importFrom stats ks.test runif
#' @examples
#' example(row_int_ranks)
#' ks.p.vals <- apply(TF.ranks, 1, "unif_ks_test")
#' unif_ks_test(TF.ranks[42, ], "continuity", max.possible.rank = genes.no)
#' @export
unif_ks_test <- function(
    ranks,
    uniform.null = c("observed", "continuity", "randomized"),
    max.possible.rank = NA,
    simulate.p.value = FALSE,
    B = 2000
) {
    uniform.null <- match.arg(uniform.null)

    if (uniform.null != "observed" &&
            (length(max.possible.rank) != 1L || is.na(max.possible.rank))) {
        stop(
            "max.possible.rank is needed when uniform.null is \"",
            uniform.null, "\"."
        )
    }

    if (uniform.null == "randomized") {
        # X + U(-1/2, 1/2) is exactly uniform on [0.5, N + 0.5] when X is
        # uniform on the integers 1..N, so no approximation is involved
        values <- ranks + runif(length(ranks), -0.5, 0.5)
    } else {
        # a displacement far too small to move anything, there only to break
        # ties so that ks.test is not handed a tied sample
        values <- jitter(ranks, amount = 0.1E-6)
    }

    support <- if (uniform.null == "observed") {
        range(values)
    } else {
        c(0.5, max.possible.rank + 0.5)
    }

    ks.test(
        values, "punif",
        min = support[1L], max = support[2L],
        simulate.p.value = simulate.p.value,
        B = B
    )$p.value
}
