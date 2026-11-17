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
#'     p-value is conservative.}
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
#' The ranks are integers, so a row almost always holds ties.  Nothing is
#' displaced to hide them: \code{\link[stats]{ks.test}} computes the statistic
#' correctly on a tied sample, and only the null distribution is affected --
#' it drops to the asymptotic Kolmogorov law, which this function asks for
#' explicitly.  A tie-free row keeps the exact law, which is calibrated for
#' it.  One consequence is that \code{"observed"} and \code{"continuity"} draw
#' no random numbers and repeat exactly; only \code{"randomized"} varies
#' between runs.
#'
#' The fitted support decreases the sensitivity of the test, and with few
#' columns the loss decides the outcome: under \code{"observed"} the p-value
#' of a row of \eqn{k} columns is bounded below by \eqn{2 / k^k}.  That is
#' 0.074 for \eqn{k = 3}, so a three-column matrix yields no markers at any
#' conventional threshold, and 0.0078, \eqn{6.4 \times 10^{-4}}{6.4e-4} and
#' \eqn{4.3 \times 10^{-5}}{4.3e-5} for four, five and six columns.  The
#' bound is
#' approached, not reached, by a row that puts all of its ranks but one at one
#' end of its own range, and it bounds the raw p-value: a run over many rows
#' has to survive \code{p.adjust.method} as well.  The fixed-support
#' conventions have no such floor -- \code{"continuity"} returns about 0.005
#' for three columns of a tall matrix -- which is the other half of the price
#' of invariance.
#'
#' Under \code{"observed"}, a row holding a single rank leaves the fitted
#' support no width at all.  There is no structural break to find there, so
#' the p-value is 1.  The fixed-support conventions read such a row as the
#' evidence it is.
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
#' @param simulate.p.value ignored: the one-sample
#' \code{\link[stats]{ks.test}} does not use it.
#' @param B ignored, see \code{simulate.p.value}.
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
    # the one-sample ks.test() never read these two, so they are ignored here
    # as well; they stay in the signature so that a call passing them warns
    # instead of failing
    if (!missing(simulate.p.value) || !missing(B)) {
        warning(
            "'simulate.p.value' and 'B' are ignored: ",
            "the one-sample ks.test() does not use them."
        )
    }

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
        values <- as.double(ranks)
    }

    support <- if (uniform.null == "observed") {
        range(values)
    } else {
        c(0.5, max.possible.rank + 0.5)
    }

    # a row holding a single rank leaves the fitted support no width, so the
    # null collapses onto that one point and the row matches it exactly.
    # ks.test cannot say that: its formula assumes a continuous null and
    # reports D = 1. The fixed-support conventions need no such care.
    if (uniform.null == "observed" && support[1L] == support[2L]) {
        return(1)
    }

    ties <- anyDuplicated(values) > 0L

    ks <- function() {
        ks.test(
            values, "punif",
            min = support[1L], max = support[2L],
            exact = if (ties) FALSE else NULL
        )$p.value
    }

    # A tied sample has no continuous null, so ks.test drops to the asymptotic
    # Kolmogorov law and warns. Ask for that law explicitly, which keeps the
    # choice visible and independent of ks.test's own default, and muffle the
    # warning just answered -- its text is translated, so matching it is not
    # reliable.
    if (ties) suppressWarnings(ks()) else ks()
}
