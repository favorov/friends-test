#'
##' (descending in each column) ranks of the same row
#' in different columns
#' (the \code{ranks} parameter)
#' The input ranks are integers in \eqn{1..max.possible.rank}.
#' Each of the split rank values \eqn{1 .. max.possible.rank-1}
#' split all the ranks into two steps: "this or less" and
#' "greater than this".
#'
#' The function fits the step (bi-uniform) model for each
#' integer splitting value in \eqn{1..max.possible.rank-1};
#' the splitting value is in the maximal value in the left part
#' of the ordering (the original ranks are descending, so best
#' columns are in the left part) and calculates the likelihood.
#' The last value with the index \eqn{max.possible.rank} is calculated for a
#' non-step uniform model.

#' See [friends.test] documentation for details.
#'
#' @param ranks vector of ranks of a row in different columns
#' @param max.possible.rank number of rows, i.e. maximal rankhoods
#'
#' fits possible bi-uniform step models for a set of descending
#' (descending in each column) ranks of the same row
#'  in different columns
#' (the \code{ranks} parameter)
#' The input ranks are integers in \eqn{1..max.possible.rank}.
#' Each of the split rank values \eqn{1 .. max.possible.rank-1}
#' split all the ranks into two steps: "this or less" and
#' "greater than this".
#'
#' The function fits the step (bi-uniform) model for each
#' integer splitting value in \eqn{1..max.possible.rank-1};
#' the splitting value is in the maximal value in the left part
#' of the ordering (the original ranks are descending, so best
#' columns are in the left part) and calculates the likelihood.
#' The last value with the index \eqn{max.possible.rank} is calculated for a
#' non-step uniform model.

#' See [friends.test] documentation for details.
#'
#' @param ranks vector of ranks of a row in different columns
#' @param max.possible.rank number of rows, i.e. maximal rank
#' @return a list of three values: \cr
#' \code{columns.order} is the order of ranks in, column-by-column\cr
#' \code{ln.likelihoods} the ln of the likelihood of each of models
#' corresponding to each split rank value in \eqn{1..max.possible.rank-1}
#' and the last, correspond to just uniform, no step\cr
#' \code{k1.by.l1} contains \eqn{k_1}, that is the number of ranks on the
#' on left of the step, including the split value, for split values
#' \eqn{1..max.possible.rank};\cr
#' @examples
#' example(row.int.ranks)
#' steps <- step.fit.ln.likelihoods(TF.ranks[42, ], genes.no)
#' @export
# Internal O(ncol) per-row step-model fitter.
#
# For each possible number-of-friends k1 in 1:(k-1), the log-likelihood
# is *convex* in the split rank l1, so the maximum over the valid range
# [sorted_ranks[k1], sorted_ranks[k1+1]-1] is always at one of the two
# boundary points.  We evaluate at both endpoints and keep the better one.
# Total work: O(k) = O(ncol) per row instead of O(nrow) — a crucial
# speedup for large matrices (VisiumHD, scRNA-seq, etc.).
#
# Returns a list with:
#   columns.order   — integer vector of column indices sorted by ascending rank
#   best_ll_by_k1   — length-k vector; entry k1 = best log-likelihood for k1
#                     friends (−Inf when the valid range for that k1 is empty)
#   best_l1_by_k1   — length-k vector; entry k1 = the optimal split rank l1
#                     achieving best_ll_by_k1[k1]
#   uniform_ll      — log-likelihood of the uniform (no-friends) model
#
# This is *not* exported; callers are best.step.fit and best.step.fit.bic.
.step_fit_compact <- function(ranks, max.possible.rank) {
    columns.order <- order(ranks)
    sorted_ranks <- ranks[columns.order]  # ascending
    k <- length(sorted_ranks)

    best_ll_by_k1 <- rep(-Inf, k)
    best_l1_by_k1 <- integer(k)

    for (k1 in seq_len(k - 1L)) {
        l1_min <- sorted_ranks[k1]
        l1_max <- sorted_ranks[k1 + 1L] - 1L
        if (l1_min > l1_max) next  # empty range: adjacent ranks are tied

        p1      <- k1 / k
        log_p1  <- log(p1)
        log_1p1 <- log(1 - p1)

        # Evaluate log-likelihood at the left boundary
        ll_min <- k1 * (log_p1 - log(l1_min)) +
            (k - k1) * (log_1p1 - log(max.possible.rank - l1_min))

        if (l1_min == l1_max) {
            best_l1_by_k1[k1] <- l1_min
            best_ll_by_k1[k1] <- ll_min
        } else {
            # Evaluate at the right boundary; prefer larger l1 on exact tie
            # (consistent with the historical max(which(ll == max_ll)) rule)
            ll_max <- k1 * (log_p1 - log(l1_max)) +
                (k - k1) * (log_1p1 - log(max.possible.rank - l1_max))
            if (ll_max >= ll_min) {
                best_l1_by_k1[k1] <- l1_max
                best_ll_by_k1[k1] <- ll_max
            } else {
                best_l1_by_k1[k1] <- l1_min
                best_ll_by_k1[k1] <- ll_min
            }
        }
    }

    list(
        columns.order = columns.order,
        best_ll_by_k1 = best_ll_by_k1,
        best_l1_by_k1 = best_l1_by_k1,
        uniform_ll    = k * log(1 / max.possible.rank)
    )
}

step.fit.ln.likelihoods <- function(ranks, max.possible.rank) {
    if (max.possible.rank < max(ranks)) {
        stop("Rows_no parameter is the maximal possible rank,
    it cannot be less then max(ranks)!")
    }
    if (!all(ranks - floor(ranks) == 0)) {
        stop("Ranks are to be integer!")
    }
    if (!all(ranks >= 1)) {
        stop("Ranks are to be integer!")
    }
    if (!is.null(dim(ranks))) {
        warning("Ranks has not-NULL dim(), it is not a vector.\n")
    }


    columns.order <- order(ranks)
    ranks <- ranks[columns.order]
    ln.likelihoods <- rep(0, max.possible.rank)
    k1.by.l1 <- rep(0, max.possible.rank)
    k <- length(ranks)
    k1 <- 0
    # l1==max.possible.rank is "no step"
    for (l1 in seq_len(max.possible.rank - 1)) {
        # we enumerate models by their l_i parameter
        while (k1 < k && ranks[k1 + 1] <= l1) {
            k1 <- k1 + 1
        } # l1 has hit next rank value
        k1.by.l1[l1] <- k1
        p1 <- k1 / k
        if (p1 > 0) {
            ln.likelihoods[l1] <-
                ln.likelihoods[l1] + k1 * log(p1 / l1)
        }
        if (p1 < 1) {
            ln.likelihoods[l1] <-
                ln.likelihoods[l1] +
                (k - k1) * log((1 - p1) / (max.possible.rank - l1))
        }
    }
    ln.likelihoods[max.possible.rank] <- k * log(1 / max.possible.rank)
    k1.by.l1[max.possible.rank] <- k

    list(
        columns.order = columns.order,
        ln.likelihoods = ln.likelihoods,
        k1.by.l1 = k1.by.l1
    )
}
