#' Fit step models for a row's rank profile (compact O(ncol) method)
#'
#' For each possible number of friends \eqn{k_1 \in 1 \ldots k-1}, finds
#' the log-likelihood-maximising split rank \eqn{\ell_1} by exploiting the
#' convexity of the log-likelihood in \eqn{\ell_1} over the valid range
#' \eqn{[\mathrm{sorted\_ranks}[k_1],\ \mathrm{sorted\_ranks}[k_1+1]-1]}.
#' Total work is O(k) = O(ncol) per row, compared to O(max.possible.rank)
#' for the full-mesh enumeration in [step.fit.ln.likelihoods.fullmesh.enum].
#'
#' The input ranks are integers in \eqn{1 \ldots \mathrm{max.possible.rank}}.
#' A step model splits columns into a "friends" group (ranks
#' \eqn{\leq \ell_1}) and a "non-friends" group (ranks \eqn{> \ell_1}).
#' When adjacent ranks are tied the valid range for that \eqn{k_1} is empty
#' and the corresponding entry is \code{-Inf}.
#'
#' See [friends.test] documentation for details.
#'
#' @param ranks integer vector of ranks of a row in different columns
#' @param max.possible.rank number of rows, i.e. maximal possible rank
#' @return a list of four values:\cr
#' \code{columns.order} is the permutation that sorts \code{ranks} ascending;\cr
#' \code{best_ll_by_k1} length-\eqn{k} numeric vector; entry \eqn{k_1} is the
#' maximum log-likelihood over all valid \eqn{\ell_1} for that number of
#' friends (\code{-Inf} when no valid \eqn{\ell_1} exists);\cr
#' \code{best_l1_by_k1} length-\eqn{k} integer vector; the \eqn{\ell_1}
#' achieving \code{best_ll_by_k1};\cr
#' \code{uniform_ll} log-likelihood of the uniform (no-friends) model.\cr
#' @seealso [step.fit.ln.likelihoods.fullmesh.enum] for the full-mesh
#' O(max.possible.rank) reference implementation,
#' [step.fit.ln.likelihoods.fullmesh] for the same result in fullmesh format
#' computed via this function.
#' @examples
#' example(row.int.ranks)
#' steps <- step.fit.ln.likelihoods(TF.ranks[42, ], genes.no)
#' @export
step.fit.ln.likelihoods <- function(ranks, max.possible.rank) {
    if (max.possible.rank < max(ranks)) {
        stop("Rows_no parameter is the maximal possible rank,
    it cannot be less than max(ranks)!")
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
    if (max.possible.rank <= length(ranks)) {
        .step_fit_fullmesh(ranks, max.possible.rank)
    } else {
        .step_fit_compact(ranks, max.possible.rank)
    }
}


#' Fit step models for a row's rank profile (full-mesh enumeration)
#'
#' Fits the bi-uniform step model by enumerating every possible split rank
#' \eqn{\ell_1 \in 1 \ldots \mathrm{max.possible.rank}-1} explicitly.
#' The last entry (\eqn{\ell_1 = \mathrm{max.possible.rank}}) stores the
#' log-likelihood of the uniform (no-friends) model.
#' Total work is O(max.possible.rank) per row.
#'
#' This is the original reference implementation, kept for validation and
#' for use cases where the full log-likelihood profile over all split ranks
#' is needed (e.g. plotting).  For all production work, prefer
#' [step.fit.ln.likelihoods], which is O(ncol) and returns the same
#' optimal split.
#'
#' See [friends.test] documentation for details.
#'
#' @inheritParams step.fit.ln.likelihoods
#' @return a list of three values:\cr
#' \code{columns.order} is the permutation that sorts \code{ranks}
#' ascending;\cr
#' \code{ln.likelihoods} length-\eqn{\mathrm{max.possible.rank}} numeric
#' vector; entry \eqn{\ell_1} is the log-likelihood of the step model with
#' split rank \eqn{\ell_1} (or the uniform model for the last entry);\cr
#' \code{k1.by.l1} length-\eqn{\mathrm{max.possible.rank}} integer vector;
#' entry \eqn{\ell_1} is the number of ranks \eqn{\leq \ell_1}.\cr
#' @seealso [step.fit.ln.likelihoods] for the O(ncol) compact method,
#' [step.fit.ln.likelihoods.fullmesh] for the fullmesh format derived from it.
#' @examples
#' example(row.int.ranks)
#' steps <- step.fit.ln.likelihoods.fullmesh.enum(TF.ranks[42, ], genes.no)
#' @export
step.fit.ln.likelihoods.fullmesh.enum <- function(ranks, max.possible.rank) {
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
    sorted_ranks <- ranks[columns.order]
    ln.likelihoods <- rep(0, max.possible.rank)
    k1.by.l1 <- rep(0, max.possible.rank)
    k <- length(sorted_ranks)
    k1 <- 0
    # l1==max.possible.rank is "no step"
    for (l1 in seq_len(max.possible.rank - 1)) {
        while (k1 < k && sorted_ranks[k1 + 1] <= l1) {
            k1 <- k1 + 1
        }
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


#' Fit step models for a row's rank profile (fullmesh format via compact method)
#'
#' A wrapper around [step.fit.ln.likelihoods] that expands the compact
#' O(ncol) result into the full log-likelihood profile over all split ranks
#' \eqn{\ell_1 \in 1 \ldots \mathrm{max.possible.rank}}, matching the output
#' format of [step.fit.ln.likelihoods.fullmesh.enum].
#'
#' This function exists primarily to verify that the fast compact algorithm
#' and the reference full-mesh enumeration agree.  It carries the same
#' O(max.possible.rank) expansion cost as the enumeration.
#'
#' @inheritParams step.fit.ln.likelihoods
#' @return Identical format to [step.fit.ln.likelihoods.fullmesh.enum]:
#' a list with \code{columns.order}, \code{ln.likelihoods}, and
#' \code{k1.by.l1}.
#' @seealso [step.fit.ln.likelihoods.fullmesh.enum],
#' [step.fit.ln.likelihoods]
#' @examples
#' example(row.int.ranks)
#' steps <- step.fit.ln.likelihoods.fullmesh(TF.ranks[42, ], genes.no)
#' @export
step.fit.ln.likelihoods.fullmesh <- function(ranks, max.possible.rank) {
    compact <- step.fit.ln.likelihoods(ranks, max.possible.rank)
    columns.order <- compact$columns.order
    sorted_ranks <- ranks[columns.order]
    k <- length(sorted_ranks)

    ln.likelihoods <- rep(0, max.possible.rank)
    k1.by.l1 <- rep(0L, max.possible.rank)
    k1 <- 0L
    for (l1 in seq_len(max.possible.rank - 1L)) {
        while (k1 < k && sorted_ranks[k1 + 1L] <= l1) {
            k1 <- k1 + 1L
        }
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
    ln.likelihoods[max.possible.rank] <- compact$uniform_ll
    k1.by.l1[max.possible.rank] <- k

    list(
        columns.order = columns.order,
        ln.likelihoods = ln.likelihoods,
        k1.by.l1 = k1.by.l1
    )
}


# Internal O(nrow) per-row step-model core for wide matrices (ncol >= nrow).
#
# Iterates over all split ranks l1 in 1:(max.possible.rank-1) and directly
# accumulates the best log-likelihood and best l1 for each k1.
# Tie-breaking: prefer larger l1 (>= comparison), consistent with
# .step_fit_compact.
# Total work: O(max.possible.rank) = O(nrow) per row.
#
# No parameter checks — callers are responsible for valid input.
# Public entry point with checks: step.fit.ln.likelihoods().
#
# Returns the same compact list as .step_fit_compact:
#   columns.order, best_ll_by_k1, best_l1_by_k1, uniform_ll
.step_fit_fullmesh <- function(ranks, max.possible.rank) {
    columns.order <- order(ranks)
    sorted_ranks <- ranks[columns.order]  # ascending
    k <- length(sorted_ranks)

    best_ll_by_k1 <- rep(-Inf, k)
    best_l1_by_k1 <- integer(k)

    k1 <- 0L
    for (l1 in seq_len(max.possible.rank - 1L)) {
        while (k1 < k && sorted_ranks[k1 + 1L] <= l1) k1 <- k1 + 1L
        if (k1 == 0L || k1 == k) next  # all ranks on one side: no valid split

        p1 <- k1 / k
        ll <- k1 * log(p1 / l1) +
            (k - k1) * log((1 - p1) / (max.possible.rank - l1))

        # >= to prefer larger l1 on exact tie
        # (same convention as .step_fit_compact)
        if (ll >= best_ll_by_k1[k1]) {
            best_ll_by_k1[k1] <- ll
            best_l1_by_k1[k1] <- l1
        }
    }

    list(
        columns.order = columns.order,
        best_ll_by_k1 = best_ll_by_k1,
        best_l1_by_k1 = best_l1_by_k1,
        uniform_ll    = k * log(1 / max.possible.rank)
    )
}


# Internal O(ncol) per-row step-model core.
#
# For each possible number-of-friends k1 in 1:(k-1), the log-likelihood
# is *convex* in the split rank l1, so the maximum over the valid range
# [sorted_ranks[k1], sorted_ranks[k1+1]-1] is always at one of the two
# boundary points.  We evaluate at both endpoints and keep the better one.
# Total work: O(k) = O(ncol) per row instead of O(nrow).
#
# No parameter checks — callers are responsible for valid input.
# Public entry point with checks: step.fit.ln.likelihoods().
#
# Returns a list with:
#   columns.order   — integer vector of column indices sorted by ascending rank
#   best_ll_by_k1   — length-k vector; entry k1 = best log-likelihood for k1
#                     friends (-Inf when the valid range for that k1 is empty)
#   best_l1_by_k1   — length-k vector; entry k1 = the optimal split rank l1
#                     achieving best_ll_by_k1[k1]
#   uniform_ll      — log-likelihood of the uniform (no-friends) model
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

        ll_min <- k1 * (log_p1 - log(l1_min)) +
            (k - k1) * (log_1p1 - log(max.possible.rank - l1_min))

        if (l1_min == l1_max) {
            best_l1_by_k1[k1] <- l1_min
            best_ll_by_k1[k1] <- ll_min
        } else {
            # Prefer larger l1 on exact tie
            # (consistent with max(which(...)) convention)
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
