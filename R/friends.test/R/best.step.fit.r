#'
#' best.step.fit
#'
#' finds the ML-best step model for one row
#'
#' See [friends.test] documentation for details.
#'
#' @inheritParams step.fit.ln.likelihoods
#' @return a list of four values: \cr
#' \code{step.models} is return from [step.fit.ln.likelihoods] call,
#' which the function starts with
#' \code{best.step.rank} is the rank value that makes the best step;
#' it is not obligatory one on the \code{ranks} value.\cr
#' \code{columns.on.left} is
#' the vector of the columns on the left of the best step
#' (including the step value). They are friends of the row.\cr
#' \code{columns.on.right} is vector of those on the right \cr
#' \code{population.on.left} is how many ranks are on left of split;
#' they are friends! \cr
#' @examples
#' example(row.int.ranks)
#' step <- best.step.fit(TF.ranks[42, ], genes.no)
#' @export
best.step.fit <- function(ranks, max.possible.rank) {
    step.models <- friends.test:::.step_fit_compact(ranks, max.possible.rank)
    k <- length(ranks)

    # Valid k1 values: 1..(k-1) with a non-empty valid l1 range
    valid_k1 <- which(is.finite(step.models$best_ll_by_k1))

    if (length(valid_k1) == 0L) {
        # Degenerate: all ranks tied — no valid step model, treat as uniform
        return(list(
            step.models        = step.models,
            best.step.rank     = max.possible.rank,
            columns.on.left    = integer(0L),
            columns.on.right   = step.models$columns.order,
            population.on.left = 0L
        ))
    }

    max.ln.l <- max(step.models$best_ll_by_k1[valid_k1])
    tied_k1  <- valid_k1[step.models$best_ll_by_k1[valid_k1] == max.ln.l]
    # Among ties keep the split with the largest l1 (consistent with the
    # historical max(which(ln.likelihoods == max_ll)) convention)
    best_k1        <- tied_k1[which.max(step.models$best_l1_by_k1[tied_k1])]
    best.step.rank <- step.models$best_l1_by_k1[best_k1]
    population.on.left <- best_k1

    list(
        step.models        = step.models,
        best.step.rank     = best.step.rank,
        columns.on.left    = step.models$columns.order[seq_len(population.on.left)],
        columns.on.right   = step.models$columns.order[
            seq(population.on.left + 1L, k)
        ],
        population.on.left = population.on.left
    )
}
