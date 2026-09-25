# The part of a step fit that does not depend on how the winner is chosen.
# best_step_fit() and best_step_fit_bic() differ only in that choice; both
# search the same candidates and build the same return list.


# The rank scale the step models live on.  "observed" takes both ends from the
# row, which is what unif_ks_test() does by default; "continuity" keeps the
# whole 1..max.possible.rank scale.  The ranks come back rebased to 1..size,
# with the offset that puts a split rank back on the scale of the input.
.rank_scale <- function(ranks, max.possible.rank, uniform.null) {
    if (uniform.null == "observed") {
        lowest <- min(ranks)
        return(list(
            ranks  = ranks - lowest + 1L,
            size   = max(ranks) - lowest + 1L,
            offset = lowest - 1L
        ))
    }
    list(ranks = ranks, size = max.possible.rank, offset = 0L)
}


# The best number of friends k1 among those with a non-empty valid l1 range,
# ties broken towards the fewest friends.  On equal likelihood the smaller
# friend set is the parsimonious one, which is the reading that also sends a
# tie between the step and the uniform model to the uniform one.  The choice
# matters on a fitted rank scale, where a short scale leaves many k1 equally
# likely; on the full scale nothing ties.
#
# Returns k1 = NA when no k1 is valid, which happens when every rank is tied,
# together with the log-likelihood attained, -Inf in that case.
.best_valid_k1 <- function(step.models) {
    valid_k1 <- which(is.finite(step.models$best_ll_by_k1))
    if (length(valid_k1) == 0L) {
        return(list(k1 = NA_integer_, max.ln.l = -Inf))
    }
    max.ln.l <- max(step.models$best_ll_by_k1[valid_k1])
    tied_k1 <- valid_k1[step.models$best_ll_by_k1[valid_k1] == max.ln.l]
    list(k1 = min(tied_k1), max.ln.l = max.ln.l)
}


# Build the return list of a step fit.  k1 of NA or 0 means no step was
# accepted, so every column ends up on the right and there are no friends.
.assemble_step <- function(step.models, k1, k, max.possible.rank) {
    if (length(k1) == 0L || is.na(k1) || k1 == 0L) {
        return(list(
            step.models        = step.models,
            best.step.rank     = max.possible.rank,
            columns.on.left    = integer(0L),
            columns.on.right   = step.models$columns.order,
            population.on.left = 0L
        ))
    }
    list(
        step.models        = step.models,
        best.step.rank     = step.models$best_l1_by_k1[k1],
        columns.on.left    = step.models$columns.order[seq_len(k1)],
        columns.on.right   = step.models$columns.order[seq(k1 + 1L, k)],
        population.on.left = k1
    )
}
