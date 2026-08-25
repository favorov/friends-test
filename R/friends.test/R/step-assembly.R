# The part of a step fit that does not depend on how the winner is chosen.
# best_step_fit() and best_step_fit_bic() differ only in that choice; both
# search the same candidates and build the same return list.


# The best number of friends k1 among those with a non-empty valid l1 range,
# ties broken towards the larger split rank l1 -- the convention inherited
# from max(which(ln.likelihoods == max_ll)).
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
    list(
        k1 = tied_k1[which.max(step.models$best_l1_by_k1[tied_k1])],
        max.ln.l = max.ln.l
    )
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
