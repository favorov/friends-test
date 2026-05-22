#'
#' best.step.fit.bic
#'
#' finds the ML-best step model for one row and
#' compares the posteriors of the found best step
#' and non-step uniform model.
#'
#' See [friends.test] documentation for details.
#'
#' @inheritParams step.fit.ln.likelihoods
#' @param prior.to.have.friends The prior for a row is important enough to
#' have friendly columns
#' @return a list of four values: \cr
#' \code{step.models} is the value return by [step.fit.ln.likelihoods]
#' call the function start with
#' \code{best.step.rank} is the rank value that makes the best step;
#' it is not obligatory one on the \code{ranks} value.\cr
#' \code{columns.on.left} is the vector of the columns on the
#' left of the best step (including the step value).
#' They are friends of the row, and the row is thir marker.\cr
#' \code{columns.on.right} is vector of those on the right \cr
#' \code{population.on.left} is how many (column) ranks are on left of split;
#' they are friends! \cr
#' if non-step uniform model wins and there are no friends,\cr
#' then \code{best.step.rank==max.possible.rank},
#' \code{population.on.left==0},
#' all columns are listed in \code{columns.on.right} and
#' \code{columns.on.left} is empty.
#' @examples
#' example(row.int.ranks)
#' step <- best.step.fit.bic(TF.ranks[42, ], genes.no, 0.5)
#' nostep <- best.step.fit.bic(TF.ranks[42, ], genes.no, 1E-50)
#' @export
best.step.fit.bic <- function(ranks, max.possible.rank, prior.to.have.friends) {
    step.models <- friends.test:::.step_fit_compact(ranks, max.possible.rank)
    k <- length(ranks)

    valid_k1 <- which(is.finite(step.models$best_ll_by_k1))
    max.ln.l <- if (length(valid_k1) > 0L) {
        max(step.models$best_ll_by_k1[valid_k1])
    } else {
        -Inf
    }

    if (max.ln.l + log(prior.to.have.friends) >=
            step.models$uniform_ll + log(1 - prior.to.have.friends)) {
        # Step model wins
        tied_k1  <- valid_k1[step.models$best_ll_by_k1[valid_k1] == max.ln.l]
        best_k1  <- tied_k1[which.max(step.models$best_l1_by_k1[tied_k1])]
        best.step.rank     <- step.models$best_l1_by_k1[best_k1]
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
    } else {
        # Uniform model wins: no friends
        list(
            step.models        = step.models,
            best.step.rank     = max.possible.rank,
            columns.on.left    = integer(0L),
            columns.on.right   = step.models$columns.order,
            population.on.left = 0L
        )
    }
}
