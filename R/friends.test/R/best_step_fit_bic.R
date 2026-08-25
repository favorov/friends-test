#'
#' best_step_fit_bic
#'
#' finds the ML-best step model for one row and
#' compares the posteriors of the found best step
#' and non-step uniform model.
#'
#' See [friends_test] documentation for details.
#'
#' @inheritParams step_fit_ln_likelihoods
#' @param prior.to.have.friends The prior for a row is important enough to
#' have friendly columns
#' @return a list of four values: \cr
#' \code{step.models} is the value return by [step_fit_ln_likelihoods]
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
#' example(row_int_ranks)
#' step <- best_step_fit_bic(TF.ranks[42, ], genes.no, 0.5)
#' nostep <- best_step_fit_bic(TF.ranks[42, ], genes.no, 1E-50)
#' @export
best_step_fit_bic <- function(ranks, max.possible.rank, prior.to.have.friends) {
    step.models <- .step_fit_compact(ranks, max.possible.rank)
    best <- .best_valid_k1(step.models)
    step_wins <- best$max.ln.l + log(prior.to.have.friends) >=
        step.models$uniform_ll + log(1 - prior.to.have.friends)
    .assemble_step(
        step.models,
        if (isTRUE(step_wins)) best$k1 else NA_integer_,
        length(ranks),
        max.possible.rank
    )
}
