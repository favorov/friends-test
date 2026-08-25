#'
#' best_step_fit
#'
#' finds the ML-best step model for one row
#'
#' See [friends_test] documentation for details.
#'
#' @inheritParams step_fit_ln_likelihoods
#' @return a list of four values: \cr
#' \code{step.models} is return from [step_fit_ln_likelihoods] call,
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
#' example(row_int_ranks)
#' step <- best_step_fit(TF.ranks[42, ], genes.no)
#' @export
best_step_fit <- function(ranks, max.possible.rank) {
    step.models <- .step_fit_compact(ranks, max.possible.rank)
    best <- .best_valid_k1(step.models)
    # maximum likelihood: the best valid step always wins, and only a fully
    # tied row leaves no valid step at all
    .assemble_step(step.models, best$k1, length(ranks), max.possible.rank)
}
