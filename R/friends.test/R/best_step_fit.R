#'
#' best_step_fit
#'
#' finds the ML-best step model for one row
#'
#' See [friends_test] documentation for details.
#'
#' @inheritParams step_fit_ln_likelihoods
#' @param uniform.null how the smallest and the largest possible rank are
#' chosen. \code{"observed"} (the default) takes them from the row itself,
#' \code{min(ranks)} and \code{max(ranks)}, which is the scale
#' [unif_ks_test] tests uniformity on by default, so that the two stages of
#' [friends_test_ks] agree. \code{"continuity"} keeps the whole scale,
#' \eqn{1 \ldots N}. \code{max.possible.rank} is not used under
#' \code{"observed"}, and \code{best.step.rank} always comes back on the
#' scale of the ranks given.
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
#' whole.scale <- best_step_fit(
#'     TF.ranks[42, ], genes.no,
#'     uniform.null = "continuity"
#' )
#' @export
best_step_fit <- function(
    ranks,
    max.possible.rank,
    uniform.null = c("observed", "continuity")
) {
    uniform.null <- match.arg(uniform.null)
    scale <- .rank_scale(ranks, max.possible.rank, uniform.null)
    step.models <- .step_fit_compact(scale$ranks, scale$size)
    best <- .best_valid_k1(step.models)
    # maximum likelihood: the best valid step always wins, and only a fully
    # tied row leaves no valid step at all
    fit <- .assemble_step(step.models, best$k1, length(ranks), scale$size)
    fit$best.step.rank <- fit$best.step.rank + scale$offset
    fit
}
