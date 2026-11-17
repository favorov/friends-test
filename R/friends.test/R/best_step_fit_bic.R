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
#' @param prior.to.have.friends The prior probability that a row has friendly
#' columns, a number in \eqn{[0, 1]}.  It has no default and has to be given.
#' @param uniform.null how the smallest and the largest possible rank are
#' chosen. \code{"observed"} (the default) takes them from the row itself,
#' \code{min(ranks)} and \code{max(ranks)}, which makes the comparison
#' invariant to where the rank profile sits and how wide it is; this is the
#' convention [friends_test_ks] uses by default. \code{"continuity"} keeps the
#' whole scale, \eqn{1 \ldots N}, so that concentration into part of it counts
#' as evidence for a step. \code{max.possible.rank} is not used under
#' \code{"observed"}. The \code{"randomized"} setting of [unif_ks_test] has no
#' counterpart here, because the step model is already discrete and has
#' nothing to randomise.
#' @return a list of five values: \cr
#' \code{step.models} is the value returned by
#' [step_fit_ln_likelihoods], the call this function starts with\cr
#' \code{best.step.rank} is the rank value that makes the best step; it is a
#' rank on the scale of \code{ranks} and need not be one of the ranks the row
#' actually takes.\cr
#' \code{columns.on.left} is the vector of the columns on the
#' left of the best step (including the step value).
#' They are friends of the row, and the row is their marker.\cr
#' \code{columns.on.right} is vector of those on the right \cr
#' \code{population.on.left} is how many (column) ranks are on left of split;
#' they are friends! \cr
#' if non-step uniform model wins and there are no friends,\cr
#' then \code{best.step.rank} is the largest rank the row could take under the
#' chosen \code{uniform.null}, that is \code{max(ranks)} for \code{"observed"}
#' and \code{max.possible.rank} for \code{"continuity"},
#' \code{population.on.left==0},
#' all columns are listed in \code{columns.on.right} and
#' \code{columns.on.left} is empty.
#' @examples
#' example(row_int_ranks)
#' step <- best_step_fit_bic(TF.ranks[42, ], genes.no, 0.5)
#' nostep <- best_step_fit_bic(TF.ranks[42, ], genes.no, 1E-50)
#' whole.scale <- best_step_fit_bic(
#'     TF.ranks[42, ], genes.no, 0.5,
#'     uniform.null = "continuity"
#' )
#' @export
best_step_fit_bic <- function(
    ranks,
    max.possible.rank,
    prior.to.have.friends,
    uniform.null = c("observed", "continuity")
) {
    .check_prior(prior.to.have.friends)
    uniform.null <- match.arg(uniform.null)
    scale <- .rank_scale(ranks, max.possible.rank, uniform.null)
    step.models <- .step_fit_compact(scale$ranks, scale$size)
    best <- .best_valid_k1(step.models)
    # the step model has two free parameters the uniform one has not, and this
    # comparison carries no penalty for them, so an exact tie goes to the
    # uniform model -- the only place parsimony can enter
    step_wins <- best$max.ln.l + log(prior.to.have.friends) >
        step.models$uniform_ll + log(1 - prior.to.have.friends)
    fit <- .assemble_step(
        step.models,
        if (isTRUE(step_wins)) best$k1 else NA_integer_,
        length(ranks),
        scale$size
    )
    fit$best.step.rank <- fit$best.step.rank + scale$offset
    fit
}
