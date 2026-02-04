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
    step.models <-
        friends.test::step.fit.ln.likelihoods(ranks, max.possible.rank)

    possible.step.ranks <- seq_len(max.possible.rank - 1)
    k1.by.l1 <- step.models$k1.by.l1[possible.step.ranks]
    possible.step.ranks <-
        possible.step.ranks[k1.by.l1 > 0 & k1.by.l1 < length(ranks)]
    # we assess only the steps that have nonzero left and right sets

    possible.ln.likelihoods <-
        step.models$ln.likelihoods[possible.step.ranks]
    max.ln.l <- max(possible.ln.likelihoods)
    best.step.index <- max(which(possible.ln.likelihoods == max.ln.l))
    # the index in possible.step_ranks (and possible.ln.likelihoods);
    # we need the rank itself

    best.step.rank <- possible.step.ranks[best.step.index]

    population.on.left <- k1.by.l1[best.step.rank]

    columns.on.left <-
        step.models$columns.order[seq_len(population.on.left)]

    columns.on.right <-
        step.models$columns.order[seq(population.on.left + 1, length(ranks))]


    list(
        step.models = step.models,
        best.step.rank = best.step.rank,
        columns.on.left = columns.on.left,
        columns.on.right = columns.on.right,
        population.on.left = population.on.left
    )
}
