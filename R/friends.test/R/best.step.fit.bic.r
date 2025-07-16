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
#' step<-best.step.fit.bic(TF.ranks[42,],genes.no,0.5)
#' nostep<-best.step.fit.bic(TF.ranks[42,],genes.no,1E-50)
#' @export
best.step.fit.bic <- function(ranks, max.possible.rank, prior.to.have.friends) {
  step.models <- friends.test::step.fit.ln.likelihoods(ranks, max.possible.rank)

  possible.step.ranks <- seq_len(max.possible.rank - 1)
  k1.by.l1 <- step.models$k1.by.l1[possible.step.ranks]
  possible.step.ranks <- possible.step.ranks[
    k1.by.l1 > 0 & k1.by.l1 < length(ranks)
  ]
  #we assess only the steps that have nonzero left and right sets

  possible.ln.likelihoods <-
    step.models$ln.likelihoods[possible.step.ranks]
  max.ln.l <- max(possible.ln.likelihoods)
  #here, we compare the best ln posterior
  #with the uniform model posterior,
  #and here we use the prior.to.have.friends
  if (max.ln.l + log(prior.to.have.friends) >=
        step.models$ln.likelihoods[max.possible.rank] +
          log(1 - prior.to.have.friends)) {
    #if we are here, the step model won
    best.step.index <- max(which(possible.ln.likelihoods == max.ln.l))
    #the index in possible.step_ranks (and possible.ln.likelihoods);
    #we need the rank itself
    best.step.rank <- possible.step.ranks[best.step.index]

    population.on.left <- k1.by.l1[best.step.rank]

    columns.on.left <-
      step.models$columns.order[seq_len(population.on.left)]

    columns.on.right <-
      step.models$columns.order[seq(population.on.left + 1,
                                    length(ranks))]
  } else {
    #if we are here, the uniform won, no friends

    best.step.rank <- max.possible.rank

    population.on.left <- 0 #all

    columns.on.left <- c() #empty -- no friends

    columns.on.right <- step.models$columns.order
    #all
  }

  list(step.models = step.models,
    best.step.rank = best.step.rank,
    columns.on.left = columns.on.left,
    columns.on.right = columns.on.right,
    population.on.left = population.on.left
  )
}