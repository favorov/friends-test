#'
#' friends.test.bic
#'
#' We have two sets:T (rows) and C (columns) and
#' A real matrix A(t,c) that describes the strength of association
#' between each t and each c; t is an element of T and c is an element of C.
#' For each t we want to identify whether it is significantly more
#' relevant for some c's than for the remaining c's.
#' If it does, those c for which the t is relevant,
#' are the t's friend. And, the t is the c's marker.
#'
#' @param A original association matrix
#' @param prior.to.have.friends The prior for a row to have friendly columns.
#' @param max.friends.n The maximal number of friends for a marker, the default
#' is \code{dim(A)[2]\%/\%2}, that is rounded half of #of columns in A.
#' The string "all" means "all friends", i.e. we do not filter by this parameter
#' value. A value $n$ means that we filter out a row if it has more
#' than $n$ friendly columns. 1 means we look only for unuque (best) friends.
#' @return A data.frame, rows are pairs of markers and friends,
#' columns are: marker, friend and friend.rank. The latter is
#' the rank of the column-friend in the vector of ranks of the
#' ranks of the row-marker in different columns.
#' @examples
#' A <- matrix(c(10,6,7,8,9,
#'                 9,10,6,7,8,
#'                 8,9,10,6,7,
#'                 7,8,9,10,6,
#'                 6,7,8,9,10,
#'                 20,0,0,0,0),
#'                 nrow=6, ncol=5, byrow=TRUE)
#' A
#' friends.test.bic(A, prior.to.have.friends=0.5)
#' friends.test.bic(A, prior.to.have.friends=0.001)
#' @export
#'
friends.test.bic <- function(A = NULL, prior.to.have.friends = -1,
                             max.friends.n = dim(A)[2] %/% 2) {
  #parameter checks
  #parameter checks
  if (is.na(max.friends.n) || max.friends.n == "all" ||
        max.friends.n == "al" || max.friends.n == "a" ||
        is.null(max.friends.n) || !as.logical(max.friends.n)) {
    max.friends.n <- ncol(A)
  }
  if (max.friends.n < 1 || max.friends.n > ncol(A)) {
    stop("max.friends.n must be between 1 and the number of collections.")
  }
  if (prior.to.have.friends < 0 || prior.to.have.friends > 1) {
    stop("friends.test.bic requires the prior.to.have.friends
          value to be explicitely provided and to be a prior.")
  }
  #add names to A matrix rows if necessary
  if (is.null(dimnames(A)[[1]])) {
    rownames(A) <- seq_len(nrow(A))
  }
  #add names to A matrix cols if necessary
  if (is.null(dimnames(A)[[2]])) {
    colnames(A) <- seq_len(ncol(A))
  }
  all_ranks <- tag.int.ranks(A)

  max.possible.rank <- dim(A)[1]

  best.fits.for.rows <-
    apply(
      all_ranks, 1,
      function(x) {
        best.step.fit.bic(
          x,
          max.possible.rank = max.possible.rank,
          prior.to.have.friends = prior.to.have.friends
        )
      }
    )

  #here, we filer out the rows where uniform model wins for
  #we also filter to match
  #max.friends.n parameter here,
  #friends.test are cases where a tag is a marker in
  #no more than max.friends.n collections

  #vapply is recommended by BioCheck as safer than sapply
  filter.for.markers <- vapply(
    best.fits.for.rows,
    function(x) {
      x$population.on.left > 0 && x$population.on.left <= max.friends.n
    },
    logical(1)
  )

  if (!sum(filter.for.markers)) {
    return(
      data.frame(
        marker = character(),
        friend = character(),
        marker.index = integer(),
        friend.index = integer(),
        friend.rank = integer()
      )
    )
  } #if no row passed best test, return empty frame rather than NULL

  best.fits.for.markers <- best.fits.for.rows[filter.for.markers]
  marker_indices <- which(filter.for.markers)

  res_pre <-
    lapply(
      seq_len(length(best.fits.for.markers)),
      function(n) {
        x <- best.fits.for.markers[[n]]
        data.frame(
          marker = names(best.fits.for.markers)[n],
          friend = colnames(all_ranks)[x$collections.on.left],
          marker.index = marker_indices[n],
          friend.index = x$collections.on.left,
          friend.rank = which(
            x$step.models$collections.order %in% x$collections.on.left
          ),
          row.names = NULL
        )
      }
    )



  res <- do.call(rbind, res_pre)

  res
}
