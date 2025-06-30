#'
#' friends.test
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
#' @param threshold The adjusted p-value threshold for KS test for
#' non-uniformity of ranks.
#' @param p.adjust.method Multiple testing correction method,
#' see \link[stats]{p.adjust}.
#' @param max.friends.n The maximal number of friends for a marker, the default
#' is \code{dim(A)[2]\%/\%2}, that is rounded half of #of columns in A.
#' The string "all" means "all friends", i.e. we do not filter by this parameter
#' value. A value $n$ means that we filter out a row if it has more
#' than $n$ friendly columns. 1 means we look only for unuque (best) friends.
#' @param uniform.max The maximum of the uniform distribution of the ranks we
#' fit the null model, it can be the maximal possible rank that is common for
#' all rows and equals the number of rows \code{'c'} or the maximal observed
#' rank for the row we test now, \code{'m'} (default).
#' @param simulate.p.value K-S by Monte-Carlo if \code{TRUE};
#' default is \code{FALSE}, see [stats::ks.test()]
#' @param B number of or replicates if \code{simulate.p.value=TRUE}
#' default is 2000, see [stats::ks.test()]
#' @return A data.frame, rows are pairs of markers and friends,
#' columns are: marker, friend and friend.rank. The latter is
#' the rank of the column-friend in the vector of ranks of the
#' ranks of the row-marker in different columns.
#' @importFrom stats p.adjust
#' @examples
#' A <- matrix(c(10,6,7,8,9,
#'                 9,10,6,7,8,
#'                 8,9,10,6,7,
#'                 7,8,9,10,6,
#'                 6,7,8,9,10,
#'                 20,0,0,0,0),
#'                 nrow=6, ncol=5, byrow=TRUE)
#' A
#' friends.test(A, threshold = .05)
#' friends.test(A, threshold = .0001)
#' friends.test(A, threshold = .05, uniform.max='m')
#' friends.test(A, threshold = .0001, uniform.max='m')
#'
#' @export
#'
friends.test <- function(A = NULL, threshold = 0.05,
                         p.adjust.method = "BH",
                         max.friends.n = dim(A)[2] %/% 2,
                         uniform.max = "m",
                         simulate.p.value = FALSE,
                         B = 2000) {
  # parameter checks
  if (is.na(max.friends.n) || max.friends.n == "all" ||
        max.friends.n == "al" || max.friends.n == "a" ||
        is.null(max.friends.n) || !as.logical(max.friends.n)) {
    max.friends.n <- ncol(A)
  }
  if (max.friends.n < 1 || max.friends.n > ncol(A)) {
    stop("max.friends.n must be between 1 and the number of collections.")
  }
  if (threshold < 0 || threshold > 1) {
    stop("threshold must be between 0 and 1.")
  }
  #case for uniform.max: M or m assign nrow(A) (max rank),
  #for C or c assign NA, any other fails
  if (uniform.max == "m" || uniform.max == "M") {
    uniform.max <- NA
  } else if (uniform.max == "c" || uniform.max == "C") {
    uniform.max <- nrow(A)
  } else if (!is.numeric(uniform.max)) {
    stop("uniform.max must be either 'm', 'M', 'c', 'C' or numeric.")
  }

  if (is.null(A) || !is.matrix(A)) {
    stop("A must be a non-empty matrix.")
  }

  #add names to A matrix rows if necessary
  if (is.null(dimnames(A)[[1]])) {
    rownames(A) <- seq(nrow(A))
  }
  #add names to A matrix cols if necessary
  if (is.null(dimnames(A)[[2]])) {
    colnames(A) <- seq(ncol(A))
  }

  #rank all the A elements in columns
  all_ranks <- tag.int.ranks(A)

  #calculate the p-values for null hypothesis for all the rows

  adj_nunif_pval <-
    p.adjust(apply(all_ranks, 1,
                   unif.ks.test, uniform.max = uniform.max,
                   simulate.p.value = FALSE,
                   B = 2000),
             method = p.adjust.method)

  marker_ranks <- all_ranks[adj_nunif_pval <= threshold, , drop = FALSE]

  if (nrow(marker_ranks) == 0) {
    message("No tags with non-uniform ranks found for given threshold.")
    return(data.frame(tag = character(),
                      collection = character(),
                      friend.rank = integer()))
  }


  #find friends that make tag ranks non-uniform
  max.possible.rank <- dim(A)[1]

  #we make a list of fit structures (returned by best.step.fit)
  #per marker (marker row)
  best.fits.for.markers <-
    apply(
      marker_ranks, 1,
      function(x) best.step.fit(x, max.possible.rank = max.possible.rank)
    )
  #we filter to match
  #max.friends.n parameter here,
  #no more than max.friends.n collections
  #vapply is recommended by BioCheck as safer than sapply

  best.fits.for.markers <- best.fits.for.markers[vapply(best.fits.for.markers, function(x) {
    x$population.on.left <= max.friends.n
  },logical(1))]

 
  if(!length(best.fits.for.markers)){
    return(data.frame(tag=character(),
            collection=character(),
            friend.rank=integer()))
  } #if no tag passed best test, return empty frame rather than NULL

  res_pre <- lapply(seq_along(best.fits.for.markers),
      function(x) {
          collections.on.left<-
            best.fits.for.markers[[x]]$collections.on.left
          data.frame(
            marker=names(best.fits.for.markers)[x],
            friend=colnames(marker_ranks)[collections.on.left],
            friend.rank=which(
              best.fits.for.markers[[x]]$step.models$collectons.order %in%
              collections.on.left
            )
          )
        })

  res <- do.call(rbind, res_pre)

  res
}
