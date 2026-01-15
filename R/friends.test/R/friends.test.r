  #'
  #' friends.test
  #'
  #' We have two sets: T (rows) and C (columns) and
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
  #' @param max.friends.n The maximal number of friends for a marker,
  #' the default is \code{dim(A)[2]\%/\%2}, rounded half of #of columns in A.
  #' The string "all" means "all friends", do not filter by this parameter.
  #' A value $n$ means that we filter out a row if it has more
  #' than $n$ friendly columns. 1 means we look only for unique (best) friends.
  #' @param uniform.max The maximum of the uniform distribution of the ranks we
  #' fit the null model, it can be the maximal possible rank that is common for
  #' all rows and equals the number of rows \code{'c'} or the maximal observed
  #' rank for the row we test now, \code{'m'} (default).
  #' @param simulate.p.value K-S by Monte-Carlo if \code{TRUE};
  #' default is \code{FALSE}, see [stats::ks.test()]
  #' @param B number of or replicates if \code{simulate.p.value=TRUE}
  #' default is 2000, see [stats::ks.test()]
  #' @return \code{Matrix}, _e.g._ sparse matrix, the same size as A,
  #' each nonzero element represent a marker+friend pair,
  #' and the value shows the rank of the friend for the marker,
  #' from 1 (the best friend), then 2, 3, etc.
  #' If a row in A does not have friends,
  #' it is empty (zeroes-filled) in the result.
  #' @return \code{Matrix}, _e.g._ sparse matrix, the same size as A,
  #' each nonzero element shows the rank of the friend for the marker, 
  #' from 1 (the best friend), then 2, 3, etc.
  #' If a row in A does not have friends,
  #' it is empty (zeroes-filled) in the result.
  #' @importFrom stats p.adjust
  #' @importFrom Matrix sparseMatrix 
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
        stop("max.friends.n must be between 1 and the number of columns.")
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
        rownames(A) <- seq_len(nrow(A))
    }
    #add names to A matrix cols if necessary
    if (is.null(dimnames(A)[[2]])) {
        colnames(A) <- seq_len(ncol(A))
    }

    #prepare the return sparse matrix
    result <- sparseMatrix(
      i = integer(0),
      j = integer(0),
      x = numeric(0),
      repr = "T",
      dims = dim(A),
      dimnames = list(marker = rownames(A),
          friend = colnames(A))
    )
    #rank all the A elements in columns
    all_ranks <- friends.test::row.int.ranks(A)

    #calculate the p-values for null hypothesis for all the rows

    adj_nunif_pval <-
      p.adjust(apply(all_ranks, 1,
                    friends.test::unif.ks.test, uniform.max = uniform.max,
                    simulate.p.value = FALSE,
                    B = 2000),
              method = p.adjust.method)


    is_marker <- (adj_nunif_pval <= threshold)
    #is it a marker?

    if (sum(is_marker) == 0) {
      message("No rows with non-uniform ranks found for given threshold.")
      return(result)
      #empty matrix return
    }

    marker_indices <- which(is_marker)

    #find friends that make in-marker ranks non-uniform
    max.possible.rank <- dim(A)[1]
    print("*")
    #let's fill the result, calling the friends.test::best.step.fit
    #for all the marker_indices
    for(marker in marker_indices){
        best.fit <- friends.test::best.step.fit(
          all_ranks[marker,],
          max.possible.rank = max.possible.rank
        )
        if(length(best.fit$columns.on.left) <= max.friends.n){
            next #marker has too much friends 
        }
        #friends
        friends <- best.fit$columns.on.left
        #the ranks of friends, the best is 1
        friend.ranks <- which(
              best.fit$step.models$columns.order %in% friends
            )
        #cbind makes pairs (marker, friend) in rows
        #then, the friend's rank is written to the matrix
        result[cbind(marker, friends)] <- friend.ranks
    }
    resulte <<- result #debug, to see

    marker_ranks <<- all_ranks[is_marker, , drop = FALSE]
    #subset all_ranks to markers only

    #we make a list of fit structures (returned by best.step.fit)
    #per marker (marker row)
    best.fits.for.markers <-
      apply(
        marker_ranks, 1,
        function(x) {
          friends.test::best.step.fit(x, max.possible.rank = max.possible.rank)
        }
      )

    #let's fill the result


    #we filter to match
    #max.friends.n parameter here,
    #no more than max.friends.n columns
    #vapply is recommended by BioCheck as safer than sapply

    filter_for_markers <-
      vapply(best.fits.for.markers, function(x) {
        length(x$columns.on.left) <= max.friends.n
      }, logical(1))

    if (!length(best.fits.for.markers)) {
      return(result) 
    } #if no row passed best test, return empty result


    best.fits.for.markers <-
      best.fits.for.markers[filter_for_markers]

    marker_indices <-
      marker_indices[filter_for_markers]

    #filter marker ids and best fits together

    #cycle though the best fits and ids

    res_pre <-
      lapply(
        seq_len(length(best.fits.for.markers)),
        function(n) {
          x <- best.fits.for.markers[[n]]
          data.frame(
            marker = names(best.fits.for.markers)[n],
            friend = colnames(marker_ranks)[x$columns.on.left],
            marker.index = marker_indices[n],
            friend.index = x$columns.on.left,
            friend.rank = which(
              x$step.models$columns.order %in% x$columns.on.left
            ),
            row.names = NULL
          )
        }
      )

    res <- do.call(rbind, res_pre)

    res
  }
