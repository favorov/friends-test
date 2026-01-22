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
#' @param max.friends.n The maximal number of friends for a marker.
#' A value $n$ means that we filter out a row if it has more
#' than $n$ friendly columns. 1 means we look only for unique (best) friends.
#' The string "all" (default) means the same as \code{ncols(A)} value,
#' do not filter markers by this parameter.
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
#' @examples
#' A <- matrix(
#'     c(
#'         10, 6, 7, 8, 9,
#'         9, 10, 6, 7, 8,
#'         8, 9, 10, 6, 7,
#'         7, 8, 9, 10, 6,
#'         6, 7, 8, 9, 10,
#'         20, 0, 0, 0, 0
#'     ),
#'     nrow = 6, ncol = 5, byrow = TRUE
#' )
#' A
#' friends.test(A, threshold = .05)
#' friends.test(A, threshold = .0001)
#' friends.test(A, threshold = .05, uniform.max = "m")
#' friends.test(A, threshold = .0001, uniform.max = "m")
#'
#' @importFrom stats p.adjust
#' @importFrom Matrix sparseMatrix
#' @importFrom purrr array_branch map map_dbl map2
#' @export
#'
friends.test <- function(A = NULL, threshold = 0.05,
                         p.adjust.method = "BH",
                         max.friends.n = "all",
                         uniform.max = "m",
                         simulate.p.value = FALSE,
                         B = 2000) {
    # parameter checks
    if (is.na(max.friends.n) || max.friends.n == "all" ||
            max.friends.n == "al" || max.friends.n == "a" ||
            is.null(max.friends.n) || !as.logical(max.friends.n)
    ) {
        max.friends.n <- ncol(A)
    }
    if (max.friends.n < 1 || max.friends.n > ncol(A)) {
        stop("max.friends.n must be between 1 and the number of columns.")
    }
    if (threshold < 0 || threshold > 1) {
        stop("threshold must be between 0 and 1.")
    }
    # case for uniform.max: M or m assign nrow(A) (max rank),
    # for C or c assign NA, any other fails
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

    # add names to A matrix rows if necessary
    if (is.null(dimnames(A)[[1]])) {
        rownames(A) <- seq_len(nrow(A))
    }
    # add names to A matrix cols if necessary
    if (is.null(dimnames(A)[[2]])) {
        colnames(A) <- seq_len(ncol(A))
    }

    # prepare the return sparse matrix
    result <- Matrix::sparseMatrix(
        i = integer(0),
        j = integer(0),
        x = numeric(0),
        repr = "T",
        dims = dim(A),
        dimnames = list(
            marker = rownames(A),
            friend = colnames(A)
        )
    )
    # rank all the A elements in columns
    all_ranks <- friends.test::row.int.ranks(A)

    # calculate the p-values for null hypothesis for all the rank rows
    # pipeline : array tp list, list to double vector of p-values
    # adjust p-value
    adj_nunif_pval <-
        all_ranks |>
        #convert the array to list of rows
        purrr::array_branch(1) |>
        #apply friends.test::unif.ks.test to each
        purrr::map_dbl(\(ranks) {
            friends.test::unif.ks.test(
                ranks,
                uniform.max = uniform.max,
                simulate.p.value = FALSE,
                B = 2000
            )
        }) |>
        p.adjust(
            method = p.adjust.method
        )


    is_marker <- (adj_nunif_pval <= threshold)
    # is it a marker?

    if (sum(is_marker) == 0) {
        message("No rows with non-uniform ranks found for given threshold.")
        return(result)
        # empty matrix return
    }

    marker_indices <- which(is_marker)

    # find friends that make in-marker ranks non-uniform
    max.possible.rank <- dim(A)[1]

    #run ut all in purrr style
    #return: list of dataframes,
    #trios i, j, rank -- rows of dataframe
    ijrlist <-
        all_ranks[marker_indices, , drop = FALSE] |>
        purrr::array_branch(1) |>
        #we have a list of nonuniform row ranks;
        #we are sure it is a list
        #we pass it to map2,
        #with .y as the row numbers in A
        purrr::map2(which(is_marker), \(ranks, i) {
            step <- friends.test::best.step.fit(
                ranks,
                max.possible.rank = max.possible.rank
            )
            if (length(step$columns.on.left) > max.friends.n) {
                return(NULL) # marker has too much friends
            }
            # friends
            friends <- step$columns.on.left
            # the ranks of friends, the best is 1
            friend.ranks <- which(
                step$step.models$columns.order %in% friends
            )
            data.frame(
                i = i,
                j = friends,
                r = friend.ranks
            )
        })

    #now, we put all trios to the result
    #we did the list of trios as an intermediate
    #because it can be prepard in parallel, reduce is here
    for (ind in seq_along(ijrlist)) {
        if (is.null(ijrlist[[ind]])) next
        marker <- ijrlist[[ind]][, 1]
        friends <- ijrlist[[ind]][, 2]
        friend.ranks <- ijrlist[[ind]][, 3]
        result[cbind(marker, friends)] <- friend.ranks
    }

    result
}
