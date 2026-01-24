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
#' @param max.friends.n The maximal number of friends for a marker.
#' A value $n$ means that we filter out a row if it has more
#' than $n$ friendly columns. 1 means we look only for unique (best) friends.
#' The string "all" (default) means the same as \code{ncols(A)} value,
#' do not filter markers by this parameter.
#' @param .progress the .progress is passed to \code{purrr} functions
#' The default is \code{.FALSE}.
#' If it is not, non-\code{purr} parts also shows progress.
#' @return \code{Matrix}, _e.g._ sparse matrix, the same size as A,
#' each nonzero element represent a marker+friend pair,
#' and the value shows the rank of the friend for the marker,
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
#' friends.test.bic(A, prior.to.have.friends = 0.5)
#' friends.test.bic(A, prior.to.have.friends = 0.001)
#' @importFrom stats p.adjust
#' @importFrom Matrix sparseMatrix
#' @export
#'
friends.test.bic <- function(A = NULL, prior.to.have.friends = -1,
                             max.friends.n = "all") {
    # parameter checks
    if (is.null(A) || (length(dim(A)) != 2))  {
        stop("The first parameter is to be a non-empty 2D matrix-like thing.")
    }

    if (is.na(max.friends.n) || max.friends.n == "all" ||
            max.friends.n == "al" || max.friends.n == "a" ||
            is.null(max.friends.n) || !as.logical(max.friends.n)
    ) {
        max.friends.n <- ncol(A)
    }
    if (max.friends.n < 1 || max.friends.n > ncol(A)) {
        stop("max.friends.n must be between 1 and the number of columns.")
    }
    if (prior.to.have.friends < 0 || prior.to.have.friends > 1) {
        stop("friends.test.bic requires the prior.to.have.friends
          value to be explicitely provided and to be a prior.")
    }
    # add names to A matrix rows if necessary
    if (is.null(dimnames(A)[[1]])) {
        rownames(A) <- seq_len(nrow(A))
    }
    # add names to A matrix cols if necessary
    if (is.null(dimnames(A)[[2]])) {
        colnames(A) <- seq_len(ncol(A))
    }
    all_ranks <- friends.test::row.int.ranks(A)
    max.possible.rank <- dim(A)[1]
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
    #run ut all in purrr style
    #return: list of dataframes,
    #trios i, j, rank -- rows of dataframe
    ijrlist <-
        all_ranks |>
        purrr::array_branch(1) |>
        #we have a list of nonuniform row ranks;
        #we are sure it is a list
        #we pass it to map2,
        #with .y as the row numbers in A
        purrr::map2(seq_len(nrow(A)), \(ranks, i) {
            step <- friends.test::best.step.fit.bic(
                ranks,
                max.possible.rank = max.possible.rank,
                prior.to.have.friends = prior.to.have.friends
            )
            frn <- length(step$columns.on.left)
            if (frn == 0 || frn > max.friends.n) {
                return(NULL)
                # here, we filer out the rows where
                # uniform model wins
                # (and so there is nothing to left of the step)
                # or
                # there are too much friends if we filter for it
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
