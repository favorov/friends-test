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
#' If you want to run the main function cycles in parallel,
#' say \code{mirai::daemons(proc#)}, the \code{proc#} is a
#' natural number of processors.
#' Do not forget to switch the parallel mode off:
#' \code{mirai::daemons(proc#)}
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
#' default is \code{FALSE}, see [stats::ks.test()].
#' @param B number of or replicates if \code{simulate.p.value=TRUE}
#' default is 2000, see [stats::ks.test()].
#' @param .progress the .progress is passed to \code{purrr} functions
#' The default is \code{.FALSE}.
#' If it is not, non-\code{purr} parts also shows progress.
#' @return \code{list}; each element represents a marker, *e.g.*,
#' a matrix row that has friend(s). Each element of the return list
#' is also a list, one element per friend, and the 2-nd level element
#' is an integer vector with three numbers, that are:
#' the marker coordinate (\code{marker}),
#' the friend coordinate (\code{friend}), and
#' the the rank of the friend for the marker (\code{rank}).
#' So, it is list of lists of simple integer vectors, each
#' vector represents a marker+friend pair,
#' the inner lists enumerate friends,
#' the outer (return) list enumerate markers.
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
#' @importFrom purrr array_branch map_dbl map2 compact pmap
#' @importFrom cli cli_progress_step cli_progress_done
#' @importFrom mirai status everywhere
#' @export
#'
friends.test <- function(A = NULL, threshold = 0.05,
                         p.adjust.method = "BH",
                         max.friends.n = "all",
                         uniform.max = "m",
                         simulate.p.value = FALSE,
                         B = 2000,
                         .progress = FALSE) {
    # parameter checks
    if (is.null(A) || (length(dim(A)) != 2))  {
        stop("The first parameter is to be a non-empty 2D matrix-like thing.")
    }

    if (is.null(max.friends.n) || is.na(max.friends.n) ||
            max.friends.n == "all" || max.friends.n == "al" ||
            max.friends.n == "a") {
        max.friends.n <- ncol(A)
    } else if (!is.numeric(max.friends.n)) {
        stop(paste(
            "max.friends.n must be numeric,",
            " or one of 'all', 'al', 'a', NA, or NULL."
        ))
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


    # add names to A matrix rows if necessary
    if (is.null(dimnames(A)[[1]])) {
        rownames(A) <- seq_len(nrow(A))
    }
    # add names to A matrix cols if necessary
    if (is.null(dimnames(A)[[2]])) {
        colnames(A) <- seq_len(ncol(A))
    }

    if (.progress) options(cli.progress_show_after = 0)

    # rank all the A elements in columns
    if (.progress) cli::cli_progress_step("Ranking...")
    all_ranks <- friends.test::row.int.ranks(A)

    # calculate the p-values for null hypothesis for all the rank rows
    # pipeline : array tp list, list to double vector of p-values
    # adjust p-value
    the.progress <- .progress
    if (.progress) {
        cli::cli_progress_step("Filtering out uniforms...")
        the.progress <- list(name = "Filtering out uniforms...")
    }

    adj_nunif_pval <-
        all_ranks |>
        #convert the array to list of rows
        purrr::array_branch(1) |>
        #apply friends.test::unif.ks.test to each
        purrr::map_dbl(purrr::in_parallel(\(ranks) {
            friends.test::unif.ks.test(
                ranks,
                uniform.max = uniform.max,
                simulate.p.value = FALSE,
                B = 2000
            )
        },
        #inside in_parallel, it knows nothing,
        #we are to pass it all via ...
        uniform.max = uniform.max
        ), .progress = the.progress) |>
        p.adjust(
            method = p.adjust.method
        )

    if (.progress) cli::cli_progress_done()

    is_marker <- (adj_nunif_pval <= threshold)
    # is it a marker?

    if (sum(is_marker) == 0) {
        message("No rows with non-uniform ranks found for given threshold.")
        return(list())
        # empty matrix return
    }

    marker_indices <- which(is_marker)

    # find friends that make in-marker ranks non-uniform
    max.possible.rank <- dim(A)[1]
    if (.progress) {
        cli::cli_progress_step("Identifying friends...")
        the.progress$name <- "Identifying friends..."
    }
    #run ut all in purrr style
    #return: list of list of, trios
    #i, j, r -- vectors:
    #marker, friend, friend.rank
    ijrlist <-
        all_ranks[marker_indices, , drop = FALSE] |>
        purrr::array_branch(1) |>
        #we have a list of nonuniform row ranks;
        #we are sure it is a list
        #we pass it to map2,
        #with .y as the row numbers in A
        purrr::map2(which(is_marker), purrr::in_parallel(\(ranks, i) {
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
            #if we give just i to pmap, the value will be the same,
            #but we want the name of the friend ti be the name of
            #elemant of the inner list
            repi <- rep(i, length(friends))
            names(repi) <- colnames(A)[friends]
            #list of vector trios
            purrr::pmap(
                list(
                    marker = repi,
                    friend = friends,
                    rank = friend.ranks
                ),
                c
            )
        },
        #inside in_parallel, it knows nothing,
        #we are to pass it all via ...
        max.possible.rank = max.possible.rank,
        max.friends.n = max.friends.n,
        A = A
        ),
        .progress = the.progress)

    if (.progress) cli::cli_progress_step("Compacting...")
    ijrlist <- purrr::compact(ijrlist)
    if (.progress) cli::cli_progress_done()
    ijrlist
}
