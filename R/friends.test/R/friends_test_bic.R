#'
#' friends_test_bic
#'
#' We have two sets:T (rows) and C (columns) and
#' A real matrix A(t,c) that describes the strength of association
#' between each t and each c; t is an element of T and c is an element of C.
#' For each t we want to identify whether it is significantly more
#' relevant for some c's than for the remaining c's.
#' If it does, those c for which the t is relevant,
#' are the t's friend. And, the t is the c's marker.
#'
#' If you want to run the row-wise calculations in parallel,
#' pass a [BiocParallel::BiocParallelParam-class] object via \code{BPPARAM},
#' for instance \code{BiocParallel::MulticoreParam(workers = 4)} on Unix-like
#' systems or \code{BiocParallel::SnowParam(workers = 4)} on all platforms.
#'
#' @param A original association matrix
#' @param prior.to.have.friends The prior for a row to have friendly columns.
#' @param max.friends.n The maximal number of friends for a marker.
#' A value $n$ means that we filter out a row if it has more
#' than $n$ friendly columns. 1 means we look only for unique (best) friends.
#' The string "all" (default) means the same as \code{ncols(A)} value,
#' do not filter markers by this parameter.
#' @param .progress if \code{TRUE}, show simple progress messages and enable
#' the text progress bar of the selected \code{BPPARAM}. The default is
#' \code{FALSE}.
#' @param BPPARAM a [BiocParallel::BiocParallelParam-class] instance that
#' controls whether the row-wise work is run serially or in parallel. The
#' default is \code{BiocParallel::SerialParam()}.
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
#' friends_test_bic(A, prior.to.have.friends = 0.5)
#' friends_test_bic(A, prior.to.have.friends = 0.001)
#' @importFrom stats p.adjust
#' @importFrom purrr array_branch compact pmap
#' @importFrom cli cli_progress_step cli_progress_done cli_progress_along
#' @importFrom methods is
#' @export
#'
friends_test_bic <- function(
    A = NULL,
    prior.to.have.friends = -1,
    max.friends.n = "all",
    .progress = FALSE,
    BPPARAM = NULL
) {
    if (prior.to.have.friends < 0 || prior.to.have.friends > 1) {
        stop(
            "friends_test_bic requires the prior.to.have.friends value ",
            "to be explicitly provided and to be a prior."
        )
    }

    prep <- .ft_prepare(A, max.friends.n, .progress, BPPARAM)
    A <- prep$A
    max.friends.n <- prep$max.friends.n
    BPPARAM <- prep$BPPARAM
    all_rank_rows <- prep$rows
    max.possible.rank <- nrow(A)

    #run ut all in purrr style
    #return: list of list of, trios
    #i, j, r -- vectors:
    #marker, friend, friend.rank
    col_names <- colnames(A)
    # return: list of lists of trios -- marker, friend, friend.rank
    ijrlist <- .ft_map_rows(
        function(
            row, i, max.friends.n, max.possible.rank,
            prior.to.have.friends, col_names
        ) {
            step <- friends.test::best_step_fit_bic(
                row,
                max.possible.rank = max.possible.rank,
                prior.to.have.friends = prior.to.have.friends
            )
            frn <- length(step$columns.on.left)
            if (frn == 0 || frn > max.friends.n) {
                # either the uniform model won, so there is nothing to the left
                # of the step, or the marker has too many friends
                return(NULL)
            }
            friends <- step$columns.on.left
            # the ranks of the friends, the best is 1
            friend.ranks <- which(step$step.models$columns.order %in% friends)
            # pmap over a repeated i so that each inner element is named after
            # its friend
            repi <- rep(i, length(friends))
            names(repi) <- col_names[friends]
            purrr::pmap(
                list(marker = repi, friend = friends, rank = friend.ranks),
                c
            )
        },
        rows = all_rank_rows,
        idx = seq_len(nrow(A)),
        MoreArgs = list(
            max.friends.n = max.friends.n,
            max.possible.rank = max.possible.rank,
            prior.to.have.friends = prior.to.have.friends,
            col_names = col_names
        ),
        BPPARAM = BPPARAM,
        .progress = .progress,
        label = "Fitting the models"
    )
    names(ijrlist) <- names(all_rank_rows)

    if (.progress) cli::cli_progress_step("Compacting...")
    ijrlist <- purrr::compact(ijrlist)
    if (.progress) cli::cli_progress_done()
    ijrlist

}
