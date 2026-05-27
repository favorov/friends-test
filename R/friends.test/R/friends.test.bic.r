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
#' friends.test.bic(A, prior.to.have.friends = 0.5)
#' friends.test.bic(A, prior.to.have.friends = 0.001)
#' @importFrom stats p.adjust
#' @importFrom purrr array_branch compact pmap
#' @importFrom cli cli_progress_step cli_progress_done
#' @export
#'
friends.test.bic <- function(A = NULL,
                             prior.to.have.friends = -1,
                             max.friends.n = "all",
                             .progress = FALSE,
                             BPPARAM = NULL) {
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

    if (.progress) options(cli.progress_show_after = 0)
    BPPARAM <- ft_bpparam(BPPARAM = BPPARAM, .progress = .progress)
    # rank all the A elements in columns
    if (.progress) cli::cli_progress_step("Ranking...")
    all_ranks <- friends.test::row.int.ranks(A)
    max.possible.rank <- dim(A)[1]
    all_rank_rows <- purrr::array_branch(all_ranks, 1)

    if (.progress) cli::cli_progress_step("Fitting the models...")
    #run ut all in purrr style
    #return: list of list of, trios
    #i, j, r -- vectors:
    #marker, friend, friend.rank
    col_names <- colnames(A)
    ijrlist <- ft_bpmapply_list(
        # local(envir=globalenv()): closure carries globalenv(), not the
        # friends.test namespace, so SnowParam workers can deserialize it
        # without needing friends.test installed; BPOPTIONS loads it first.
        local(
            \(ranks, i, max.friends.n, max.possible.rank, prior.to.have.friends, col_names) {
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
                #if we give just i to pmap, the value will be the same,
                #but we want the name of the friend ti be the name of
                #elemant of the inner list
                repi <- rep(i, length(friends))
                names(repi) <- col_names[friends]
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
            envir = globalenv()
        ),
        all_rank_rows,
        seq_len(nrow(A)),
        MoreArgs = list(
            max.friends.n = max.friends.n,
            max.possible.rank = max.possible.rank,
            prior.to.have.friends = prior.to.have.friends,
            col_names = col_names
        ),
        BPPARAM = BPPARAM
    )
    names(ijrlist) <- names(all_rank_rows)

    if (.progress) cli::cli_progress_step("Compacting...")
    ijrlist <- purrr::compact(ijrlist)
    if (.progress) cli::cli_progress_done()
    ijrlist

}
