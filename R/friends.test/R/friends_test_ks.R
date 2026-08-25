#'
#' friends_test_ks
#'
#' The Kolmogorov-Smirnov branch of the friends test. For every row it tests
#' the null hypothesis that the row's ranks are uniformly distributed over the
#' columns; a row that rejects it is a marker, and the columns to the left of
#' the best step are its friends.
#'
#' See [friends_test] for the method itself and for the Bayesian alternative
#' [friends_test_bic].
#'
#' If you want to run the row-wise calculations in parallel,
#' pass a [BiocParallel::BiocParallelParam-class] object via \code{BPPARAM},
#' for instance \code{BiocParallel::MulticoreParam(workers = 4)} on Unix-like
#' systems or \code{BiocParallel::SnowParam(workers = 4)} on all platforms.
#'
#' @param A original association matrix
#' @param threshold The adjusted p-value threshold for KS test for
#' non-uniformity of ranks.
#' @param p.adjust.method Multiple testing correction method,
#' see \link[stats]{p.adjust}.
#' @param max.friends.n The maximal number of friends for a marker.
#' A value $n$ means that we filter out a row if it has more
#' than $n$ friendly columns. 1 means we look only for unique (best) friends.
#' The string \code{"all"} (the default) and \code{NULL} both mean
#' \code{ncol(A)}, that is, do not filter markers by this parameter.
#' @param uniform.max The maximum of the uniform distribution of the ranks we
#' fit the null model, it can be the maximal possible rank that is common for
#' all rows and equals the number of rows \code{'c'} or the maximal observed
#' rank for the row we test now, \code{'m'} (default).
#' @param simulate.p.value K-S by Monte-Carlo if \code{TRUE};
#' default is \code{FALSE}, see [stats::ks.test()].
#' @param B number of or replicates if \code{simulate.p.value=TRUE}
#' default is 2000, see [stats::ks.test()].
#' @param .progress if \code{TRUE}, report what the call is doing. What you see
#' depends on the backend: a serial one draws a \code{cli} progress bar with a
#' percentage and the elapsed time, a parallel one only names the stage it has
#' reached, since the text progress bar of \code{BPPARAM} is switched off.
#' Neither renders when the output is redirected rather than shown in a
#' terminal, so build logs stay quiet. The default is \code{FALSE}.
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
#' friends_test_ks(A, threshold = .05)
#' friends_test_ks(A, threshold = .0001)
#' friends_test_ks(A, threshold = .05, uniform.max = "m")
#' friends_test_ks(A, threshold = .0001, uniform.max = "m")
#'
#' @importFrom stats p.adjust
#' @importFrom purrr array_branch compact pmap
#' @importFrom cli cli_progress_step cli_progress_done cli_progress_along
#' @importFrom methods is
#' @export
#'
friends_test_ks <- function(
    A = NULL,
    threshold = 0.05,
    p.adjust.method = "BH",
    max.friends.n = "all",
    uniform.max = "m",
    simulate.p.value = FALSE,
    B = 2000,
    .progress = FALSE,
    BPPARAM = NULL
) {
    if (.progress) {
        # cli needs the bar to appear at once; put the user's setting back
        # whichever way this call ends
        old_options <- options(cli.progress_show_after = 0)
        on.exit(options(old_options), add = TRUE)
    }

    if (threshold < 0 || threshold > 1) {
        stop("threshold must be between 0 and 1.")
    }
    # uniform.max: 'm' fits the null to the observed range, 'c' to the whole
    # rank scale, that is nrow(A); anything else must be a number
    if (uniform.max == "m" || uniform.max == "M") {
        uniform.max <- NA
    } else if (uniform.max == "c" || uniform.max == "C") {
        uniform.max <- nrow(A)
    } else if (!is.numeric(uniform.max)) {
        stop("uniform.max must be either 'm', 'M', 'c', 'C' or numeric.")
    }

    prep <- .ft_prepare(A, max.friends.n, .progress, BPPARAM)
    A <- prep$A
    max.friends.n <- prep$max.friends.n
    BPPARAM <- prep$BPPARAM
    all_ranks <- prep$ranks
    all_rank_rows <- prep$rows

    # calculate the p-values for null hypothesis for all the rank rows
    adj_nunif_pval <- unlist(
        .ft_map_rows(
            function(row, i, uniform.max, simulate.p.value, B) {
                friends.test::unif_ks_test(
                    row,
                    uniform.max = uniform.max,
                    simulate.p.value = simulate.p.value,
                    B = B
                )
            },
            rows = all_rank_rows,
            idx = seq_along(all_rank_rows),
            MoreArgs = list(
                uniform.max = uniform.max,
                simulate.p.value = simulate.p.value,
                B = B
            ),
            BPPARAM = BPPARAM,
            .progress = .progress,
            label = "Filtering out uniforms"
        ),
        use.names = FALSE
    ) |> p.adjust(method = p.adjust.method)

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
    #run ut all in purrr style
    #return: list of list of, trios
    #i, j, r -- vectors:
    #marker, friend, friend.rank
    marker_rank_rows <- purrr::array_branch(
        all_ranks[marker_indices, , drop = FALSE],
        1
    )
    col_names <- colnames(A)
    # return: list of lists of trios -- marker, friend, friend.rank
    ijrlist <- .ft_map_rows(
        function(row, i, max.possible.rank, max.friends.n, col_names) {
            step <- friends.test::best_step_fit(
                row,
                max.possible.rank = max.possible.rank
            )
            if (length(step$columns.on.left) > max.friends.n) {
                return(NULL) # marker has too many friends
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
        rows = marker_rank_rows,
        idx = marker_indices,
        MoreArgs = list(
            max.possible.rank = max.possible.rank,
            max.friends.n = max.friends.n,
            col_names = col_names
        ),
        BPPARAM = BPPARAM,
        .progress = .progress,
        label = "Identifying friends"
    )
    names(ijrlist) <- names(marker_rank_rows)

    if (.progress) cli::cli_progress_step("Compacting...")
    ijrlist <- purrr::compact(ijrlist)
    if (.progress) cli::cli_progress_done()
    ijrlist
}
