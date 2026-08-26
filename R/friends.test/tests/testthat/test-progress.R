# .progress picks a different code path for a serial and a parallel backend:
# the serial one draws a cli bar, the parallel one only names the stage. What
# they must never do is change the answer.

progress_mat <- function() {
    set.seed(11)
    n <- 60
    k <- 6
    A <- matrix(rnorm(n * k), nrow = n, ncol = k)
    A[1:8, 1:3] <- A[1:8, 1:3] + 8
    rownames(A) <- paste0("row", seq_len(n))
    colnames(A) <- paste0("col", seq_len(k))
    A
}


test_that("the serial bar does not change what friends_test_ks returns", {
    A <- progress_mat()
    # a threshold that actually yields markers, so that the run goes through
    # the second stage and the compacting step rather than returning early
    args <- list(A, threshold = 0.5, p.adjust.method = "none")

    set.seed(3)
    quiet <- do.call(friends_test_ks, args)
    set.seed(3)
    noisy <- suppressMessages(
        do.call(friends_test_ks, c(args, list(.progress = TRUE)))
    )
    expect_identical(quiet, noisy)
    expect_gt(length(quiet), 0)
})


test_that("the serial bar does not change what friends_test_bic returns", {
    A <- progress_mat()
    set.seed(3)
    quiet <- friends_test_bic(A, prior.to.have.friends = 0.5)
    set.seed(3)
    noisy <- suppressMessages(
        friends_test_bic(A, prior.to.have.friends = 0.5, .progress = TRUE)
    )
    expect_identical(quiet, noisy)
})


test_that("the parallel path reports without changing the answer", {
    skip_on_os("windows")   # MulticoreParam forks
    A <- progress_mat()
    param <- BiocParallel::MulticoreParam(workers = 2)

    set.seed(3)
    quiet <- friends_test_ks(A)
    set.seed(3)
    noisy <- suppressMessages(
        friends_test_ks(A, .progress = TRUE, BPPARAM = param)
    )
    expect_identical(quiet, noisy)
})


test_that(".progress leaves the cli option as it found it", {
    A <- progress_mat()
    before <- getOption("cli.progress_show_after")

    suppressMessages(friends_test_ks(A, .progress = TRUE))
    expect_identical(getOption("cli.progress_show_after"), before)

    # and when the call fails part way through
    expect_error(
        suppressMessages(friends_test_ks(A, .progress = TRUE, threshold = 5))
    )
    expect_identical(getOption("cli.progress_show_after"), before)
})


test_that("the backend's own text progress bar stays off", {
    # the package draws its own; BiocParallel's was disabled in 0.99.20
    param <- BiocParallel::SerialParam()
    BiocParallel::bpprogressbar(param) <- TRUE
    expect_false(BiocParallel::bpprogressbar(ft_bpparam(param, TRUE)))
})


test_that("the startup message is only for a person at a console", {
    hook <- friends.test:::.onAttach

    # under R CMD check, in a script or on a build machine
    expect_silent(hook("", "friends.test"))
    expect_null(hook("", "friends.test"))

    # and with someone watching
    pretend_interactive <- hook
    environment(pretend_interactive) <- list2env(
        list(interactive = function() TRUE),
        parent = environment(hook)
    )
    expect_message(pretend_interactive("", "friends.test"), "friends.test")
})
