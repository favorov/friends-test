test_mat <- function() {
    set.seed(11)
    n <- 60
    k <- 6
    A <- matrix(rnorm(n * k), nrow = n, ncol = k)
    A[1:6, 1:3] <- A[1:6, 1:3] + 8
    rownames(A) <- paste0("row", seq_len(n))
    colnames(A) <- paste0("col", seq_len(k))
    A
}


test_that("friends_test dispatches to the branch named by mode", {
    A <- test_mat()

    set.seed(3)
    viaDispatcher <- friends_test(A, threshold = 0.05)
    set.seed(3)
    direct <- friends_test_ks(A, threshold = 0.05)
    expect_identical(viaDispatcher, direct)

    set.seed(3)
    viaDispatcher <- friends_test(A, mode = "bic", prior.to.have.friends = 0.5)
    set.seed(3)
    direct <- friends_test_bic(A, prior.to.have.friends = 0.5)
    expect_identical(viaDispatcher, direct)
})


test_that("friends_test defaults to the KS branch", {
    A <- test_mat()
    set.seed(3)
    byDefault <- friends_test(A)
    set.seed(3)
    explicit <- friends_test(A, mode = "ks")
    expect_identical(byDefault, explicit)
})


test_that("friends_test forwards the arguments both branches share", {
    A <- test_mat()

    set.seed(3)
    viaDispatcher <- friends_test(A, mode = "bic",
        prior.to.have.friends = 0.5, max.friends.n = 2)
    set.seed(3)
    direct <- friends_test_bic(A, prior.to.have.friends = 0.5,
        max.friends.n = 2)
    expect_identical(viaDispatcher, direct)
})


test_that("friends_test rejects an argument belonging to the other mode", {
    A <- test_mat()

    expect_error(
        friends_test(A, mode = "bic", threshold = 0.05),
        "belongs to mode \"ks\""
    )
    expect_error(
        friends_test(A, mode = "ks", prior.to.have.friends = 0.5),
        "belongs to mode \"bic\""
    )
})


test_that("friends_test rejects an unknown mode", {
    expect_error(friends_test(test_mat(), mode = "bayes"))
})


test_that("friends_test leaves other argument errors to the branch", {
    # not owned by either branch: the callee reports it, we do not
    expect_error(
        friends_test(test_mat(), nonsense = 1),
        "unused argument"
    )
})
