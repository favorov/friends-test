test_that("unif_ks_test returns large p-value for uniform data", {
    set.seed(1)
    x <- rep(1, 10)
    res <- unif_ks_test(x)
    expect_true(res > 0.05)
})

test_that("unif_ks_test returns small p-value for non-uniform data", {
    set.seed(1)
    x <- c(1:9, 100)
    res <- unif_ks_test(x)
    expect_true(res < 0.05)
})


test_that("the three uniform.null conventions differ as intended", {
    set.seed(1)
    N <- 1000
    # a row spread evenly over the lower half: flat, but not over 1..N
    shifted <- sample.int(N %/% 2, 12, replace = TRUE)

    # fitting the support to the row makes it invariant to that shift
    expect_gt(unif_ks_test(shifted), 0.05)

    # fixing the support does not: the concentration is itself evidence
    expect_lt(
        unif_ks_test(shifted, "continuity", max.possible.rank = N),
        0.05
    )
    expect_lt(
        unif_ks_test(shifted, "randomized", max.possible.rank = N),
        0.05
    )
})


test_that("uniform.null is validated, and two of them need the rank scale", {
    ranks <- c(3L, 17L, 42L, 55L)

    expect_error(unif_ks_test(ranks, "nonsense"))
    expect_error(unif_ks_test(ranks, "continuity"), "max.possible.rank")
    expect_error(unif_ks_test(ranks, "randomized"), "max.possible.rank")

    # the default does not need it
    expect_no_error(unif_ks_test(ranks))
})


test_that("only the randomized convention varies between runs", {
    set.seed(2)
    ranks <- sample.int(500, 10, replace = TRUE)

    # the tie-breaking jitter is far too small to move a decision
    spread <- diff(range(replicate(50, unif_ks_test(ranks))))
    expect_lt(spread, 1e-6)

    # displacing by half a unit is a real randomisation
    spread <- diff(range(
        replicate(50, unif_ks_test(ranks, "randomized", max.possible.rank = 500))
    ))
    expect_gt(spread, 1e-6)
})
