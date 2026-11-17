test_that("a row holding a single rank is not a marker", {
    # the fitted support has no width, so there is no break to find
    expect_identical(unif_ks_test(rep(1L, 10)), 1)

    # against a fixed support the same row is strong evidence
    expect_lt(
        unif_ks_test(rep(1L, 10), "continuity", max.possible.rank = 100),
        0.05
    )
})

test_that("unif_ks_test returns small p-value for non-uniform data", {
    expect_lt(unif_ks_test(c(1:9, 100)), 0.05)
})


test_that("ties are kept, and move the test onto the asymptotic law", {
    tied <- c(3L, 17L, 17L, 17L, 42L, 55L, 55L, 61L, 90L)
    free <- c(3L, 17L, 29L, 42L, 55L, 61L, 90L)

    # ks.test warns about the ties; the warning is answered here, not passed on
    expect_no_warning(p.tied <- unif_ks_test(tied))
    expect_equal(p.tied, suppressWarnings(stats::ks.test(
        as.double(tied), "punif", min = 3, max = 90, exact = FALSE
    )$p.value))

    # a tie-free row keeps the exact law, which is calibrated for it
    expect_equal(unif_ks_test(free), stats::ks.test(
        as.double(free), "punif", min = 3, max = 90
    )$p.value)
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


test_that("the fitted support puts a floor under the p-value", {
    # with the support fitted to the row, the KS statistic cannot exceed
    # (k - 1) / k, so a row of k columns cannot reach below 2 / k^k. The floor
    # is approached by a row with all its ranks but one at one end.
    for (k in 3:6) {
        extreme <- c(seq_len(k - 1L), 1e9)
        bound <- 2 / k^k
        expect_gt(unif_ks_test(extreme), bound)
        expect_lt(unif_ks_test(extreme), bound * 1.001)
    }

    # three columns therefore yield no marker at any conventional threshold,
    # while a fixed support has no such floor
    expect_gt(unif_ks_test(c(1, 2, 1e9)), 0.05)
    expect_lt(
        unif_ks_test(c(1L, 1L, 1L), "continuity", max.possible.rank = 1000),
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

    # nothing is displaced in these two, so they repeat exactly
    expect_length(unique(replicate(20, unif_ks_test(ranks))), 1L)
    expect_length(unique(replicate(
        20, unif_ks_test(ranks, "continuity", max.possible.rank = 500)
    )), 1L)

    # displacing by half a unit is a real randomisation
    expect_gt(
        diff(range(replicate(
            50, unif_ks_test(ranks, "randomized", max.possible.rank = 500)
        ))),
        1e-3
    )
})

test_that("simulate.p.value and B are ignored", {
    ranks <- c(3L, 17L, 29L, 42L, 55L, 61L, 90L)

    expect_warning(
        p <- unif_ks_test(ranks, simulate.p.value = TRUE),
        "are ignored"
    )
    expect_identical(p, unif_ks_test(ranks))

    expect_warning(
        unif_ks_test(ranks, B = 10),
        "are ignored"
    )

    # passing the default value is still passing it
    expect_warning(
        unif_ks_test(ranks, simulate.p.value = FALSE),
        "are ignored"
    )
})
