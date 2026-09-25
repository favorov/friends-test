# The rank scale the step models are fitted on, in both branches.  "continuity" is the
# whole 1..N scale, which is what the branch did before 0.99.23; "observed"
# takes the ends from the row, as the KS branch does by default.


test_that("continuity reproduces the fits of 0.99.22", {
    # captured from the 0.99.22 code, so this is a regression guard rather
    # than a restatement of the present implementation
    expect_fit <- function(ranks, N, prior, left, step.rank) {
        fit <- best_step_fit_bic(ranks, N, prior, uniform.null = "continuity")
        info <- paste("prior =", prior)
        expect_identical(fit$columns.on.left, left, info = info)
        expect_identical(fit$population.on.left, length(left), info = info)
        expect_identical(fit$best.step.rank, step.rank, info = info)
    }

    one <- c(97L, 1L, 98L, 99L, 100L)
    for (prior in c(0.5, 0.25, 0.001)) expect_fit(one, 100L, prior, 2L, 96L)
    expect_fit(one, 100L, 1e-6, integer(0L), 100L)

    run <- c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)
    for (prior in c(0.5, 0.25)) expect_fit(run, 21L, prior, 1:7, 7L)
    for (prior in c(0.001, 1e-6)) expect_fit(run, 21L, prior, integer(0L), 21L)

    twos <- c(1L, 1L, 1L, 1L, 6L, 6L, 6L, 6L, 6L, 6L)
    for (prior in c(0.5, 0.25)) expect_fit(twos, 21L, prior, 1:4, 1L)
    for (prior in c(0.001, 1e-6)) expect_fit(twos, 21L, prior, integer(0L), 21L)
})


test_that("an exact tie now goes to the uniform model", {
    # ranks that fill the scale leave every split with exactly the uniform
    # log-likelihood, and at prior 0.5 the two sides of the comparison are
    # equal.  0.99.22 gave the step four columns here
    full <- c(3L, 2L, 5L, 1L, 4L)
    models <- .step_fit_compact(full, 5L)
    valid <- models$best_ll_by_k1[is.finite(models$best_ll_by_k1)]
    expect_identical(unique(valid), models$uniform_ll)

    fit <- best_step_fit_bic(full, 5L, 0.5, uniform.null = "continuity")
    expect_identical(fit$columns.on.left, integer(0L))
    expect_identical(fit$population.on.left, 0L)
})


test_that("observed is continuity on the rebased row", {
    ranks <- c(507L, 501L, 519L, 512L, 503L, 515L)
    lowest <- min(ranks)
    size <- max(ranks) - lowest + 1L

    for (prior in c(0.5, 0.01, 1e-4)) {
        observed <- best_step_fit_bic(ranks, 1000L, prior)
        rebased <- best_step_fit_bic(
            ranks - lowest + 1L, size, prior,
            uniform.null = "continuity"
        )
        expect_identical(observed$columns.on.left, rebased$columns.on.left)
        expect_identical(observed$columns.on.right, rebased$columns.on.right)
        expect_identical(observed$population.on.left, rebased$population.on.left)
        expect_identical(
            observed$best.step.rank,
            rebased$best.step.rank + lowest - 1L
        )
    }
})


test_that("observed reports the step on the scale of the ranks given", {
    ranks <- c(880L, 884L, 881L, 202L, 205L, 201L)
    fit <- best_step_fit_bic(ranks, 1000L, 0.5)
    expect_identical(fit$columns.on.left, c(6L, 4L, 5L))
    # the step separates the two groups, so it sits between them
    expect_gte(fit$best.step.rank, max(ranks[fit$columns.on.left]))
    expect_lt(fit$best.step.rank, min(ranks[fit$columns.on.right]))
})


test_that("the no-step sentinel follows the convention", {
    ranks <- c(507L, 501L, 519L, 512L, 503L, 515L)
    expect_identical(
        best_step_fit_bic(ranks, 1000L, 1e-50)$best.step.rank,
        max(ranks)
    )
    expect_identical(
        best_step_fit_bic(
            ranks, 1000L, 1e-50, uniform.null = "continuity"
        )$best.step.rank,
        1000L
    )
})


test_that("degenerate rows give no friends under observed", {
    # a run of consecutive ranks fills its own observed scale
    for (prior in c(0.5, 0.25, 1e-6)) {
        fit <- best_step_fit_bic(101:108, 1000L, prior)
        expect_identical(fit$columns.on.left, integer(0L), info = prior)
        expect_identical(fit$best.step.rank, 108L, info = prior)
    }

    # every rank tied: the observed scale is one rank wide
    tied <- rep(77L, 8L)
    fit <- best_step_fit_bic(tied, 1000L, 0.5)
    expect_identical(fit$population.on.left, 0L)
    expect_identical(fit$best.step.rank, 77L)
})


test_that("the randomized setting of the KS branch is not offered here", {
    expect_error(
        best_step_fit_bic(c(1L, 5L, 9L), 10L, 0.5, uniform.null = "randomized"),
        "should be one of"
    )
    expect_error(
        friends_test_bic(
            matrix(runif(20), nrow = 5), 0.5, uniform.null = "randomized"
        ),
        "should be one of"
    )
})


test_that("friends_test_bic defaults to observed and forwards the choice", {
    set.seed(5)
    A <- matrix(rnorm(60 * 6), nrow = 60, ncol = 6)
    A[1:5, 1:2] <- A[1:5, 1:2] + 8
    rownames(A) <- paste0("row", seq_len(60))
    colnames(A) <- paste0("col", seq_len(6))

    set.seed(3)
    by.default <- friends_test_bic(A, 0.01)
    set.seed(3)
    explicit <- friends_test_bic(A, 0.01, uniform.null = "observed")
    expect_identical(by.default, explicit)

    set.seed(3)
    whole.scale <- friends_test_bic(A, 0.01, uniform.null = "continuity")
    expect_false(identical(by.default, whole.scale))

    # and the dispatcher passes it through
    set.seed(3)
    dispatched <- friends_test(
        A, mode = "bic", prior.to.have.friends = 0.01,
        uniform.null = "continuity"
    )
    expect_identical(dispatched, whole.scale)
})


test_that("a tie between friend counts goes to the smaller set", {
    # on its own observed scale this row leaves one friend and four equally
    # likely; the rule is shared with best_step_fit(), so the KS branch
    # locates the step the same way
    ranks <- c(1L, 2L, 2L, 3L, 2L)
    models <- .step_fit_compact(ranks, 3L)
    valid <- which(is.finite(models$best_ll_by_k1))
    best <- models$best_ll_by_k1[valid]
    expect_identical(valid[best == max(best)], c(1L, 4L))

    fit <- best_step_fit_bic(ranks, 5L, 0.5)
    expect_identical(fit$population.on.left, 1L)
    expect_identical(fit$columns.on.left, 1L)

    expect_identical(best_step_fit(ranks, 3L)$population.on.left, 1L)
})


test_that("best_step_fit follows the scale it is given", {
    # a run of consecutive ranks fills its own scale, so every friend count is
    # equally likely and the tie rule takes the smallest
    tight <- 101:108
    expect_identical(best_step_fit(tight, 1000L)$population.on.left, 1L)
    expect_identical(
        best_step_fit(
            tight, 1000L, uniform.null = "continuity"
        )$population.on.left,
        7L
    )

    spread <- c(1L, 2L, 3L, 900L, 901L, 902L)
    lowest <- min(spread)
    size <- max(spread) - lowest + 1L
    expect_identical(
        best_step_fit(spread, 1000L)$best.step.rank,
        best_step_fit(
            spread - lowest + 1L, size, uniform.null = "continuity"
        )$best.step.rank + lowest - 1L
    )
})


test_that("friends_test_ks locates the step on the scale it tested on", {
    set.seed(9)
    A <- matrix(rnorm(20 * 8), nrow = 20, ncol = 8)
    A[1, 1:3] <- A[1, 1:3] + 12
    A[2, 4:5] <- A[2, 4:5] + 12
    rownames(A) <- paste0("row", seq_len(20))
    colnames(A) <- paste0("col", seq_len(8))
    # rnorm leaves no ties, so the ranks do not depend on the tie-breaking RNG
    ranks <- row_int_ranks(A)

    for (null in c("observed", "continuity", "randomized")) {
        set.seed(4)
        got <- friends_test_ks(A, threshold = 0.05, uniform.null = null)
        expect_gt(length(got), 0L)

        # the step model is discrete, so randomized reaches it as continuity
        step.null <- if (null == "observed") "observed" else "continuity"
        for (nm in names(got)) {
            fit <- best_step_fit(
                ranks[match(nm, rownames(A)), ], nrow(A),
                uniform.null = step.null
            )
            expect_identical(
                unname(vapply(got[[nm]], function(t) t[["friend"]], integer(1))),
                fit$columns.on.left,
                info = paste(null, nm)
            )
        }
    }
})
