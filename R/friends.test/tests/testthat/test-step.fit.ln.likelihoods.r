test_that("step.fit.ln.likelihoods returns compact format for known input", {
    ranks <- c(97, 1, 98, 99, 100)
    rows.no <- 100
    result <- step.fit.ln.likelihoods(ranks, rows.no)
    k <- length(ranks)
    expect_equal(length(result$columns.order), k)
    expect_equal(length(result$best_ll_by_k1), k)
    expect_equal(length(result$best_l1_by_k1), k)
    expect_true(is.finite(result$uniform_ll))
    expect_equal(result$columns.order, c(2, 1, 3, 4, 5))
})

test_that("step.fit.ln.likelihoods.fullmesh returns fullmesh format for known input", {
    ranks <- c(97, 1, 98, 99, 100)
    rows.no <- 100
    result <- step.fit.ln.likelihoods.fullmesh(ranks, rows.no)
    expect_equal(length(result$columns.order), length(ranks))
    expect_equal(length(result$ln.likelihoods), rows.no)
    expect_equal(length(result$k1.by.l1), rows.no)
    expect_equal(result$columns.order, c(2, 1, 3, 4, 5))
})

test_that("returns error when a rank is higher than row.no", {
    ranks <- c(1, 2, 3, 4, 6)
    rows.no <- 5
    expect_error(step.fit.ln.likelihoods(ranks, rows.no))
    expect_error(step.fit.ln.likelihoods.fullmesh(ranks, rows.no))
    expect_error(step.fit.ln.likelihoods.fullmesh.enum(ranks, rows.no))
})

test_that(
    "step.fit.ln.likelihoods.fullmesh and .fullmesh.enum agree on 100 random examples",
    {
        set.seed(42)
        n_tests <- 100

        for (i in seq_len(n_tests)) {
            # vary both k (ncol) and max.possible.rank (nrow)
            k <- sample(3:30, 1)
            max.possible.rank <- sample(k:100, 1)

            # mix tied and unique ranks; ties cause empty k1 ranges
            tie_fraction <- sample(c(0, 0.2, 0.5), 1)
            n_unique <- max(2L, round(k * (1 - tie_fraction)))
            pool <- sample(seq_len(max.possible.rank), n_unique)
            ranks <- sample(pool, k, replace = TRUE)

            fm  <- step.fit.ln.likelihoods.fullmesh(ranks, max.possible.rank)
            ref <- step.fit.ln.likelihoods.fullmesh.enum(ranks, max.possible.rank)

            expect_equal(
                fm$columns.order, ref$columns.order,
                info = paste("columns.order mismatch at iteration", i)
            )
            expect_equal(
                fm$ln.likelihoods, ref$ln.likelihoods,
                tolerance = 1e-10,
                info = paste("ln.likelihoods mismatch at iteration", i)
            )
            expect_equal(
                fm$k1.by.l1, ref$k1.by.l1,
                info = paste("k1.by.l1 mismatch at iteration", i)
            )
        }
    }
)
