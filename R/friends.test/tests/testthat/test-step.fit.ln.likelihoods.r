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
            # vary both k (ncol) and max.possible.rank (nrow);
            # include tall (nrow > ncol), square and wide (nrow <= ncol) cases
            k <- sample(3:30, 1)
            shape <- sample(c("tall", "square", "wide"), 1)
            max.possible.rank <- switch(shape,
                tall   = sample(max(k + 1L, 10L):100L, 1),
                square = k,
                wide   = sample(3L:k, 1)
            )
            if (max.possible.rank < 3L) max.possible.rank <- 3L

            # mix tied and unique ranks; ties cause empty k1 ranges
            tie_fraction <- sample(c(0, 0.2, 0.5), 1)
            n_unique <- min(
                max(2L, round(k * (1 - tie_fraction))),
                max.possible.rank
            )
            pool <- sample(seq_len(max.possible.rank), n_unique)
            ranks <- sample(pool, k, replace = TRUE)

            fm  <- step.fit.ln.likelihoods.fullmesh(ranks, max.possible.rank)
            ref <- step.fit.ln.likelihoods.fullmesh.enum(ranks, max.possible.rank)

            expect_equal(
                fm$columns.order, ref$columns.order,
                info = paste("columns.order mismatch at iteration", i, shape)
            )
            expect_equal(
                fm$ln.likelihoods, ref$ln.likelihoods,
                tolerance = 1e-10,
                info = paste("ln.likelihoods mismatch at iteration", i, shape)
            )
            expect_equal(
                fm$k1.by.l1, ref$k1.by.l1,
                info = paste("k1.by.l1 mismatch at iteration", i, shape)
            )
        }
    }
)

test_that(
    "step.fit.ln.likelihoods adaptive selection agrees with fullmesh.enum on 100 examples",
    {
        set.seed(123)
        n_tests <- 100

        for (i in seq_len(n_tests)) {
            k <- sample(3:30, 1)
            shape <- sample(c("tall", "square", "wide"), 1)
            max.possible.rank <- switch(shape,
                tall   = sample(max(k + 1L, 10L):100L, 1),
                square = k,
                wide   = sample(3L:k, 1)
            )
            if (max.possible.rank < 3L) max.possible.rank <- 3L

            tie_fraction <- sample(c(0, 0.2, 0.5), 1)
            n_unique <- min(
                max(2L, round(k * (1 - tie_fraction))),
                max.possible.rank
            )
            pool <- sample(seq_len(max.possible.rank), n_unique)
            ranks <- sample(pool, k, replace = TRUE)

            adaptive <- step.fit.ln.likelihoods(ranks, max.possible.rank)
            ref      <- step.fit.ln.likelihoods.fullmesh.enum(ranks, max.possible.rank)

            # Check compact fields against reference fullmesh
            valid_k1 <- which(is.finite(adaptive$best_ll_by_k1))

            # best overall split must match (excluding degenerate k1=0 or k1=k)
            valid_l1 <- which(ref$k1.by.l1 > 0L & ref$k1.by.l1 < k)
            if (length(valid_k1) > 0L && length(valid_l1) > 0L) {
                # same tie-breaking as best.step.fit: largest best_l1 among tied k1
                max.ln.l <- max(adaptive$best_ll_by_k1[valid_k1])
                tied_k1  <- valid_k1[
                    adaptive$best_ll_by_k1[valid_k1] == max.ln.l
                ]
                best_k1  <- tied_k1[which.max(adaptive$best_l1_by_k1[tied_k1])]
                best_l1  <- adaptive$best_l1_by_k1[best_k1]

                ref_best <- max(ref$ln.likelihoods[valid_l1])
                expect_equal(
                    max.ln.l, ref_best,
                    tolerance = 1e-10,
                    info = paste("best ll mismatch at iteration", i, shape)
                )
                # reference: largest l1 (last max convention) among valid splits
                ref_best_l1 <- max(which(
                    ref$k1.by.l1 > 0L & ref$k1.by.l1 < k &
                    ref$ln.likelihoods == ref_best
                ))
                expect_equal(
                    best_l1, ref_best_l1,
                    info = paste("best l1 mismatch at iteration", i, shape)
                )
            }

            # uniform ll must match
            expect_equal(
                adaptive$uniform_ll,
                ref$ln.likelihoods[max.possible.rank],
                tolerance = 1e-10,
                info = paste("uniform_ll mismatch at iteration", i, shape)
            )
        }
    }
)
