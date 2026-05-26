# Reference implementation (O(nrow) full-mesh enumeration) kept here for
# validation only — not part of the public API.
.fullmesh_enum_ref <- function(ranks, max.possible.rank) {
    columns.order <- order(ranks)
    sorted_ranks <- ranks[columns.order]
    ln.likelihoods <- rep(0, max.possible.rank)
    k1.by.l1 <- rep(0, max.possible.rank)
    k <- length(sorted_ranks)
    k1 <- 0
    for (l1 in seq_len(max.possible.rank - 1)) {
        while (k1 < k && sorted_ranks[k1 + 1] <= l1) k1 <- k1 + 1
        k1.by.l1[l1] <- k1
        p1 <- k1 / k
        if (p1 > 0)
            ln.likelihoods[l1] <- ln.likelihoods[l1] + k1 * log(p1 / l1)
        if (p1 < 1)
            ln.likelihoods[l1] <- ln.likelihoods[l1] +
                (k - k1) * log((1 - p1) / (max.possible.rank - l1))
    }
    ln.likelihoods[max.possible.rank] <- k * log(1 / max.possible.rank)
    k1.by.l1[max.possible.rank] <- k
    list(
        columns.order  = columns.order,
        ln.likelihoods = ln.likelihoods,
        k1.by.l1       = k1.by.l1
    )
}

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

test_that(paste0(
    "step.fit.ln.likelihoods.fullmesh returns ",
    "fullmesh format for known input"
), {
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
})

test_that(paste0(
    "step.fit.ln.likelihoods.fullmesh and ",
    "reference enum agree on 3 representative examples"
), {
    set.seed(42)
    cases <- list(
        list(ranks = c(97, 1, 98, 99, 100), max.possible.rank = 100,  shape = "tall"),
        list(ranks = c(3, 1, 5, 2, 4),      max.possible.rank = 5,    shape = "square"),
        list(ranks = c(2, 1, 3, 3, 2),      max.possible.rank = 3,    shape = "wide")
    )

    for (case in cases) {
        ranks <- case$ranks
        max.possible.rank <- case$max.possible.rank
        shape <- case$shape

        fm  <- step.fit.ln.likelihoods.fullmesh(ranks, max.possible.rank)
        ref <- .fullmesh_enum_ref(ranks, max.possible.rank)

        expect_equal(fm$columns.order,  ref$columns.order,
            info = paste("columns.order mismatch:", shape))
        expect_equal(fm$ln.likelihoods, ref$ln.likelihoods, tolerance = 1e-10,
            info = paste("ln.likelihoods mismatch:", shape))
        expect_equal(fm$k1.by.l1,       ref$k1.by.l1,
            info = paste("k1.by.l1 mismatch:", shape))
    }
})

test_that(paste0(
    "step.fit.ln.likelihoods adaptive selection ",
    "agrees with fullmesh.enum on 100 examples"
), {
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
        ref <- .fullmesh_enum_ref(ranks, max.possible.rank)

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
