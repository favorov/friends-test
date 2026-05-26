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

test_that(
    ".step_fit_compact and .step_fit_fullmesh agree on targeted cases",
    {
        # Each case exercises a specific algorithmic property.
        # For all cases we compare the two private implementations directly.
        cases <- list(
            # 1. All same rank — no valid split, all best_ll = -Inf
            list(ranks = c(5L, 5L, 5L, 5L), M = 10L, label = "all same rank",
                check = function(cpt, fmh) {
                    expect_true(all(!is.finite(cpt$best_ll_by_k1)),
                        info = "compact: all -Inf when all ranks equal")
                    expect_true(all(!is.finite(fmh$best_ll_by_k1)),
                        info = "fullmesh: all -Inf when all ranks equal")
                }),
            # 2. k=2, unique ranks — ll convex so max at endpoint, here l1=2
            #    ll(l1) = -log(l1*(M-l1)); minimise product at l1=2 (vs l1=7)
            list(ranks = c(2L, 8L), M = 10L, label = "k=2 unique",
                check = function(cpt, fmh) {
                    expect_equal(cpt$best_l1_by_k1[1L], 2L,
                        info = "compact: optimal l1=2 for k=2")
                    expect_equal(fmh$best_l1_by_k1[1L], 2L,
                        info = "fullmesh: optimal l1=2 for k=2")
                }),
            # 3. Symmetric tie-breaking: ranks c(3,3,8,8) M=10, k1=2 range=[3,7]
            #    By symmetry ll(3)=ll(7); convention picks the larger l1=7
            list(ranks = c(3L, 3L, 8L, 8L), M = 10L, label = "symmetric tie",
                check = function(cpt, fmh) {
                    expect_equal(cpt$best_l1_by_k1[2L], 7L,
                        info = "compact: tie resolved to larger l1=7")
                    expect_equal(fmh$best_l1_by_k1[2L], 7L,
                        info = "fullmesh: tie resolved to larger l1=7")
                }),
            # 4. Clean cluster, tall matrix (nrow >> ncol) — compact path
            list(ranks = c(1L, 1L, 1L, 9L, 9L, 9L), M = 20L,
                label = "clean cluster tall"),
            # 5. Clean cluster, wide matrix (nrow < ncol) — fullmesh path
            list(ranks = c(1L, 1L, 1L, 5L, 5L, 5L), M = 5L,
                label = "clean cluster wide"),
            # 6. Square boundary (nrow == ncol, dispatch threshold)
            list(ranks = c(1L, 2L, 3L, 4L), M = 4L,
                label = "square boundary"),
            # 7. Heavy ties: half the values tied, some k1 ranges empty
            list(ranks = c(1L, 1L, 5L, 5L, 5L, 5L), M = 10L,
                label = "heavy ties")
        )

        for (case in cases) {
            cpt <- .step_fit_compact(case$ranks, case$M)
            fmh <- .step_fit_fullmesh(case$ranks, case$M)

            expect_equal(cpt$columns.order, fmh$columns.order,
                info = paste("columns.order:", case$label))
            expect_equal(cpt$best_ll_by_k1, fmh$best_ll_by_k1,
                tolerance = 1e-10,
                info = paste("best_ll_by_k1:", case$label))
            expect_equal(cpt$best_l1_by_k1, fmh$best_l1_by_k1,
                info = paste("best_l1_by_k1:", case$label))
            expect_equal(cpt$uniform_ll, fmh$uniform_ll,
                tolerance = 1e-10,
                info = paste("uniform_ll:", case$label))

            if (!is.null(case$check)) case$check(cpt, fmh)
        }
    }
)
