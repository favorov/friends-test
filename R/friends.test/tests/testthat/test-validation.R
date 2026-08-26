# The refusals. Coverage showed these were the last untested lines: a check
# that never fires in a passing test is a check nobody has run.

test_that("the main functions refuse something that is not a matrix", {
    # each branch checks its own parameter before the shared prologue looks at
    # the matrix, so give the Bayesian one a usable prior and ask only about
    # the matrix
    calls <- list(
        function(A) friends_test_ks(A),
        function(A) friends_test_bic(A, prior.to.have.friends = 0.5)
    )
    for (fn in calls) {
        expect_error(fn(NULL), "2D matrix")
        expect_error(fn(1:10), "2D matrix")
        expect_error(fn(array(1:8, c(2, 2, 2))), "2D matrix")
    }
})


test_that("max.friends.n is checked against the shape of the matrix", {
    A <- matrix(rnorm(60), nrow = 20, ncol = 3)

    expect_error(friends_test_ks(A, max.friends.n = 0), "between 1 and")
    expect_error(friends_test_ks(A, max.friends.n = 4), "between 1 and")
    expect_error(friends_test_ks(A, max.friends.n = "most"), "single number")
    expect_error(friends_test_ks(A, max.friends.n = c(1, 2)), "single number")
})


test_that("each branch checks its own parameter", {
    A <- matrix(rnorm(60), nrow = 20, ncol = 3)

    expect_error(friends_test_ks(A, threshold = -1), "between 0 and 1")
    expect_error(friends_test_ks(A, threshold = 5), "between 0 and 1")

    # the prior has no default worth using: it must be given
    expect_error(friends_test_bic(A), "prior.to.have.friends")
    expect_error(
        friends_test_bic(A, prior.to.have.friends = 2),
        "prior.to.have.friends"
    )
})


test_that("row_int_ranks needs both dimensions above one", {
    expect_error(row_int_ranks(matrix(1:3, nrow = 1)), "dimensions")
    expect_error(row_int_ranks(matrix(1:3, ncol = 1)), "dimensions")
})


test_that("neglect_diagonal blanks the diagonal of a square matrix", {
    set.seed(5)
    A <- matrix(rnorm(25), nrow = 5, ncol = 5)

    ranks <- row_int_ranks(A, neglect_diagonal = TRUE)
    expect_true(all(is.na(diag(ranks))))
    expect_false(any(is.na(ranks[upper.tri(ranks)])))

    # the ranks of the remaining entries run 1..(n-1) within each column
    expect_equal(sort(ranks[!is.na(ranks[, 1]), 1]), 1:4)
})


test_that("neglect_diagonal is refused for a non-square matrix", {
    set.seed(5)
    A <- matrix(rnorm(20), nrow = 5, ncol = 4)

    expect_warning(
        ranks <- row_int_ranks(A, neglect_diagonal = TRUE),
        "square"
    )
    # and having warned, it carries on without touching anything
    expect_false(any(is.na(ranks)))
})


test_that("step_fit_ln_likelihoods says which input it objects to", {
    expect_error(step_fit_ln_likelihoods(c(1, 2, 6), 5), "max.possible.rank")
    expect_error(step_fit_ln_likelihoods(c(1.5, 2), 5), "whole numbers")
    expect_error(step_fit_ln_likelihoods(c(0, 2), 5), "1 or greater")
    expect_warning(step_fit_ln_likelihoods(matrix(1:4, 2), 9), "dim")
})


test_that("a marker with too many friends is dropped", {
    set.seed(4)
    n <- 40
    k <- 6
    A <- matrix(rnorm(n * k), nrow = n, ncol = k)
    # this row is at the top of every column, so it has k friends
    A[1, ] <- A[1, ] + 20
    rownames(A) <- paste0("row", seq_len(n))
    colnames(A) <- paste0("col", seq_len(k))

    set.seed(3)
    kept <- friends_test_ks(A, threshold = 0.5, p.adjust.method = "none")
    set.seed(3)
    dropped <- friends_test_ks(
        A, threshold = 0.5, p.adjust.method = "none", max.friends.n = 1
    )

    expect_true(length(dropped) <= length(kept))
    expect_true(all(lengths(dropped) <= 1))
})
