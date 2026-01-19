test_that("unif.ks.test returns large p-value for uniform data", {
    set.seed(1)
    x <- rep(1, 10)
    res <- unif.ks.test(x)
    expect_true(res > 0.05)
})

test_that("unif.ks.test returns small p-value for non-uniform data", {
    set.seed(1)
    x <- c(1:9, 100)
    res <- unif.ks.test(x)
    expect_true(res < 0.05)
})
