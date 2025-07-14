test_that("step.ln.likelihoods returns expected values for known input", {
  ranks <- c(97, 1, 98, 99, 100)
  rows.no <- 100
  result <- step.fit.ln.likelihoods(ranks, rows.no)
  expect_equal(length(result$columns.order), length(ranks))
  expect_equal(length(result$ln.likelihoods), rows.no)
  expect_equal(length(result$k1.by.l1), rows.no)
  expect_equal(result$columns.order, c(2, 1, 3, 4, 5))
})

test_that("returns error when a rank is higher than row.no", {
  ranks <- c(1, 2, 3, 4, 6)
  rows.no <- 5
  expect_error(step.fit.ln.likelihoods(ranks, rows.no))
})