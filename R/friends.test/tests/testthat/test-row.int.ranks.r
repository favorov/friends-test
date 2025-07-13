test_that("ranks assigned correctly", {
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  ranks <- tag.int.ranks(mat)
  expect_equal(ranks, matrix(c(2, 1, 2, 1, 2, 1), nrow = 2))
})

test_that("number of ranks equal to number of tags", {
  mat <- matrix(c(1:100), nrow = 5)
  ranks <- tag.int.ranks(mat)
  expect_equal(nrow(ranks), 5)
})