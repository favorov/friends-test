test_that("no errors in simplest case", {
  mat <- diag(nrow = 5, ncol = 5)
  rownames(mat) <- paste0("tag", 1:5)
  colnames(mat) <- paste0("coll", 1:5)
  expect_no_error(friends.test.bic(mat, .5))
})

test_that("best friend is determined correctly", {
  text <- "    coll1     coll2     coll3      coll4     coll5
            tag1 0.1765568 0.7176185 0.2121425 0.01339033 0.5995658
            tag2 0.6870228 0.9919061 0.6516738 0.38238796 0.4935413
            tag3 0.3841037 0.3800352 0.1255551 0.86969085 0.1862176
            tag4 0.7698414 0.7774452 0.2672207 0.34034900 0.8273733
            tag5 0.0000000 0.0000000 0.0000000 0.00000000 1.0000000"
  attention <- as.matrix(read.table(text = text, header = TRUE))

  friends <- friends.test.bic(attention, .25, max.friends.n = 1)
  expect_equivalent(dim(friends), c(1, 5))
  expect_equivalent(friends$marker, c("tag5"))
  expect_equivalent(friends$friend, c("coll5"))
  expect_equivalent(friends$marker.index, c(5))
  expect_equivalent(friends$friend.index, c(5))
  expect_equivalent(friends$marker, rownames(attention)[friends$marker.index])
  expect_equivalent(friends$friend, colnames(attention)[friends$friend.index])
  expect_equivalent(friends$friend.rank, c(1))

  friends <- friends.test.bic(attention, .5, max.friends.n = 1)
  expect_equivalent(dim(friends), c(2, 5))
  expect_equivalent(friends$marker, c("tag3", "tag5"))
  expect_equivalent(friends$friend, c("coll4", "coll5"))
  expect_equivalent(friends$marker.index, c(3, 5))
  expect_equivalent(friends$friend.index, c(4, 5))
  expect_equivalent(friends$marker, rownames(attention)[friends$marker.index])
  expect_equivalent(friends$friend, colnames(attention)[friends$friend.index])
  expect_equivalent(friends$friend.rank, c(1, 1))
})

# best.friends method is not illustrated well using square diagonal matrices
# we will use a rectangular matrix for this test (ncolls << ntags)
set.seed(1) # actually, it works with like 9/10 of seeds
ntags <- 100
ncolls <- 10
almost_diagon_mat <- matrix(1 + 9 * runif(ntags * ncolls), nrow = ntags)
almost_diagon_mat[1:ncolls, ] <- runif(ncolls * ncolls)
diag(almost_diagon_mat) <- 19
rownames(almost_diagon_mat) <- paste0("tag", 1:ntags)
colnames(almost_diagon_mat) <- paste0("coll", 1:ncolls)
probe <- data.frame(
  marker = paste0(c("tag"), 1:ncolls),
  friend = paste0(c("coll"), 1:ncolls),
  marker.index = 1:ncolls,
  friend.index = 1:ncolls,
  friend.rank = rep(1, ncolls)
)


test_that("passes non-diagonal diagonal test with low prior to have friend", {
  res <- friends.test.bic(almost_diagon_mat, .001)
  expect_equivalent(res, probe)
})

test_that("passes non-diagonal diagonal test with high prior to have friend", {
  res <- friends.test.bic(almost_diagon_mat, .5)
  expect_equivalent(dplyr::left_join(probe, res), probe)
  # we test that probe is contained in res
})
