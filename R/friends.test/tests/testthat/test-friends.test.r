test_that("no errors in simplest case", {
    mat <- diag(nrow = 5, ncol = 5)
    rownames(mat) <- paste0("row", 1:5)
    colnames(mat) <- paste0("coll", 1:5)
    expect_no_error(friends.test(mat))
})

test_that("best friend is determined correctly", {
    text <- "    coll1     coll2     coll3      coll4     coll5
            row1 0.1765568 0.7176185 0.2121425 0.01339033 0.5995658
            row2 0.6870228 0.9919061 0.6516738 0.38238796 0.4935413
            row3 0.3841037 0.3800352 0.1255551 0.86969085 0.1862176
            row4 0.7698414 0.7774452 0.2672207 0.34034900 0.8273733
            row5 0.0000000 0.0000000 0.0000000 0.00000000 1.0000000"
    attention <- as.matrix(read.table(text = text, header = TRUE))

    friends <- friends.test(attention)
    expect_equivalent(dim(friends), dim(attention))
    expected <- sparseMatrix(
        i = integer(0),
        j = integer(0),
        x = numeric(0),
        repr = "T",
        dims = dim(attention),
        dimnames = list(
            marker = rownames(attention),
            friend = colnames(attention)
        )
    )
    expected[5, 5] <- 1
    expect_equivalent(friends, expected)
})


test_that("passes non-diagonal diagonal test", {
    # best.friends method is not illustrated well using square diagonal matrices
    # we will use a rectangular matrix for this test (ncolls << nrows)
    set.seed(1) # actually, it works with like 9/10 of seeds
    nrows <- 100
    ncolls <- 10
    almost_diagon_mat <- matrix(1 + 9 * runif(nrows * ncolls), nrow = nrows)
    almost_diagon_mat[1:ncolls, ] <- runif(ncolls * ncolls)
    diag(almost_diagon_mat) <- 19
    rownames(almost_diagon_mat) <- paste0("row", 1:nrows)
    colnames(almost_diagon_mat) <- paste0("coll", 1:ncolls)
    friends <- friends.test(almost_diagon_mat)
    expect_equivalent(dim(friends), dim(almost_diagon_mat))
    expected <- sparseMatrix(
        i = integer(0),
        j = integer(0),
        x = numeric(0),
        repr = "T",
        dims = dim(almost_diagon_mat),
        dimnames = list(
            marker = rownames(almost_diagon_mat),
            friend = colnames(almost_diagon_mat)
        )
    )
    expected[cbind(1:10, 1:10)] <- 1
    expect_equivalent(friends, expected)
})
