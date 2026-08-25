test_that("best_step_fit returns expected values for known input", {
    ranks <- c(97, 1, 98, 99, 100)
    rows.no <- 100
    result <- best_step_fit(ranks, rows.no)
    expect_true(result$best.step.rank %in% seq(1, 96))
    # split into 1 and all others
    expect_equal(result$population.on.left, 1)
    expect_equal(result$columns.on.left, c(2))
    expect_equal(result$columns.on.right, c(1, 3, 4, 5))
})


test_that("best_step_fit and independent code equal, c(1,2,3,4,5,6,7,8)", {
    ranks <- c(1, 2, 3, 4, 5, 6, 7, 8)
    row.lim <- 21 # row.no
    col.order <- order(ranks)
    ranks <- ranks[col.order]
    l.lim <- 0:row.lim # limit of enumeration of l
    k <- length(ranks)
    lkl <- rep(-1000, row.lim) # likelihoods
    step.pos <- rep(0, row.lim) # no of ranks on left
    pos <- 1
    for (l in l.lim) {
        q <- which(ranks <= l)
        if (length(q) == 0) {
            # do nothig, print("trololo\n")
        } else if (length(q) < length(ranks)) {
            m <- max(q)
            p <- m / k
            lkl[pos] <- m * log(p / l) + (k - m) * log((1 - p) / (row.lim - l))
            step.pos[pos] <- length(q)
            pos <- pos + 1
        } else if (length(q) == length(ranks)) {
            break
        }
    }
    stp.pos <- min(which(lkl == max(lkl)))
    cols_no <- step.pos[stp.pos] # columns before jump
    bsf <- best_step_fit(ranks, row.lim) # best.friends
    expect_equal(bsf$population.on.left, cols_no)
})


test_that("best_step_fit and independent code equal, c(1,1,1,1,6,6,6,6,6,6)", {
    ranks <- c(1, 1, 1, 1, 6, 6, 6, 6, 6, 6)
    row.lim <- 21 # row.no
    col.order <- order(ranks)
    ranks <- ranks[col.order]
    l.lim <- 0:row.lim # limit of enumeration of l
    k <- length(ranks)
    lkl <- rep(-1000, row.lim) # likelihoods
    step.pos <- rep(0, row.lim) # no of ranks on left
    pos <- 1
    for (l in l.lim) {
        q <- which(ranks <= l)
        if (length(q) == 0) {
            # do nothig, print("trololo\n")
        } else if (length(q) < length(ranks)) {
            m <- max(q)
            p <- m / k
            lkl[pos] <- m * log(p / l) + (k - m) * log((1 - p) / (row.lim - l))
            step.pos[pos] <- length(q)
            pos <- pos + 1
        } else if (length(q) == length(ranks)) {
            break
        }
    }
    stp.pos <- min(which(lkl == max(lkl)))
    cols_no <- step.pos[stp.pos] # columns before jump
    bsf <- best_step_fit(ranks, row.lim) # best.friends
    expect_equal(bsf$population.on.left, cols_no)
})


test_that("a fully tied row yields no friends, whatever the prior", {
    tied <- rep(5L, 6)
    M <- 10

    # no valid step exists, so maximum likelihood has nothing to accept
    ml <- best_step_fit(tied, M)
    expect_identical(ml$population.on.left, 0L)
    expect_identical(ml$columns.on.left, integer(0L))

    # prior = 1 makes the step model win the comparison by default; there is
    # still no step to report, and this used to raise an error from seq_len()
    for (prior in c(1e-6, 0.5, 1)) {
        fit <- best_step_fit_bic(tied, M, prior)
        expect_identical(fit$population.on.left, 0L,
            info = paste("prior =", prior))
        expect_identical(fit$best.step.rank, M, info = paste("prior =", prior))
    }
})
