test_that("best.step.fit returns expected values for known input", {
  ranks <- c(97, 1, 98, 99, 100)
  rows.no <- 100
  result <- best.step.fit(ranks, rows.no)
  expect_true(result$best.step.rank %in% seq(1, 96))
  #split into 1 and all others
  expect_equal(result$population.on.left, 1)
  expect_equal(result$columns.on.left, c(2))
  expect_equal(result$columns.on.right, c(1, 3, 4, 5))
})


test_that("best.step.fit and independent code equal, c(1,2,3,4,5,6,7,8)", {
  ranks <- c(1, 2, 3, 4, 5, 6, 7, 8)
  row.lim <- 21 #row.no
  col.order <- order(ranks)
  ranks <- ranks[col.order]
  l.lim <- 0:row.lim #limit of enumeration of l
  k <- length(ranks)
  lkl <- rep(-1000, row.lim) #likelihoods
  step.pos <- rep(0, row.lim) #no of ranks on left
  pos <- 1
  for (l in l.lim){
    q <- which(ranks <= l)
    if (length(q) == 0) {
      #do nothig, print("trololo\n")
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
  cols_no <- step.pos[stp.pos] #columns before jump
  bsf <- best.step.fit(ranks, row.lim) #best.friends
  expect_equal(bsf$population.on.left, cols_no)
})


test_that("best.step.fit and independent code equal, c(1,1,1,1,6,6,6,6,6,6)", {
  ranks <- c(1, 1, 1, 1, 6, 6, 6, 6, 6, 6)
  row.lim <- 21 #row.no
  col.order <- order(ranks)
  ranks <- ranks[col.order]
  l.lim <- 0:row.lim #limit of enumeration of l
  k <- length(ranks)
  lkl <- rep(-1000, row.lim) #likelihoods
  step.pos <- rep(0, row.lim) #no of ranks on left
  pos <- 1
  for (l in l.lim){
    q <- which(ranks <= l)
    if (length(q) == 0) {
      #do nothig, print("trololo\n")
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
  cols_no <- step.pos[stp.pos] #columns before jump
  bsf <- best.step.fit(ranks, row.lim) #best.friends
  expect_equal(bsf$population.on.left, cols_no)
})
