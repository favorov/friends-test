test_that("Signal-noise test for ks", {
  set.seed(42)

  # generate a signal-noise matrix, as in custom code by Sasha S
  sig.noise.obj <- function(n = 80, k = 30, n0 = 10, p = 0.2,
                            rate_exp = 1 / 2, lambda_pois = 10) {
    M <- matrix(rexp(n * k, rate = rate_exp), nrow = n, ncol = k)
    selected_rows <- sample(n, size = n0)
    mask <- matrix(FALSE, nrow = n, ncol = k)
    mask[selected_rows, ] <- matrix(runif(n0 * k) < p, nrow = n0, ncol = k)
    M[mask] <- rpois(sum(mask), lambda = lambda_pois)
    res <- list(M = M, selected_rows = selected_rows, mask = mask)
    return(res)
  }

  # compare generated mask (signal) to estimated mask by Sasha S
  compute_error <- function(mask_true, mask_pred) {
    tp <- sum(mask_true & mask_pred) # True Positives
    fp <- sum(!mask_true & mask_pred) # False Positives
    fn <- sum(mask_true & !mask_pred) # False Negatives
    tn <- sum(!mask_true & !mask_pred) # True Negatives

    # Всего истинных положительных и отрицательных
    total_true <- tp + fn
    total_false <- fp + tn

    # Проценты
    tp_pct_true <- tp / total_true
    # сколько % от всех TRUE в mask_true нашли
    fp_pct_false <- fp / total_false
    # сколько % от всех FALSE в mask_true ошибочно пометили
    precision_pct <- if ((tp + fp) > 0) tp / (tp + fp) else NA
    # доля TP среди всех предсказанных положительных
    recall_pct <- tp / total_true
    # доля TP среди всех реальных положительных


    list(
      TP = tp_pct_true,
      FP = fp_pct_false,
      PR = precision_pct,
      RCL = recall_pct
    )
  }

  signoise <- sig.noise.obj()

  friends <- friends.test(signoise[["M"]])
  friends.mask <- (friends != 0)

  err <- compute_error(signoise[["mask"]], friends.mask)
  expect_true(err[["TP"]] > 0.1, #not a superresult
              info = "True Positive rate should be greater than 55%")
  expect_true(err[["PR"]] > 0.75,
              info = "Precision should be greater than 55%")
  expect_true(err[["RCL"]] > 0.1,
              info = "Recall should be greater than 55%")
  expect_true(err[["FP"]] < 0.001,
              info = "False Positive rate should be less than 1%")
})
