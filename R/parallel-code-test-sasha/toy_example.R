# n   <- 400       # число строк
# k   <- 80       # число столбцов
# n0  <- 40       # сколько строк «портим»

n   <- 80       # число строк
k   <- 30       # число столбцов
n0  <- 10       # сколько строк «портим»
p   <- 0.2      # вероятность замены
rate_exp    <- 1/2    # rate для Exp
lambda_pois <- 10   # λ для Pois

#-------------------------

# stat <- c()
# false <- c()
# for (i in 1:100){



# 1) Генерация матрицы
M <- matrix(rexp(n * k, rate = rate_exp), nrow = n, ncol = k)

# 2) Отметка строк и маскирование
selected_rows <- sample(n, size = n0)
mask <- matrix(FALSE, nrow = n, ncol = k)
mask[selected_rows, ] <- matrix(runif(n0 * k) < p, nrow = n0, ncol = k)

# 3) Замена отмеченных ячеек на Poisson
M[mask] <- rpois(sum(mask), lambda = lambda_pois)

# Задаём имена столбцов: "col_1", "col_2", …, "col_k"
colnames(M) <- paste0("col_", seq_len(k))

# Задаём имена строк: "row_1", "row_2", …, "row_n"
rownames(M) <- paste0("row_", seq_len(n))

R <- rank_matrix(M)


#===================== uniformity test ==========================

# col_detected <- c()
# row_detected <- c()
# 
# alpha = .05
# 
# chi2_results <- t(
#   apply(R, 1, function(x) {
#     # категории – все целые между min(x) и max(x)
#     cats  <- seq(min(x), max(x))
#     # наблюдаемые частоты в этих категориях
#     obs   <- table(factor(x, levels = cats))
#     # равномерные теоретические вероятности
#     p_exp <- rep(1/length(cats), length(cats))
#     # сам χ²-тест
#     tst   <- suppressWarnings(chisq.test(x = obs, p = p_exp))
#     c(statistic = unname(tst$statistic),
#       p.value   = tst$p.value)
#   })
# )
# 
# chi2_df <- data.frame(
#   row       = rownames(R),
#   chi2_stat = chi2_results[ , "statistic"],
#   chi2_p    = chi2_results[ , "p.value"],
#   row.names = NULL
# )
# 
# 
# nonuniform_rows <- chi2_df$row[chi2_df$chi2_p <= alpha]
# idx1 <- as.integer(sub("^row_", "",nonuniform_rows))
# common <- intersect(idx1, selected_rows)
# print(common)
# R_filtered <- R[nonuniform_rows, , drop = FALSE]
# 
#  for (r in 1:nrow(R_filtered)){
#    out <- compute_best_split(rank_vec = R_filtered[r, ], max_rank = n, q = 0.3)
#    col_ind <- paste0("col_", out$left_indices)
#    if (out$tag == 'yes'){
#      row_ind <- rep(rownames(R_filtered)[r], length(col_ind))
#      if (length(row_ind)<k/2){
#        col_detected <- append(col_detected, col_ind)
#        row_detected <- append(row_detected, row_ind)
#      }
#    }
#  }

#===========================





#=========================== 1-n ======================

# col_detected <- c()
# row_detected <- c()
# 
# for (r in 1:nrow(R)){
#   out <- compute_best_split_bayes(rank_vec = R[r, ], max_rank = n, q = 0.1)
#   col_ind <- paste0("col_", out$left_indices)
#   if (out$tag == 'yes'){
#     if (length(col_ind) < k/2){
#       row_ind <- rep(rownames(R)[r], length(col_ind))
#       col_detected <- append(col_detected, col_ind)
#       row_detected <- append(row_detected, row_ind)
#     }
#   }
# }




#=========================== min-max ======================

# q = n0/n
# col_detected <- c()
# row_detected <- c()
# for (r in 1:nrow(R)){
#   out <- compute_best_split_bayes2(rank_vec = R[r, ], q = q, logK=TRUE)
#   col_ind <- paste0("col_", out$left_indices)
#   if (out$tag == 'yes'){
#     if (length(col_ind) < k/2){
#       row_ind <- rep(rownames(R)[r], length(col_ind))
#       col_detected <- append(col_detected, col_ind)
#       row_detected <- append(row_detected, row_ind)
#     }
#   }
# }

#======================



#=========================== min-n ======================

q = n0/n
col_detected <- c()
row_detected <- c()
for (r in 1:nrow(R)){
  out <- compute_best_split_bayes3(rank_vec = R[r, ], max_rank = n, q = q, logK=TRUE)
  col_ind <- paste0("col_", out$left_indices)
  if (out$tag == 'yes'){
    if (length(col_ind) < k/2){
      row_ind <- rep(rownames(R)[r], length(col_ind))
      col_detected <- append(col_detected, col_ind)
      row_detected <- append(row_detected, row_ind)
    }
  }
}

#======================





cells_to_mark <- data.frame(
  row = row_detected,
  col = col_detected,
  stringsAsFactors = FALSE
)


# Вычисляем числовые координаты в системе image():
# x = индекс столбца, y = "перевёрнутый" индекс строки
xpos <- match(cells_to_mark$col, colnames(M))
ypos <- match(cells_to_mark$row, rownames(M))
# в image() мы рисуем t(M[n:1,]), поэтому строка j идёт на y = n - j + 1
ypos_plot <- nrow(M) - ypos + 1

# # Рисуем оба графика
par(mfrow = c(1, 2), mar = c(4,4,3,2))

# 1) Градиентный heatmap через image()
grad_cols <- colorRampPalette(c("white","grey80","grey70","green3","green1"))(256)
image(1:ncol(M), 1:nrow(M), t(M[nrow(M):1, ]),
      col   = grad_cols,
      xaxt  = "n", yaxt = "n",
      main  = "Значения: серый→зелёный",
      xlab  = "Столбцы", ylab = "Строки")
axis(1, at = 1:ncol(M), labels = colnames(M))
axis(2, at = 1:nrow(M), labels = rev(rownames(M)))

# накладываем крестики
points(xpos, ypos_plot, pch = 21, col = "blue", lwd = 1, cex = 1.5)


# 2) Категориальная карта Exp vs Pois
cat_mat <- matrix(1L, nrow = nrow(M), ncol = ncol(M))
cat_mat[mask] <- 2L
image(1:ncol(M), 1:nrow(M), t(cat_mat[nrow(M):1, ]),
      col   = c("grey80","green3"),
      xaxt  = "n", yaxt = "n",
      main  = "Exp (серым) / Pois (зелёным)",
      xlab  = "Столбцы", ylab = "Строки")
axis(1, at = 1:ncol(M), labels = colnames(M))
axis(2, at = 1:nrow(M), labels = rev(rownames(M)))
legend("topright", legend = c("Exp","Pois"), fill = c("grey80","green3"), bty = "n")

# снова накладываем крестики
points(xpos, ypos_plot, pch = 21, col = "blue", lwd = 1, cex = 1.5)


mtext(
  "Bernoulli model p = .3",
  outer = TRUE,
  cex   = 1.5,
  font  = 2
)

# вернуть настройки
par(mfrow = c(1,1))


#===================== compute precision and recall ==================

mask_pred <- find_mask(mat = R, cells = cells_to_mark)
cmpr <- compare_masks(mask_true=mask, mask_pred =mask_pred) 

compute_error(mask_true=mask, mask_pred =mask_pred)




