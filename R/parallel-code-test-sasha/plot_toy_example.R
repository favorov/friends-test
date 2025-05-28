library(pheatmap)
library(RColorBrewer)

# Параметры
set.seed(123)
n   <- 200       # число строк
k   <- 20       # число столбцов
n0  <- 15       # сколько строк «портим»
p   <- 0.1      # вероятность замены
rate_exp    <- 5    # rate для Exp
lambda_pois <- 10   # λ для Pois

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

# 4) Рисуем оба графика на одной панели (1 строка × 2 столбца)
# -------------------------------------------------------------


# 4.1 Палитра для gradient-heatmap
grad_cols <- colorRampPalette(c("grey80","grey50","grey20","green4","green1"))(256)

# Настройка окна: 1 ряд, 2 колонки, отступы
par(mfrow = c(1, 2), mar = c(4,4,3,2))

# График 1: градиентный heatmap
par(mfrow = c(1, 2),      # 1 строка, 2 столбца
    mar   = c(4,4,3,2))    # отступы: снизу, слева, сверху, справа

# ——————————————————————————————
# График 1: градиентный “heatmap” через image()
# переворачиваем по Y, чтобы строка 1 сверху
image(x   = 1:k,
      y   = 1:n,
      z   = t(M[n:1, ]),
      col = grad_cols,
      xaxt= "n", yaxt= "n",
      main= "Значения: серый→зелёный",
      xlab= "Столбцы", ylab= "Строки")
axis(1, at = 1:k, labels = 1:k)
axis(2, at = 1:n, labels = n:1)

# ——————————————————————————————
# График 2: категориальная карта Exp vs Pois
cat_mat <- matrix(1L, nrow = n, ncol = k)
cat_mat[mask] <- 2L

image(x   = 1:k,
      y   = 1:n,
      z   = t(cat_mat[n:1, ]),
      col = c("grey80","green3"),
      xaxt= "n", yaxt= "n",
      main= "Exp (серым) / Pois (зелёным)",
      xlab= "Столбцы", ylab= "Строки")
axis(1, at = 1:k, labels = 1:k)
axis(2, at = 1:n, labels = n:1)
legend("topright",
       legend = c("Exp","Pois"),
       fill   = c("grey80","green3"),
       bty    = "n")
par(mfrow = c(1,1))
