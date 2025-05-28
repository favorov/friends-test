#Plot singal noise ratio

mat <- M

signal_vals <- mat[mask]
noise_vals  <- mat[!mask]

# 2. Определяем общий диапазон и количество бинов
rng    <- range(mat, na.rm = TRUE)
breaks <- pretty(rng, n = 30)

# 3. Строим первую гистограмму (сигнал), с нормировкой на плотность
hist(signal_vals,
     breaks = breaks,
     freq   = FALSE,              # рисуем плотность, а не счётчики
     xlim   = rng,
     ylim   = c(0, max(density(signal_vals)$y, density(noise_vals)$y)),
     main   = "Гистограммы плотности: сигнал vs шум",
     xlab   = "Значения",
     col    = rgb(1, 0, 0, 0.4),  # полупрозрачный красный
     border = "white")

# 4. Добавляем гистограмму шума
hist(noise_vals,
     breaks = breaks,
     freq   = FALSE,
     add    = TRUE,
     col    = rgb(0, 0, 1, 0.4),  # полупрозрачный синий
     border = "white")

# 5. Легенда
legend("topright",
       legend = c("Сигнал", "Шум"),
       fill   = c(rgb(1, 0, 0, 0.4), rgb(0, 0, 1, 0.4)),
       border = "white")

# library(ggplot2)
# 
# # Собираем датафрейм
# df <- data.frame(
#   value = c(signal_vals, noise_vals),
#   type  = factor(rep(c("signal", "noise"),
#                      c(length(signal_vals), length(noise_vals))))
# )
# 
# # Рисуем гистограммы с плотностью
# ggplot(df, aes(x = value, fill = type)) +
#   geom_histogram(aes(y = ..density..),
#                  position = "identity",
#                  alpha    = 0.4,
#                  bins     = 30) +
#   labs(title = "Гистограммы плотности: сигнал vs шум",
#        x     = "Значения",
#        y     = "Плотность") +
#   theme_minimal()





grad_cols <- colorRampPalette(c("white","grey80","grey70","yellow3","yellow1"))(256)
image(1:ncol(M), 1:nrow(M), t(R[nrow(M):1, ]),
      col   = grad_cols,
      xaxt  = "n", yaxt = "n",
      main  = "Значения: серый→зелёный",
      xlab  = "Столбцы", ylab = "Строки")
axis(1, at = 1:ncol(M), labels = colnames(M))
axis(2, at = 1:nrow(M), labels = rev(rownames(M)))


# 3. Выбираем только «TRUE» в маске
sel <- as.vector(mask)
xs <- rep(1:ncol(R), each = nrow(R))
ys <- rep(nrow(R):1,   times = ncol(R))
# 4. Накладываем треугольники (pch = 2) на signal
points(xs[sel], ys[sel],
       pch = 2,        # треугольник
       col = "blue",  # цвет, можно изменить
       lwd = 2,
       cex = 1)        # размер символа

points(xpos, ypos_plot, pch = 21, col = "green", lwd = 2, cex = 1.5)

