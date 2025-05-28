# ------------------------------------------------------------------
# Функция для поиска лучшей точки разбиения в векторе рангов
# ------------------------------------------------------------------
# rank_vec   — числовой вектор рангов (длина k)
# max_rank   — максимальный возможный ранг (целое > 1)
# ------------------------------------------------------------------


rank_matrix <- function(mat) {
  # mat — числовая матрица (n × k)
  # apply по столбцам (MARGIN = 2)
  # rank(-x) сделает так, что наибольшее значение получит ранг 1
  # ties.method = "random" разыгрывает равные значения случайно
  apply(mat, 2, function(col) rank(-col, ties.method = "random"))
}


compute_best_split <- function(rank_vec, max_rank, q) {
  # 1) Подготовка
  k <- length(rank_vec)
  idx_sorted <- order(rank_vec)       # индексы в порядке возрастания рангов
  ranks_sorted <- rank_vec[idx_sorted]
  
  # векторы под накопление
  logL <- numeric(max_rank)           # сюда будем складывать log-правдоподобие
  count_le <- integer(max_rank)       # сколько элементов <= порога
  
  # 2) Основной цикл по порогу threshold = 1 .. max_rank-1
  for (threshold in seq_len(max_rank - 1)) {
    # сколько рангов не превосходит текущий threshold
    n_left <- sum(ranks_sorted <= threshold)
    count_le[threshold] <- n_left
    
    p_left  <- n_left / k              # доля «малых» элементов
    p_right <- 1 - p_left              # доля «больших»
    
    # вклад левой части
    L_left  <- if (n_left > 0)
      n_left * log(p_left  / threshold)
    else 0
    
    # вклад правой части
    L_right <- if (n_left < k)
      (k - n_left) * log(p_right / (max_rank - threshold))
    else 0
    
    logL[threshold] <- L_left + L_right
  }
  
  # 3) Граничный случай threshold = max_rank
  logL[max_rank]   <- k * log(1 / max_rank)
  count_le[max_rank] <- k
  
  # 4) Оставляем только допустимые пороги
  
  valid <- which(count_le > 0 & count_le < k)
 
  if (length(valid) == 0){
    list(
      tag = 'no'
    )
  }else if(length(valid)> 0){
    best_thresh <- valid[which.max(logL[valid])]
    v1 = max(logL[valid]) 
    v2 = v1
    if (v1 == v2){
      n_left    <- count_le[best_thresh]
      left_idx  <- idx_sorted[seq_len(n_left)]
      right_idx <- idx_sorted[seq_len(k)[-(seq_len(n_left))]]
      
      list(
        tag = 'yes',
        best_threshold   = best_thresh,   # оптимальный threshold
        left_indices     = left_idx,      # индексы rank_vec в «левой» группе
        right_indices    = right_idx,     # индексы rank_vec в «правой» группе
        log_likelihoods  = logL,          # весь вектор logL для анализа
        counts_at_split  = count_le       # сколько элементов <= каждому threshold
      )
    } else {
      list(
        tag = 'no'
      )
    }
  }
}


compute_best_split_bayes <- function(rank_vec, max_rank, q) {
  # 1) Подготовка
  k <- length(rank_vec)
  idx_sorted <- order(rank_vec)       # индексы в порядке возрастания рангов
  ranks_sorted <- rank_vec[idx_sorted]
  
  # векторы под накопление
  logL <- numeric(max_rank)           # сюда будем складывать log-правдоподобие
  count_le <- integer(max_rank)       # сколько элементов <= порога
  
  # 2) Основной цикл по порогу threshold = 1 .. max_rank-1
  for (threshold in seq_len(max_rank - 1)) {
    # сколько рангов не превосходит текущий threshold
    n_left <- sum(ranks_sorted <= threshold)
    count_le[threshold] <- n_left
    
    p_left  <- n_left / k              # доля «малых» элементов
    p_right <- 1 - p_left              # доля «больших»
    
    # вклад левой части
    L_left  <- if (n_left > 0)
      n_left * log(p_left  / threshold)
    else 0
    
    # вклад правой части
    L_right <- if (n_left < k)
      (k - n_left) * log(p_right / (max_rank - threshold))
    else 0
    
    logL[threshold] <- L_left + L_right
  }
  
  # 3) Граничный случай threshold = max_rank
  logL[max_rank]   <- k * log(1 / max_rank)
  count_le[max_rank] <- k
  
  # 4) Оставляем только допустимые пороги
  
  valid <- which(count_le > 0 & count_le < k)
  
  if (length(valid) == 0){
    list(
      tag = 'no'
    )
  }else if(length(valid)> 0){
    best_thresh <- valid[which.max(logL[valid])]
    v1 = max(logL[valid]) + log(q) - log(k)
    v2 = logL[max_rank] + log(1-q)
    if (v1 > v2){
      n_left    <- count_le[best_thresh]
      left_idx  <- idx_sorted[seq_len(n_left)]
      right_idx <- idx_sorted[seq_len(k)[-(seq_len(n_left))]]
      
      list(
        tag = 'yes',
        best_threshold   = best_thresh,   # оптимальный threshold
        left_indices     = left_idx,      # индексы rank_vec в «левой» группе
        right_indices    = right_idx,     # индексы rank_vec в «правой» группе
        log_likelihoods  = logL,          # весь вектор logL для анализа
        counts_at_split  = count_le       # сколько элементов <= каждому threshold
      )
    } else {
      list(
        tag = 'no'
      )
    }
  }
}




compute_best_split_bayes2 <- function(rank_vec, q, logK = TRUE) {
  # 1) Подготовка
  k <- length(rank_vec)
  idx_sorted <- order(rank_vec)       # индексы в порядке возрастания рангов
  ranks_sorted <- rank_vec[idx_sorted]
  
  max_rank = max(ranks_sorted )
  min_rank = min(ranks_sorted )
  # векторы под накопление
  logL <- numeric(max_rank)           # сюда будем складывать log-правдоподобие
  count_le <- integer(max_rank)       # сколько элементов <= порога
  
  # 2) Основной цикл по порогу threshold = 1 .. max_rank-1
  for (threshold in min_rank:(max_rank - 1)) {
    # сколько рангов не превосходит текущий threshold
    n_left <- sum(ranks_sorted <= threshold)
    count_le[threshold] <- n_left
    
    p_left  <- n_left / k              # доля «малых» элементов
    p_right <- 1 - p_left              # доля «больших»
    
    # вклад левой части
    L_left  <- if (n_left > 0)
      n_left * log(p_left  / (threshold - min_rank + 1) )
    else 0
    
    # вклад правой части
    L_right <- if (n_left < k)
      (k - n_left) * log(p_right / (max_rank - threshold))
    else 0
    
    logL[threshold] <- L_left + L_right
  }
  
  # 3) Граничный случай threshold = max_rank
  logL[max_rank]   <- k * log(1 / (max_rank - min_rank +1) )
  count_le[max_rank] <- k
  
  # 4) Оставляем только допустимые пороги
  
  valid <- which(count_le > 0 & count_le < k)
  
  if (length(valid) == 0){
    list(
      tag = 'no'
    )
  }else if(length(valid)> 0){
    best_thresh <- valid[which.max(logL[valid])]
    if (logK == TRUE){
      v1 = max(logL[valid]) + log(q) - log(k)
    }else if (logK==FALSE){
      v1 = max(logL[valid]) + log(q)
    }
    
    v2 = logL[max_rank] + log(1-q)
    if (v1 > v2){
      n_left    <- count_le[best_thresh]
      left_idx  <- idx_sorted[seq_len(n_left)]
      right_idx <- idx_sorted[seq_len(k)[-(seq_len(n_left))]]
      
      list(
        tag = 'yes',
        best_threshold   = best_thresh,   # оптимальный threshold
        left_indices     = left_idx,      # индексы rank_vec в «левой» группе
        right_indices    = right_idx,     # индексы rank_vec в «правой» группе
        log_likelihoods  = logL,          # весь вектор logL для анализа
        counts_at_split  = count_le       # сколько элементов <= каждому threshold
      )
    } else {
      list(
        tag = 'no'
      )
    }
  }
}


compute_best_split_bayes3 <- function(rank_vec, max_rank, q, logK = TRUE) {
  # 1) Подготовка
  k <- length(rank_vec)
  idx_sorted <- order(rank_vec)       # индексы в порядке возрастания рангов
  ranks_sorted <- rank_vec[idx_sorted]
  
  min_rank = min(ranks_sorted )
  # векторы под накопление
  logL <- numeric(max_rank)           # сюда будем складывать log-правдоподобие
  count_le <- integer(max_rank)       # сколько элементов <= порога
  
  # 2) Основной цикл по порогу threshold = 1 .. max_rank-1
  for (threshold in min_rank:(max_rank - 1)) {
    # сколько рангов не превосходит текущий threshold
    n_left <- sum(ranks_sorted <= threshold)
    count_le[threshold] <- n_left
    
    p_left  <- n_left / k              # доля «малых» элементов
    p_right <- 1 - p_left              # доля «больших»
    
    # вклад левой части
    L_left  <- if (n_left > 0)
      n_left * log(p_left  / threshold )
    else 0
    
    # вклад правой части
    L_right <- if (n_left < k)
      (k - n_left) * log(p_right / (max_rank - threshold))
    else 0
    
    logL[threshold] <- L_left + L_right
  }
  
  # 3) Граничный случай threshold = max_rank
  logL[max_rank]   <- k * log(1 / (max_rank - min_rank +1) )
  count_le[max_rank] <- k
  
  # 4) Оставляем только допустимые пороги
  
  valid <- which(count_le > 0 & count_le < k)
  
  if (length(valid) == 0){
    list(
      tag = 'no'
    )
  }else if(length(valid)> 0){
    best_thresh <- valid[which.max(logL[valid])]
    if (logK == TRUE){
      v1 = max(logL[valid]) + log(q) - log(k)
    }else if (logK==FALSE){
      v1 = max(logL[valid]) + log(q)
    }
    
    v2 = logL[max_rank] + log(1-q)
    if (v1 > v2){
      n_left    <- count_le[best_thresh]
      left_idx  <- idx_sorted[seq_len(n_left)]
      right_idx <- idx_sorted[seq_len(k)[-(seq_len(n_left))]]
      
      list(
        tag = 'yes',
        best_threshold   = best_thresh,   # оптимальный threshold
        left_indices     = left_idx,      # индексы rank_vec в «левой» группе
        right_indices    = right_idx,     # индексы rank_vec в «правой» группе
        log_likelihoods  = logL,          # весь вектор logL для анализа
        counts_at_split  = count_le       # сколько элементов <= каждому threshold
      )
    } else {
      list(
        tag = 'no'
      )
    }
  }
}




discrete_ks_test <- function(ranks, n_boot = 10000, seed = NULL) {
  # ranks — вектор наблюдаемых рангов (дискретные значения на grid)
  # grid  — вектор всех возможных значений (дискретная сетка)
  # n_boot — число бутстрэп-репликаций для оценки p-значения
  # seed   — для воспроизводимости
  
  left_end <- min(ranks)
  right_end <- max(ranks)
  grid <- left_end:right_end
  
  F0 <- ecdf(grid)
  
  # Проведём одновыборочный К–С тест:
  #   x      — ваши наблюдения
  #   F0     — функция CDF нулевой гипотезы
  #   simulate.p.value=TRUE — считать p-значение бутстрэпом
  #   B      — число репликаций
  res <- ks.test(ranks, F0,
                 simulate.p.value = TRUE,
                 B = n_boot)
  return(res)
}



find_mask <- function(mat, cells){
  all_rows <- rownames(mat)
  all_cols <- colnames(mat)
  
  # создаём матрицу FALSE того же размера
  mask <- matrix(FALSE,
                 nrow = length(all_rows),
                 ncol = length(all_cols),
                 dimnames = list(all_rows, all_cols))
  
  # находим числовые индексы
  ri <- match(cells$row, all_rows)
  ci <- match(cells$col, all_cols)
  
  # ставим TRUE
  mask[cbind(ri, ci)] <- TRUE
  return(mask)
} 


compare_masks <- function(mask_true, mask_pred) {
  # Проверки
  if (!is.matrix(mask_true) || !is.matrix(mask_pred)) {
    stop("Обе маски должны быть матрицами")
  }
  if (!all(dim(mask_true) == dim(mask_pred))) {
    stop("Размерности масок должны совпадать")
  }
  
  total <- sum(mask_true)
  # Индексы
  found_pos  <- which(mask_true  & mask_pred,  arr.ind = TRUE)
  missed_pos <- which(mask_true  & !mask_pred, arr.ind = TRUE)
  false_pos  <- which(!mask_true & mask_pred,  arr.ind = TRUE)
  
  # Количества
  found_cnt  <- nrow(found_pos)
  missed_cnt <- nrow(missed_pos)
  false_cnt  <- nrow(false_pos)
  
  
  list(
    found    = found_cnt,
    missedt   = missed_cnt,
    false    = false_cnt,
    total   = sum(mask_true)
  )
  
  # # Печать результатов
  # cat(sprintf("Found (TRUE & TRUE): %d\n", found_cnt))
  # cat(sprintf("Missed (TRUE & FALSE): %d\n", missed_cnt))
  # cat(sprintf("False positives (FALSE & TRUE): %d\n", false_cnt))
}



compute_error <- function(mask_true, mask_pred){
  tp <- sum(mask_true  & mask_pred)    # True Positives
  fp <- sum(!mask_true & mask_pred)    # False Positives
  fn <- sum(mask_true  & !mask_pred)   # False Negatives
  tn <- sum(!mask_true & !mask_pred)   # True Negatives
  
  # Всего истинных положительных и отрицательных
  total_true  <- tp + fn
  total_false <- fp + tn
  
  # Проценты
  tp_pct_true   <- tp / total_true  # сколько % от всех TRUE в mask_true нашли
  fp_pct_false  <- fp / total_false # сколько % от всех FALSE в mask_true ошибочно пометили
  precision_pct <- if ((tp + fp) > 0) tp / (tp + fp) else NA  # доля TP среди всех предсказанных положительных
  recall_pct    <- tp / total_true  # доля TP среди всех реальных положительных
  
  
  list(
    TP = tp_pct_true,
    FP = fp_pct_false,
    PR = precision_pct,
    RCL= recall_pct
  )
  
}


# alpha = 0.05
# chi2_results <- t(
#   apply(R, 1, function(x) {
#     tst   <- suppressWarnings(discrete_ks_test(x, n_boot = 100))
#     c(p.value   = tst$p.value)
#   })
# )
# 
# chi2_df <- data.frame(
#   row       = rownames(R),
#   chi2_p    = as.vector(chi2_results),
#   row.names = NULL
# )