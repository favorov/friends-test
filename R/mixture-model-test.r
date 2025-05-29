n_   <- 400     
# число строк 
k_   <- 50       
# число столбцов 
n0_  <- 10       
# сколько строк «портим» 
p_   <- 0.1      
# вероятность замены 
rate_exp_    <- 1/2    
# rate для Exp 
lambda_pois_ <- 100   
# λ для Pois  
#-------------------------  # 
#stat <- c() # 
#false <- c() # 
for (i in 1:1){    
  # 1) Генерация матрицы 
  M_ <- matrix(rexp(n_ * k_, rate = rate_exp_), nrow = n_, ncol = k_)  
  # 2) Отметка строк и маскирование 
  selected_rows_ <- sort(sample.int(n_, size = n0_))
  mask_ <- matrix(FALSE, nrow = n_, ncol = k_) 
  mask_[selected_rows_, ] <- 
    matrix(runif(n0_ * k_) < p_, nrow = n0_, ncol = k_, byrow=TRUE)  
  # 3) Замена отмеченных ячеек на Poisson 
  M_[mask_] <- rpois(sum(mask_), lambda = lambda_pois_)  
  # Задаём имена столбцов: "col_1", "col_2", …, "col_k" 
  colnames(M_) <- paste0("col_", seq_len(k_))  
  # Задаём имена строк: "row_1", "row_2", …, "row_n" 
  rownames(M_) <- paste0("row_", seq_len(n_))
}


  
#вот тут должно быть ок # 
#n   <- 400       # число строк # 
#k   <- 80       # число столбцов # 
#n0  <- 40       # сколько строк «портим»