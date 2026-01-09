# Sparse Matrix in Triplet Form Example
# This script creates a sparse matrix, adds rows, and displays row 3 nonzero elements

library(Matrix)

# Create initial sparse matrix in triplet form (i, j, x format)
# i = row indices, j = column indices, x = values
initial_rows <- c(1, 1, 2, 3, 3, 4)
initial_cols <- c(1, 3, 2, 1, 4, 3)
initial_vals <- c(5, 8, 2, 7, 9, 3)

# Create sparse matrix (6 rows x 5 columns)
sparse_mat <- sparseMatrix(i = initial_rows, 
                           j = initial_cols, 
                           x = initial_vals,
                           dims = c(6, 5))

cat("Initial sparse matrix:\n")
print(sparse_mat)
cat("\n")

# Add new rows 5 times
for (iteration in 1:5) {
  # Create new row data in triplet form
  new_row_index <- nrow(sparse_mat) + 1
  new_row_cols <- sample(1:ncol(sparse_mat), size = 2)  # Random column positions
  new_row_vals <- sample(1:10, size = 2)  # Random values
  
  # Create a new sparse matrix with the additional row
  all_rows <- c(initial_rows, rep(new_row_index, length(new_row_cols)))
  all_cols <- c(initial_cols, new_row_cols)
  all_vals <- c(initial_vals, new_row_vals)
  
  sparse_mat <- sparseMatrix(i = all_rows,
                             j = all_cols,
                             x = all_vals,
                             dims = c(new_row_index, ncol(sparse_mat)))
  
  # Update for next iteration
  initial_rows <- all_rows
  initial_cols <- all_cols
  initial_vals <- all_vals
  
  cat(sprintf("After adding row %d (iteration %d):\n", new_row_index, iteration))
  print(sparse_mat)
  cat("\n")
}

# Show all nonzero elements of row 3
cat("=== Nonzero elements of row 3 ===\n")
row_3 <- sparse_mat[3, ]
nonzero_indices <- which(row_3 != 0)
nonzero_values <- row_3[nonzero_indices]

cat(sprintf("Row 3 has %d nonzero element(s):\n", length(nonzero_indices)))
for (i in seq_along(nonzero_indices)) {
  cat(sprintf("  Column %d: %g\n", nonzero_indices[i], nonzero_values[i]))
}

# Alternative: Show as triplet form for row 3
cat("\nRow 3 in triplet form:\n")
row3_summary <- summary(sparse_mat[3, , drop = FALSE])
print(row3_summary)

# Show all rows that are not completely zeroes
cat("\n=== Rows that are not completely zeroes ===\n")
row_sums <- rowSums(sparse_mat != 0)  # Count nonzero elements per row
nonzero_rows <- which(row_sums > 0)

cat(sprintf("Found %d row(s) with nonzero elements:\n", length(nonzero_rows)))
for (row_idx in nonzero_rows) {
  nonzero_cols <- which(sparse_mat[row_idx, ] != 0)
  nonzero_vals <- sparse_mat[row_idx, nonzero_cols]
  cat(sprintf("\nRow %d (%d nonzero element(s)):\n", row_idx, length(nonzero_cols)))
  for (i in seq_along(nonzero_cols)) {
    cat(sprintf("  Column %d: %g\n", nonzero_cols[i], nonzero_vals[i]))
  }
}

