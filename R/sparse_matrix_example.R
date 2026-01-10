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

sparse_mat <- rbind(sparse_mat,sparse_mat)

cat("Rbind sparse matrix:\n")
print(sparse_mat)
cat("\n")

# Show all rows that are not completely zeroes
cat("\n=== Rows that are not completely zeroes ===\n")
row_sums <- rowSums(sparse_mat != 0)  # Count nonzero elements per row
nonzero_rows <- which(row_sums > 0)
print(nonzero_rows)

cat("\n=== Rows that are in summary ===\n")
print(unique(sort(sparse_mat@i+1)))

