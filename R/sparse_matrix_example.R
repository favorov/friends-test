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

add_row <- c(0,0,1,0,0)
sparse_mat <- rbind(sparse_mat, add_row)

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

# Create an empty sparse matrix of known size
cat("\n=== Empty sparse matrix (10 rows x 8 columns) ===\n")
empty_sparse <- sparseMatrix(i = integer(0),
                             j = integer(0),
                             x = numeric(0),
                             repr = "R",
                             dims = c(10, 8))

# Name rows as r_a...r_j and columns as c_a...c_h
rownames(empty_sparse) <- paste0("r_", letters[1:10])
colnames(empty_sparse) <- paste0("c_", letters[1:8])

print(empty_sparse)
cat("\nDimensions:", dim(empty_sparse), "\n")
cat("Number of non-zero elements:", length(empty_sparse@x), "\n")

my_i <- c(1,2,2,7)
my_j <- c(5,3,8,1)
my_x <- c(11,12,13,14)

# Set elements at (my_i, my_j) coordinates to my_x values in one command
empty_sparse[cbind(my_i, my_j)] <- my_x

cat("\n=== After setting values ===\n")
print(empty_sparse)