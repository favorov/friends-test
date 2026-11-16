#'
#' row_int_ranks
#'
#' Given the \eqn{|T| \times |C|} interaction matrix,
#' convert it to matrix of descending ranks of rows in
#' columns (columns).
#' See [friends_test] documentation for details.
#' @param A the original interaction matrix
#' @param distance_like if \code{TRUE}, ranks are ascending
#' (the smaller is the better). Default is \code{FALSE}.
#' @param neglect_diagonal if square matrix, and the diagonal
#' does not make sense, we \code{NA} it and our ranks are in
#' \eqn{[1 .. |T|-1]}. Default is \code{FALSE}.
#' @return a \eqn{|T| x |C|} matrix of integer ranks in
#' \eqn{[1 .. |T|]} (or, \eqn{[1 .. |T|-1]} if neglect_diagonal).
#' @examples
#' genes.no <- 100
#' TFs.no <- 50
#' gene.names <-
#'     paste0("gene_", as.character(seq(from = 1, to = genes.no)))
#' TF.names <-
#'     paste0("TF_", as.character(seq(from = 1, to = TFs.no)))
#' regulation <- matrix(rep(1, TFs.no * genes.no),
#'     ncol = TFs.no, byrow = FALSE
#' )
#' # we fill it with ones, now let's fill the regulation of gene 42
#' # by the first 1/2 TFs  with 2
#' regulation[42, seq(1, TFs.no / 2)] <- 2
#' rownames(regulation) <- gene.names
#' colnames(regulation) <- TF.names
#'
#' TF.ranks <- row_int_ranks(regulation)
#' @export
row_int_ranks <- function(
    A = NULL,
    distance_like = FALSE,
    neglect_diagonal = FALSE
) {
    dims <- dim(A)
    if (min(dims) < 2) {
        stop("The friends.test requires both dimensions of the
        A matrix to be more than 1")
    }
    if (neglect_diagonal) {
        if (dims[1] == dims[2]) {
            diag(A) <- NA
        } else {
            warning(
                "neglect_diagonal can work only for square interaction matrix"
            )
            neglect_diagonal <- FALSE
        }
    }

    order <- ifelse(distance_like, 1, -1)
    # if the interaction strength is distance_like, we order in ascending
    # if nor, descending.
    # E.g., the least ranks are the
    # most close interactions
    # if distance_like holds, the least is the best (first)
    # and order==1 (ascending)
    ranks.of.rows <-
        apply(
            A, 2,
            function(x) {
                data.table::frankv(x,
                    ties.method = "random",
                    na.last = TRUE, order = order
                )
            }
        )
    # we applied ranking column-by-column (column-by-column)
    rownames(ranks.of.rows) <- rownames(A)
    colnames(ranks.of.rows) <- colnames(A)
    # we reapply NA to the diagonal -- it will be used not to see
    # at in the C++ u statistics calculation
    # it also signals C++ that there are |C|-1 values rather that |C|
    # there a no other source on NA's in ranks.of.rows
    if (neglect_diagonal) {
        diag(ranks.of.rows) <- NA
    }
    ranks.of.rows
}
