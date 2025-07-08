#'
#' tag.int.ranks
#'
#' Given the (\code{attention}) \eqn{|T| \times |C|} matrix,
#' convert it to matrix of descending ranks of tags(rows) in
#' collections (columns).
#' See [friends.test] documentation for details.
#' @param attention original attention matrix
#' @param distance_like if \code{TRUE}, ranks are ascending
#' (the smaller is the better). Default is \code{FALSE}.
#' @param neglect_diagonal if square matrix, and the diagonal
#' does not make sense, we \code{NA} it and our ranks are in
#' \eqn{[1 .. |T|-1]}. Default is \code{FALSE}.
#' @return a \eqn{|T| x |C|} matrix of integer ranks in
#' \eqn{[1 .. |T|]} (or, \eqn{[1 .. |T|-1]} if neglect_diagonal).
#' @examples
#' genes.no<-100
#' TFs.no<-50
#' gene.names<-
#'     paste0("gene_",as.character(seq( from = 1, to = genes.no )))
#' TF.names<-
#'     paste0("TF_",as.character(seq( from = 1, to = TFs.no )))  
#' regulation<-matrix(rep(1,TFs.no*genes.no),
#'     ncol=TFs.no,byrow=FALSE
#' )
#' #we fill it with ones, now let's fill the regulation of gene 42
#' #by the first 1/2 TFs  with 2
#' regulation[42,seq(1,TFs.no/2)]=2
#' rownames(regulation)<-gene.names
#' colnames(regulation)<-TF.names
#'
#' TF.ranks<-tag.int.ranks(regulation)
#' @export
tag.int.ranks <- function(
    attention = NULL,
    distance_like = FALSE,
    neglect_diagonal = FALSE) {
  dims <- dim(attention)
  if (min(dims) < 2) {
    stop("The friends.test requires both dimentions of the
        A matrix to be more than 1")
}
if (neglect_diagonal) {
    if (dims[1] == dims[2]) {
      diag(attention) <- NA
    } else {
      warning("neglect_diagonal can work only for square attention matrix")
      neglect_diagonal <- FALSE
    }
}
   
    order<-ifelse(distance_like,1,-1)
    #if attention is distance_like, we will order in ascending
    #if nor, descending.
    #E.g., the least ranks are the
    #most close attentions
    # if distance_like holds, the least is the best (first)
    #and order==1 (ascending)
    ranks.of.tags<-apply(attention,2,
                         function(x){
                             data.table::frankv(x,ties.method='random',
                                                na.last=TRUE,order=order)
                         }
    )
    #we applied ranking column-by-column (collection-by-cloud)
    rownames(ranks.of.tags)<-rownames(attention)
    colnames(ranks.of.tags)<-colnames(attention)
    #we reapply NA to the diagonal -- it will be used not to see at in the C++ u statistics calculation
    #it also signals C++ that there are |C|-1 values rather that |C|
    #there a no other source on NA's in ranks.of.tags
    if (neglect_diagonal){diag(ranks.of.tags)<-NA}
    ranks.of.tags
}
 