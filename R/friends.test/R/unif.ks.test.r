#'
#' unif.ks.test
#' 
#' returns the Kolomogorov-Smirnov p-value for uniformity of the values.
#' The values are the ranks of the same row in different columns.
#'
#' See [friends.test] documentation for details.
#' 
#' @param ranks vector of ranks of a tag in different collections, \eqn{1 .. N})
#' @param uniform.max the maximal rank in the uniform, default is NA which means
#' that we take the max(ranks) as the maximal value
#' @param simulate.p.value K-S by Monte-Carlo if \code{TRUE}; 
#' default is \code{FALSE}, see [stats::ks.test()]
#' @param B number of or replicates if \code{simulate.p.value=TRUE}
#' default is 2000, see [stats::ks.test()]
#' @return p-value for the K-S test comparing the ranks distribution with uniform
#' @importFrom stats ks.test
#' @examples
#' example(tag.int.ranks)
#' ks.p.vals<-apply(TF.ranks,1,"unif.ks.test")
#' @export
unif.ks.test<-function(ranks,
                       uniform.max=NA,
                       simulate.p.value=FALSE,
                       B=2000){
  jranks<-jitter(ranks,amount=0.1E-6)
  jrmin <- min(jranks)
  if(is.na(uniform.max)){
    jrmax <- max(jranks)
  } else {
    jrmax<-uniform.max
  }
  jranks_mapped <- (jranks-jrmin)/(jrmax-jrmin)
  res<-ks.test(jranks_mapped,"punif")
  res$p.value

  ranks<-jitter(ranks,amount=0.1E-6)
  left_end <- min(ranks)
  if(is.na(uniform.max)){
    right_end <- max(jranks)
  } else {
    right_end<-uniform.max
  }

  res <- ks.test(ranks, "punif",min=left_end,max=right_end,
                 simulate.p.value = simulate.p.value,
                 B = B)
  return(res$p.value)
}
