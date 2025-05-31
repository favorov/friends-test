#' 
#' step.fit.ln.likelihoods
#' 
#' fits possible bi-uniform step models for a set of descending 
#' (descending in each collection) ranks of the same tag in different collections
#' (the \code{ranks} parameter)
#' The input ranks are integers in \eqn{1..max.possible.rank}. 
#' Each of the split rank values \eqn{1 .. max.possible.rank-1} split all the ranks 
#' into two steps: "this or less" and "greater than this". 
#'
#' The function fits the step (bi-uniform) model for each
#' integer splitting value in \eqn{1..max.possible.rank-1}; 
#' the splitting value is in the maximal value in the left part 
#' of the ordering (the original ranks are descending, so best 
#' collections are in the left part) and calculates the likelihood. 
#' The last value with the index \eqn{max.possible.rank} is calculated for a
#' non-step uniform model.

#' See [friends.test] documentation for details.
#'
#' @param ranks vector of ranks of a tag in different collections
#' @param max.possible.rank number of tags, i.e. maximal rank 
#' @return a list of three values: \cr
#' \code{collectons.order} is the order of ranks in, collection-by-collection\cr
#' \code{ln.likelihoods} the ln of the likelihood of each of models corresponding 
#' to each split rank value in \eqn{1..max.possible.rank-1} and the last, correspond to just uniform\cr
#' \code{k1.by.l1} contains \eqn{k_1}, that is the number of ranks on the 
#' on left of the step, including the split value, for split values \eqn{1..max.possible.rank};\cr
#' @examples
#' example(tag.int.ranks)
#' steps<-step.fit.ln.likelihoods(TF.ranks[42,],genes.no)
#' @export
step.fit.ln.likelihoods<-function(ranks,max.possible.rank){
  if(max.possible.rank<max(ranks)){
    stop('Tags_no parameter is the maximal possible rank, it cannot be less then max(ranks)!')
  }
  if(!all(ranks-floor(ranks)==0)){
    stop("Ranks are to be integer!")
  }
  if(!all(ranks>=1)){
    stop("Ranks are to be integer!")
  }
  if(!is.null(dim(ranks))){
    warning("Ranks has not-NULL dim(), it is not a vector.\n")
  }
  
  
  collectons.order<-order(ranks)
  ranks<-ranks[collectons.order]
  ln.likelihoods<-rep(0,max.possible.rank)
  k1.by.l1<-rep(0,max.possible.rank)
  k<-length(ranks)
  k1<-0
  #l1==max.possible.rank is "no step"
  for (l1 in seq_len(max.possible.rank-1)){ #1:(max.possible.rank-1)
    #we enumerate models by their l_i parameter
    while (k1<k && ranks[k1+1]<=l1)
    {
      k1<-k1+1      
    }#l1 has hit next rank value
    k1.by.l1[l1]<-k1
    p1<-k1/k
    if(p1>0){
      ln.likelihoods[l1]<-
        ln.likelihoods[l1]+k1*log(p1/l1)
    }
    if(p1<1){
      ln.likelihoods[l1]<-
        ln.likelihoods[l1]+(k-k1)*log((1-p1)/(max.possible.rank-l1))
    }
  }
  ln.likelihoods[max.possible.rank]<-k*log(1/max.possible.rank)
  k1.by.l1[max.possible.rank]<-k
  
  list(
    collectons.order=collectons.order,
    ln.likelihoods=ln.likelihoods,
    k1.by.l1=k1.by.l1
  )
}
