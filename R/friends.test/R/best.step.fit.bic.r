#' 
#' best.step.fit.bic
#' 
#' finds the ML-best step model  for one tag and 
#' compares the posteriors of the found best step
#' and non-step uniform model.
#' 
#' See [friends.test] documentation for details.
#'
#' @inheritParams step.fit.ln.likelihoods
#' @param prior.to.have.friends The prior for a tag is important enough to have friendly collections.
#' @return a list of four values: \cr
#' \code{step.models} is return from [step.fit.ln.likelihoods] call the function start with
#' \code{best.step.rank} is the rank value that makes the best step;
#' it is not obligatory one on the \code{ranks} value.\cr
#' \code{collections.on.left} is the vector of the collections on the left of the best step 
#' (including the step value). They are friends of the tag.\cr
#' \code{collections.on.right} is vector of those on the right \cr
#' \code{population.on.left} is how many ranks are on left of split; they are friends! \cr
#' if non-step uniform model wins and there are no friends,\cr 
#' then \code{best.step.rank==max.possible.rank}, \code{population.on.left==0},\cr
#' all collections are listed in \code{collections.on.right} and \code{collections.on.left} is empty
#' @examples
#' example(tag.int.ranks)
#' step<-best.step.fit.bic(TF.ranks[42,],genes.no,0.5)
#' nostep<-best.step.fit.bic(TF.ranks[42,],genes.no,1E-50)
#' @export
best.step.fit.bic<-function(ranks, max.possible.rank, prior.to.have.friends){
  step.models <- step.fit.ln.likelihoods(ranks,max.possible.rank)
 
  possible.step.ranks<-seq_len(max.possible.rank-1)
  k1.by.l1<-step.models$k1.by.l1[possible.step.ranks]
  possible.step.ranks<-possible.step.ranks[k1.by.l1>0 & k1.by.l1<length(ranks)]
  #we assess only the steps that have nonzero left and right sets
  
  #best.step.index<-which.max(step.models$ln.likelihoods[possible.step.ranks])
  #almost; we want the last value, so:
  possible.ln.likelihoods<-step.models$ln.likelihoods[possible.step.ranks]
  max.ln.l<-max(possible.ln.likelihoods)
  #here, we compare the best ln posterior 
  #with the uniform model posterior,
  #and here we use the prior.to.have.friends
  if (max.ln.l+
        log(prior.to.have.friends) >= 
      step.models$ln.likelihoods[max.possible.rank]+
        log(1-prior.to.have.friends)) {
    #if we are here, the step model won
    best.step.index<-max(which(possible.ln.likelihoods==max.ln.l))
    #the index in possible.step_ranks (and possible.ln.likelihoods);
    #we need the rank itself
    best.step.rank<-possible.step.ranks[best.step.index]
  
    population.on.left<-k1.by.l1[best.step.rank]

    collections.on.left<-
    step.models$collectons.order[seq_len(population.on.left)] 
    #1:population.on.left
    collections.on.right<-
      step.models$collectons.order[seq(population.on.left+1,length(ranks))] 
    #(population.on.left+1):k
  } else {
    #if we are here, the uniform won, no friends
    
    best.step.rank<-max.possible.rank
    
    population.on.left<-0 #all
    
    collections.on.left<-c() #empty -- no friends
      
    collections.on.right<-step.models$collectons.order
    #all
   }
  
  list(step.models=step.models,
       best.step.rank=best.step.rank,
       collections.on.left=collections.on.left,
       collections.on.right=collections.on.right,
       population.on.left=population.on.left
       )
}

