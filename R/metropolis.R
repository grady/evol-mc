##' update population of chains
##'
##' This is a fairly general function. The main requirement is that a
##' vector of logicals with the same length as the population size
##' will correctly subset individuals from the state object using
##' `[`. This means that a population of states can be a vector, a
##' list, or a matrix (with individuals in rows). In particular,
##' data.frames do not work.
##'
##' The rprop function should accept as its first argument the current
##' state population object (whatever that may be) and return
##' population object of the same type and size with proposal variates
##' corresponding to the individuals in the current population.
##'
##' The dprop function is required only if the proposal distribution
##' is not reversible. It should take as its arguments two population
##' objects; the first provides the locations at which to evaluate the
##' densities, while the second is the population to consider as the
##' "current" state.
##' 
##' The target function should accept a population of states and
##' return a vector of density evaluations for each individual in the
##' population.
##' @param state current population of states.
##' @param rprop proposal generation function. See details.
##' @param dprop proposal density function, required if proposal is
##' not reversible. See details.
##' @param target target density function
##' @return the population of states at the next step of the chain.
##' @author Grady Weyenberg
mh <- function(state,rprop,dprop=NULL,target) {
  new <- rprop(state)
  a <- target(new) / target(state)
  if(is.function(dprop)) a <- a * dprop(state,new) / dprop(new,state)
  i <- a < 1.0 # if false, replace; if true, keep?
  i[i] <- runif(sum(i)) >= a[i] # keep with prob 1-a
  state[!i] <- new[!i]
  state
}


## mh.setup <- function(target,rprop,dprop=NULL) {
##   obj <- structure(list(),call=match.call(),class="mhobj")
##   obj$target <- match.fun(target)
##   obj$rprop <- match.fun(rprop)
##   if(!is.null(dprop)) obj$dprop <- match.fun(dprop)
##   obj
## }

## print.mhobj <- function(x, ...) {
##   x.call <- as.list(attr(x,"call"))
##   cat("Metropolis-Hastings sampler object:\n\n")
##   cat("Target density:", x.call$target, "\n")
##   cat("Proposal sampler:", x.call$rprop, "\n")
##   if(is.null(x.call$dprop)) {
##     cat("* Proposal density is assumed to be reversible *\n")
##   } else {
##     cat("Proposal density:", x.call$dprop,"\n")
##   }
##   invisible(x)
## }

## mh.step <- function(state,mhobj) {
##   target <- mhobj$target
##   rprop <- mhobj$rprop
##   dprop <- mhobj$dprop

##   new <- rprop(state)
##   a <- target(new) / target(state)
##   if(is.function(dprop)) a <- a * dprop(state,new) / dprop(new,state)
##   i <- a < 1.0 # if false, replace; if true, keep?
##   i[i] <- runif(sum(i)) >= a[i] # keep with prob 1-a
##   state[!i] <- new[!i]
##   state
## }

## mh.run <- function(mhobj, init, n){
##   if(!inherits(mhobj,"mhobj")) stop("mjobj should be an object created using mh.new()")
##   n <- n + 1L
##   i <- 1L
##   chain <- structure(vector('list', n), class="chain")
##   chain[[1L]] <- init
##   while((i <- i + 1L) <= n){
##     chain[[i]] <- mh.step(chain[[i-1L]], mhobj)
##   }
##   chain
## }

## plot.chain <- function(x,y,...) {
##   asa
## }


## rprop <- function(state) state + runif(length(state),-1,1)
## mh(rep(0,5),rprop,target=dnorm)
## x <- simplify2array(Reduce(function(state,x)mh(state,rprop,target=dnorm), 1:10000, rep(0,3),accumulate=TRUE))
## hist(Reduce(function(state,x) mh(state,rprop,target=dnorm), 1:100000, 0,accumulate=TRUE),"fd",FALSE)
## curve(dnorm,add=TRUE)

## rprop <- function(state) state + rmvnorm(nrow(state),rep(0,ncol(state)))
## mh(matrix(1:3,3,3), rprop, target=dmvnorm)
