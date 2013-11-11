##' Create a metropolis hastings updater function.
##'
##' This function creates and returns a function which implements a
##' Metropolis-Hastings updater.
##'
##' @param target target distribution function. Function should accept
##' current (population of) state(s) as its first parameter and return
##' a vector containing the density evaluation(s).
##' @param rprop either a function, or a list of functions. Proposal
##' generator functio(n).
##' @param dprop either a function, a list of functions or
##' NULL. Proposal density function(s), NULL value means the proposal
##' density is symmetric.
##' @return a function which implements the Metropolis-Hasting updater.
##' @author Grady Weyenberg
##' @export
metropolis <- function(target,rprop,dprop=NULL) {
  rprop <- match.fun(rprop)
  target <- match.fun(target)
  fn <- function(state,...){
    new <- rprop(state)
    a <- target(new) / target(state)
    if(is.function(dprop)) a <- a * dprop(state,new) / dprop(new,state)
    i <- a < 1.0 # if false, replace; if true, keep?
    ## tryCatch(
    i[i] <- runif(sum(i)) >= a[i] # keep with prob 1-a
    ## error=browser())
    state[!i] <- new[!i]
    attr(state,'accept') <- !i
    state
  } 
  fn
}
