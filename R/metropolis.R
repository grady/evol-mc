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
  function(state,...){
    new <- rprop(state,...)
    ## a is -log(metropolis factor)
    a <- target(state) - target(new)
    if(is.function(dprop)) a <- a - dprop(state,new,...) + dprop(new,state,...)
    i <- a > 0.0 # if false acceptance prob is >=1, so replace
    ## tryCatch(
    i[i] <- rexp(sum(i)) <= a[i] # if (i) keep w/prob 1-exp(-a) = pexp(a)
    ## ,error=function(x) browser())
    state[!i] <- new[!i] # if (i) keep else replace
    attr(state,'accept') <- !i
    state
  } 
}
