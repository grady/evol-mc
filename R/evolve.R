##' Heat a target distribution.
##'
##' Takes a target density function and returns a new function which
##' will evaluate each row of a state matrix at the corresponding
##' temperature.
##' @param fn target density function
##' @param temps temperature ladder, should probably include a 1.0 somewhere.
##' @param beta inverse-temperatures. an alternative way to specify
##' the temperatures, defaults to 1/temps.
##' @return a function(state) which applies fn(state[i,])^beta[i] to each row i of the state
##' @author Grady Weyenberg
##' @export
heat <- function(fn,temps=1,beta=1/temps) function(state) apply(state,1,fn)*beta

##' Evolution/Tempering functions
##' 
##' @param ladder density function.
##' @param rprop mutation proposal function.
##' @param dprop mutation density function, if required.
##' @return function implementing the step.
##' @author Grady Weyenberg
##' @export
mutate <- function(ladder, rprop, dprop=NULL){
  update <- metropolis(ladder, rprop, dprop)
  swap <- metropolis(sum %c% ladder, exchange)
  swap %c% update
}

##' @rdname mutate
##' @export
reproduce <- function(ladder){
  mate <- metropolis(sum %c% ladder, crossover)
  swap <- metropolis(sum %c% ladder, exchange)
  swap %c% mate
}

##' @rdname mutate
##' @export
weighted.reproduce <- function(ladder){
  mate <- metropolis(sum %c% ladder, weighted.crossover, ld.wt.crossover)
  swap <- metropolis(sum %c% ladder, exchange)
  swap %c% mate
}
