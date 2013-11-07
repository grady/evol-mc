##' Create a gibbs sampler object.
##'
##' This function accepts an arbitrary number of functions. It returns
##' a function, which when called repeatedly implements a Gibbs
##' updating scheme.
##'
##' Each function provided should accept the current state of the
##' chain, and return a new state, updating some portion
##' of the state using the full conditional.
##'
##' Each time the Gibbs sampler object is called, it will choose one
##' of the provided functions (in succession) and use it to produce
##' the new state.
##'
##' The signature of the returned function is
##'
##' function(state,...)
##'
##' the ... is passed to whichever function is chosen to do the
##' updating.
##' @param ... list of updating functions to be called in sequence.
##' @return a function which implements a Gibbs updater.
##' @author Grady Weyenberg
gibbs <- function(...){
  conditionals <- lapply(list(...),match.fun)
  if(length(conditionals)<1) stop("supply at least one function")
  k <- length(conditionals)
  i <- 0L
  update <- function(state,...){
    i <<- if(i >= k) 1L else i + 1L
    structure(conditionals[[i]](state,...), i=i)
  }
  update
}
