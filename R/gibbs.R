##' Create a gibbs sampler object.
##'
##' This function accepts an arbitrary number of functions. It returns
##' a function, which when called repeatedly implements a Gibbs
##' updating scheme. (Provided, of course, that the functions are
##' "carefully selected.")
##'
##' Each function provided should accept the current state of the
##' chain, and return a new state, updating some portion
##' of the state using (presumably) a full conditional.
##'
##' Each time the Gibbs sampler object is called, it will choose one
##' of the provided functions, either in succession or stochastically,
##' depending on the value of p, and use it to produce the new state.
##'
##' The signature of the returned function is
##'
##' function(state,...)
##'
##' the ... is passed to whichever function is chosen to do the
##' updating.
##' @param ... list of updating functions to be called in sequence.
##' @param p if not false, weights to stochastically select an
##' updating function.
##' @return a function which implements a Gibbs updater.
##' @author Grady Weyenberg
##' @export
gibbs <- function(...,p=FALSE){
  funs <- lapply(list(...),match.fun)
  if(length(funs)<1) stop("supply at least one function")
  k <- length(funs)
  if (k==1){
    update <- funs[[1]]
  } else if(any(as.logical(p))) {
    update <- function(state,...)
      funs[[sample.int(k,1,prob=p)]](state,...)
  } else {
    i <- 0L
    update <- function(state,...){
      i <<- if(i >= k) 1L else i + 1L
      structure(funs[[i]](state,...), i=i)
    }
  }
  update
}
