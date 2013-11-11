##' Function composure
##'
##' Returns a function which is the composure of the functions
##' provided. They are applied in the reverse order that they are
##' supplied (to match traditional mathematical notation, g o f.
##' @param ... functions 
##' @return a function
##' @author Grady Weyenberg
##' @export
compose <- function(...){
  ff <- lapply(list(...),match.fun)
  function(x){
    for(f in rev(ff)) x <- f(x)
    x
  }
}



##' Compose as an infix operator.
##'
##' Oh god what have I created? It returns a function that is the
##' composition of g with f.
##' @usage g \%c\% f
##' @rdname compose
##' @param g applied second
##' @param f applied first
"%c%" <- function(g,f) function(x) g(f(x))
