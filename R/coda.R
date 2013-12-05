## Stuff for interfacing with the coda package.

##' Convert a chain object to a coda mcmc.list object
##' 
##' @importFrom coda as.mcmc mcmc.list
##' @method as.mcmc.list chain
##' @param x chain to be converted
##' @param ... unused arguments
##' @return a coda mcmc.list object
##' @author Grady Weyenberg
as.mcmc.list.chain <- function(x,...){
  x <- aperm(simplify2array(x))
  mcmc.list(unlist(apply(x,3,list %c% as.mcmc), FALSE))
}
