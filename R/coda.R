## Stuff for interfacing with the coda package.

##' Convert a chain object to a coda::mcmc.list object
##' 
##' @importFrom coda as.mcmc mcmc.list as.mcmc.list
##' @param x chain to be converted
##' @param ... unused arguments
##' @return a coda::mcmc.list object
##' @author Grady Weyenberg
##' @export
##' @method as.mcmc.list chain
as.mcmc.list.chain <- function(x,...){
  x <- aperm(simplify2array(x))
  mcmc.list(unlist(apply(x,3,list %c% as.mcmc), FALSE))
}
