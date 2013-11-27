##' Adaptive proposal distribution
##'
##' Attempts to create a good proposal distribution for updating a
##' state vector. The function returned implements a multivariate
##' normal proposal distribution with fixed covariance (suitable for
##' use with metropolis), where the covariance is selected based on an
##' initial run of the markov chain. Magic numbers are taken from Matt
##' Schofields lecture notes.
##' @param target target log-density
##' @param init initial value. chosing a good one is probably important.
##' @param sigma starting covariance, default is identity matrix
##' @param blocksize number of iterations to run between updates of covariance
##' @param nblocks number of times the covariance gets updated
##' @param bar print progress bar?
##' @return a multivariate proposing function 
##' @author Grady Weyenberg
##' @export
##' @examples
##' ## define a multivariate target density
##' tgt <- function(state)
##'   -mahalanobis(as.vector(state),colMeans(iris[1:4]),var(iris[1:4]))/2
##' ## adapt.normal attempts to create a good proposal distribution
##' mvn.prop <- adapt.normal(tgt, rep(0,4), nblocks=20)
##' environment(mvn.prop)$sigma #the proposal covariance
##' g.ch <- iterate(1000, metropolis(tgt, mvn.prop), rep(0,4))
##' mean(sapply(g.ch[-1],attr,"accept")) #acceptance rate for mvn.prop
adapt.normal <- function(target, init, sigma=diag(length(init)), blocksize=500, nblocks=100,bar=FALSE){
  updater <- metropolis(target,gaussian.walk(sigma))
  chain <- iterate(blocksize,updater, init)
  ## check that the starting variance isn't too large
  if(mean(sapply(chain[-1],attr,'accept')) < 0.1)
    warning("Acceptance rate <10% for first sigma update.")
  i <- 0L
  if(bar){
    pb <- txtProgressBar(i,nblocks)
    on.exit(close(pb))
  }
  while((i <- i+1L) <= nblocks){ 
    sigma <- var(t(simplify2array(chain))) * 5.75 / length(init) #magic number from lect9
    diag(sigma) <- diag(sigma) * 1.05 #decrease the correlations slightly
    updater <- metropolis(target,gaussian.walk(sigma))
    chain <- c(chain, iterate(blocksize, updater, chain[[length(chain)]]))
    if(bar) setTxtProgressBar(pb,i)
  }
  gaussian.walk(sigma)
}


tgt <- function(state)  -mahalanobis(as.vector(state),colMeans(iris[1:4]),var(iris[1:4]))/2
mvn.prop <- adapt.normal(tgt, rep(0,4), nblocks=20)
