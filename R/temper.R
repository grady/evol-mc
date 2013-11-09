##' Exchange states from a population.
##'
##' A special proposal generator which interchanges two neighboring
##' individuals from a population. Assumes that the state is a matrix,
##' and that individuals are stored in rows. Useful for parallel
##' tempering, etc.
##' @param state current population state.
##' @return new state with two neighbors interchanged.
##' @author Grady Weyenberg
##' @export
exchange <- function(state){
  n <- nrow(state)
  i <- sample.int(n,1)
  j <- i + (rbinom(1,1,0.5)*2L - 1L)
  if (j<1) j <- 2L
  if (j>n) j <- n-1L
  state[c(i,j),] <- state[c(j,i),]
  state
}

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
##' @examples
##' g <- heat(dnorm, 2^(0:4))
##' gp <- function(state) state + rnorm(length(state))
##' m <- metropolis(g,gp)
##' init <- rbind(1,2,3,4,5)
##' gch <- iterate(10000,m,init)
##' plot(gch)
##' plot(density(unlist(prune(gch,1,TRUE))))
##' for(i in 2:5) lines(y~x,density(unlist(prune(gch,i,TRUE)))[1:2])
heat <- function(fn,temps=1,beta=1/temps) function(state) apply(state,1,fn)^beta

##' Returns a function which implements parallel tempering.
##'
##' foobar
##' @param target target density
##' @param temps temperature ladder
##' @param rprop proposal generator
##' @param dprop proposal density. only needed if non-symmetric.
##' @return a function implementing a parallel tempering update.
##' @author Grady Weyenberg
##' @export
##' @examples
##' init <- rbind(1,2,3,4,5)
##' f <- function(state) exp(sum(log(dnorm(state) + dnorm(state,10)))) #gaussian mixture target
##' gp <- function(state) state + rnorm(length(state)) #proposal distribution
##' temps <- c(1,5,10,15,25)
##' gch <- iterate(10000,temper(f,temps,gp),init)
##' plot(gch)
##' xx <- prune(gch,1,1) #extract the chain with correct target distribution
##' hist(xx,freq=FALSE)
##' curve((dnorm(x)+dnorm(x,10))/2,add=TRUE)
temper <- function(target,temps,rprop,dprop=NULL){
  g <- heat(target,temps)
  m <- metropolis(g,rprop,dprop)
  e <- metropolis(function(x) exp(sum(log(g(x)))), exchange)
  function(state) e(m(state))
}
