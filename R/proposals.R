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

##' Proposes matings uniformly.
##'
##' Does proposes a uniform k=1 crossover.
##' @param state population matrix of parents
##' @return a new state
##' @author Grady Weyenberg
##' @export
crossover <- function(state){
  r <- sample.int(nrow(state),2)
  c <- seq_len(sample.int(ncol(state),1))
  state[r,c] <- state[rev(r),c]
  state
}

##' Proposes a density weighted k=1 crossover.
##' @param state population matrix of parents
##' @return a new state
##' @author Edward A. Roualdes
##' @export
weighted.crossover <- function(state){
    h <- apply(state,1,function(x) exp(dmix(x)))
    ## h <- h / sum(h) #normalize? (i don't think it's required?)
    vec <- seq_len(nrow(state))
    x1 <- sample(vec, 1, prob=h)
    x2 <- sample(vec[-x1], 1)
    c <- seq_len(sample.int(ncol(state),1))
    state[c(x1,x2),c] <- state[c(x2,x1),c]
    attr(state, 'i') <- x1
    state
}

##' associated crossover density function
##'
##' evaluation of density depends on the two
##' rows that were randomly selected for crossover.
##' @param old old population matrix of parents
##' @param new newly proposed population matrix of parents
##' @author Edward A. Roualdes
##' @export
ld.wt.crossover <- function(new, old){
    ## find differing rows between old and new states
    if (is.null(i <- attr(new, 'i')))
      i <- attr(old, 'i')
    x <- apply(old,1,function(y) exp(dmix(y)))
    x[i] / sum(x)
}


##' Multivariate normal random walk proposal
##'
##' Creates a Gaussian random walk updater function using the given
##' covariance matrix. Assumes state is a vector. NB: This is NOT the
##' updater function itself. The return value is the updater function.
##' @param sigma covariance matrix
##' @return proposal generator function 
##' @author Grady Weyenberg
##' @export
##' @examples
##' tgt <- function(state)
##'   -mahalanobis(as.vector(state),colMeans(iris[1:4]),var(iris[1:4]))/2
##' rprop <- gaussian.walk(diag(4)/20)
##' iterate(10, metropolis(tgt,rprop), rep(0,4))
gaussian.walk <- function(sigma){
  rt.sigma <- chol(sigma,pivot=TRUE)
  rt.sigma <- rt.sigma[,order(attr(rt.sigma, "pivot"))] #undo pivot
  function(state) state + drop(crossprod(rt.sigma, rnorm(ncol(rt.sigma))))
}


##' Uniform jumping proposal functions
##'
##' I'm not sure about if these should be like gaussian.walk or
##' this. The function from metropolis returns function(state,...),
##' and the ... is passed to the rprop and dprop functions. It allows
##' for this to work, but in practice it seems to make code less self
##' contained and more confusing, since you have to suppply arguments
##' to iterate to control the walker. On the other hand this method
##' means one less intermediate function to construct, and the
##' opportunity to adapt without creating a new proposal function.
##' @param state current state
##' @param eps maximum magnitude of a jump
##' @author Grady Weyenberg
##' @export
##' @examples
##' tgt <- function(state)
##'   -mahalanobis(as.vector(state),colMeans(iris[1:4]),var(iris[1:4]))/2
##' iterate(10, metropolis(tgt,uniform.walk), rep(0,4), eps=0.5)
uniform.walk <- function(state,eps=1) state + runif(length(state),-eps,eps)


