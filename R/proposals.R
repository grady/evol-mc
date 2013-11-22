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
##' @author Edward Roualdes
##' @export
crossover2 <- function(state){
    h <- apply(state,1,function(x) exp(-1*dmix(x)))
    vec <- seq_len(nrow(state))
    x1 <- sample(vec, 1, prob=h/sum(h))
    x2 <- sample(vec[-x1], 1)
    c <- seq_len(sample.int(ncol(state),1))
    state[c(x1,x2),c] <- state[c(x2,x1),c]
    state
}

##' associated crossover density function
##'
##' evaluation of density depends on the two
##' rows that were randomly selected for crossover.
##' @param old old population matrix of parents
##' @param new newly proposed population matrix of parents
##' @author Edward Roualdes
##' @export
crossover2d <- function(old, new){
    ## find differing rows between old and new states
    idx <- which(apply(old==new,1,
                     function(x) all(as.logical(x))) == FALSE)
    x <- apply(new,1,function(x) exp(-1*dmix(x)))
    px <- sum(x[idx])/sum(x)
    px
}
