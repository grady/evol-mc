##' Create a crossover updater
##' @inheritParams temper
##' @return a function that implements the crossover updater
##' @author Grady Weyenberg
##' @export
crossover <- function(target,temps){
  g <- heat(target,temps)
  x <- metropolis(function(x) exp(sum(log(g(x)))), xover)
  e <- metropolis(function(x) exp(sum(log(g(x)))), exchange)
  function(state) e(x(state))
}

xover <- function(state){
  ##this proposes crossovers uniformly
  r <- sample.int(nrow(state),2)
  c <- seq_len(sample.int(ncol(state),1))
  state[r,c] <- state[rev(r),c]
  state
}

