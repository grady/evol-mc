##' Subset each element of a chain.
##' @param chain chain to operate on.
##' @param ... passed to the `[` function for each element in chain.
##' @return a new chain
##' @author Grady Weyenberg
prune <- function(chain,...){
  x <- lapply(chain,`[`,...)
  class(x) <- oldClass(chain)
  x
}

##' Iteratively call a function.
##'
##' @param n number of times to iterate.
##' @param fn function to iterate.
##' @param init initial value for chain.
##' @return a list of states at each iteration (length n+1).
##' @author Grady Weyenberg
iterate <- function(n,fn,init,...){
  n <- n+1L
  chain <- vector('list',n)
  chain[[1]] <- init
  i <- 1L
  while((i <- i+1)<=n){
    chain[[i]] <- fn(chain[[i-1L]])
  }
  structure(chain, class="chain")
}

##' Pretty pictures.
##'
##' each element of chain should be a matrix with each row
##' corresponding to an individual state of the chain and columns
##' containing different variables. Each variable is plotted
##' separately, with multiple individual chains overplotted.
##' @param x a chain to plot. 
##' @author Grady Weyenberg
plot.chain <- function(x,...){
  m <- simplify2array(x)
  d <- dim(m)
  op <- par(mfcol=c(2,d[2]))
  on.exit(par(op))
  xlim <- c(0,d[3])
  ylim <- apply(m,2,range)
  for(i in seq_len(d[2])){
    name <- dimnames(m)[[2]][i]
    plot(1,1,'n',xlim,ylim[,i], xlab='index', ylab=name)
    title(paste("Traceplot of",d[1],"populations"))
    for(j in seq_len(d[1])) lines(m[j,i,], col=j)
    acf(as.vector(t(m[,i,])), main=paste("Series", name))
  }
}

hist.chain <- function(x,discard=0.1,...){
  if(discard<1.0) discard <- round(discard * length(x))
  cat("Discarding first",discard,"states.\n")
  x <- x[-seq_len(discard)]
  m <- simplify2array(x)
  d <- dim(m)
  op <- par(mfcol=c(1,d[2]))
  on.exit(par(op))
  for(i in seq_len(d[2])){
    name <- dimnames(m)[[2]][i]
    hist(m[,i,],...,main=paste("Histogram of",name))
  }
}

##' Basic summary of a chain object.
##'
##' Finds mean, sd, and the q-th quantiles of the chain, optionally
##' discarding a portion of the beginning of the chain. Each element
##' in the list of chain states should be a matrix with variables to
##' be summarized in columns. If there are multiple individual chains
##' in the rows they will be all contribute to the summary statistics.
##' 
##' @param chain list of states to be summarized.
##' @param discard If >= 1, integer removes that many elements from chain
##' before summarizing. If < 1.0, removes that fraction from the
##' chain. Default is 10%.
##' @return matrix of summary statistics.
##' @author Grady Weyenberg
summary.chain <- function(chain,discard=0.1,q=c(0.025,0.965),...){
  if(discard<1.0) discard <- round(discard * length(chain))
  chain <- chain[-seq_len(discard)]
  m <- simplify2array(chain)
  d <- dim(m)
  f <- function(x) c(mean=mean(x), se=sd(x), quantile(x,q))
  cat("Discarding first",discard,"states.\n")
  apply(m,2,f)
}

##' You probably don't really want to see the entire list of states.
print.chain <- function(x,...){
  cat("Chain with",length(x),"states.\n\n")
  cat("Final state:\n")
  print(x[[length(x)]],...)
  invisible(x)
}

##' Need this to keep the extraction operator from fucking up the
##' class.
##' @author Grady Weyenberg
`[.chain` <- function(x,...){
  val <- NextMethod("[")
  class(val) <- oldClass(x)
  val
}
