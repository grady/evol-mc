set.seed(12345)

means <- t(replicate(20, runif(3,-10,10)))
##' Trivariate 20-part gaussian mixture.
##' @title target density
##' @param x matrix with observations in rows
##' @param means 
##' @return vector of length nrow(x) with density values
##' @author Grady Weyenberg
##' @export
target <- function(x,means){
  if(ncol(x) != 3L) stop("x must have 3 colums")
  const <- 3 * (log(0.1)+log(2*pi))
  df <- function(mean){ #multivariate density
    x <- sweep(x,2,mean)
    d <- rowSums(x * x) * 10
    exp(-(const + d)/2)
  }
  rowMeans(apply(means,1,df))
}

## rowMeans(apply(means,1,function(mean) dmvnorm(matrix(1:12,4,3),mean,diag(3)/10)))
target(matrix(1:12,4,3),means)

x <- seq(-10,10,0.05)
outer(x,x, function(y,z) dnorm(c(y,z,0)))
