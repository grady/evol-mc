##' Trivariate 20-part gaussian mixture.
##' @title target density
##' @param x matrix with observations in rows
##' @param means means
##' @return vector of length nrow(x) with density values
##' @author Grady Weyenberg
##' @export
##' @examples
##' means <- t(replicate(20, runif(3,-10,10)))
##' target(matrix(1:12,4,3),means)
##' ## rowMeans(apply(means,1,function(mean) dmvnorm(matrix(1:12,4,3),mean,diag(3)/10)))
target <- function(x,means){
  if(is.vector(x)) x <- matrix(x,1)
  if(ncol(x) != 3L) stop("x must have 3 colums")
  const <- 3 * (log(0.1)+log(2*pi))
  df <- function(mean){ #multivariate density
    x <- sweep(x,2,mean)
    d <- rowSums(x * x) * 10
    exp(-(const + d)/2)
  }
  val <- apply(means,1,df)
  if(is.matrix(val)) rowMeans(val) else mean(val)
}
