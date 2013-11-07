\name{target}
\alias{target}
\title{target density}
\usage{
  target(x, means)
}
\arguments{
  \item{x}{matrix with observations in rows}

  \item{means}{means}
}
\value{
  vector of length nrow(x) with density values
}
\description{
  Trivariate 20-part gaussian mixture.
}
\examples{
means <- t(replicate(20, runif(3,-10,10)))
target(matrix(1:12,4,3),means)
## rowMeans(apply(means,1,function(mean) dmvnorm(matrix(1:12,4,3),mean,diag(3)/10)))
}
\author{
  Grady Weyenberg
}

