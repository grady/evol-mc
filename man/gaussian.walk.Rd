\name{gaussian.walk}
\alias{gaussian.walk}
\title{Multivariate normal random walk proposal}
\usage{
  gaussian.walk(sigma)
}
\arguments{
  \item{sigma}{covariance matrix}
}
\value{
  proposal generator function
}
\description{
  Creates a Gaussian random walk updater function using the
  given covariance matrix. Assumes state is a vector. NB:
  This is NOT the updater function itself. The return value
  is the updater function.
}
\examples{
tgt <- function(state)
  -mahalanobis(as.vector(state),colMeans(iris[1:4]),var(iris[1:4]))/2
rprop <- gaussian.walk(diag(4)/20)
iterate(10, metropolis(tgt,rprop), rep(0,4))
}
\author{
  Grady Weyenberg
}

