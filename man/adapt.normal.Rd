\name{adapt.normal}
\alias{adapt.normal}
\title{Adaptive proposal distribution}
\usage{
  adapt.normal(target, init, sigma = diag(length(init)),
    blocksize = 500, nblocks = 100, bar = FALSE)
}
\arguments{
  \item{target}{target log-density}

  \item{init}{initial value. chosing a good one is probably
  important.}

  \item{sigma}{starting covariance, default is identity
  matrix}

  \item{blocksize}{number of iterations to run between
  updates of covariance}

  \item{nblocks}{number of times the covariance gets
  updated}

  \item{bar}{print progress bar?}
}
\value{
  a multivariate proposing function
}
\description{
  Attempts to create a good proposal distribution for
  updating a state vector. The function returned implements
  a multivariate normal proposal distribution with fixed
  covariance (suitable for use with metropolis), where the
  covariance is selected based on an initial run of the
  markov chain. Magic numbers are taken from Matt
  Schofields lecture notes.
}
\examples{
## define a multivariate target density
tgt <- function(state)
  -mahalanobis(as.vector(state),colMeans(iris[1:4]),var(iris[1:4]))/2
## adapt.normal attempts to create a good proposal distribution
mvn.prop <- adapt.normal(tgt, rep(0,4), nblocks=20)
environment(mvn.prop)$sigma #the proposal covariance
g.ch <- iterate(1000, metropolis(tgt, mvn.prop), rep(0,4))
mean(sapply(g.ch[-1],attr,"accept")) #acceptance rate for mvn.prop
}
\author{
  Grady Weyenberg
}

