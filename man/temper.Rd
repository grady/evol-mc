\name{temper}
\alias{temper}
\title{Returns a function which implements parallel tempering.}
\usage{
  temper(target, temps, rprop, dprop = NULL)
}
\arguments{
  \item{target}{target density}

  \item{temps}{temperature ladder}

  \item{rprop}{proposal generator}

  \item{dprop}{proposal density. only needed if
  non-symmetric.}
}
\value{
  a function implementing a parallel tempering update.
}
\description{
  foobar
}
\examples{
init <- rbind(1,2,3,4,5)
f <- function(state) exp(sum(log(dnorm(state) + dnorm(state,10)))) #gaussian mixture target
gp <- function(state) state + rnorm(length(state)) #proposal distribution
temps <- c(1,5,10,15,25)
gch <- iterate(10000,temper(f,temps,gp),init)
plot(gch)
xx <- prune(gch,1,1) #extract the chain with correct target distribution
hist(xx,freq=FALSE)
curve((dnorm(x)+dnorm(x,10))/2,add=TRUE)
}
\author{
  Grady Weyenberg
}

