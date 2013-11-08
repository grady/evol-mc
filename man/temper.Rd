\name{temper}
\alias{temper}
\title{Returns a function which implements the exchange update step.}
\usage{
  temper(fn)
}
\arguments{
  \item{fn}{heated density function (see heat)}
}
\value{
  a function implementing the parallel tempering exchange
  operation.
}
\description{
  foobar
}
\examples{
init <- rbind(1,2,3,4,5)
f <- function(state) exp(sum(log(dnorm(state) + dnorm(state,10)))) #gaussian mixture target
g <- heat(f,c(1,5,10,15,25)) #heated densities
gp <- function(state) state + rnorm(length(state)) #proposal distribution
gch <- iterate(10000,gibbs(metropolis(g,gp),temper(g)),init)
plot(gch)
xx <- prune(gch,1,1) #extract the chain with correct target distribution
hist(xx,freq=FALSE)
curve((dnorm(x)+dnorm(x,10))/2,add=TRUE)
}
\author{
  Grady Weyenberg
}

