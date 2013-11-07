\name{metropolis}
\alias{metropolis}
\title{Create a metropolis hastings updater function.}
\usage{
  metropolis(target, rprop, dprop = NULL)
}
\arguments{
  \item{target}{target distribution function. Function
  should accept current (population of) state(s) as its
  first parameter and return a vector containing the
  density evaluation(s).}

  \item{rprop}{either a function, or a list of functions.
  Proposal generator functio(n).}

  \item{dprop}{either a function, a list of functions or
  NULL. Proposal density function(s), NULL value means the
  proposal density is symmetric.}
}
\value{
  a function which implements the Metropolis-Hasting
  updater.
}
\description{
  This function creates and returns a function which
  implements a Metropolis-Hastings updater.
}
\author{
  Grady Weyenberg
}

