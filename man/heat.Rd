\name{heat}
\alias{heat}
\title{Heat a target distribution.}
\usage{
  heat(fn, temps = 1, beta = 1/temps)
}
\arguments{
  \item{fn}{target density function}

  \item{temps}{temperature ladder, should probably include
  a 1.0 somewhere.}

  \item{beta}{inverse-temperatures. an alternative way to
  specify the temperatures, defaults to 1/temps.}
}
\value{
  a function(state) which applies fn(state[i,])^beta[i] to
  each row i of the state
}
\description{
  Takes a target density function and returns a new
  function which will evaluate each row of a state matrix
  at the corresponding temperature.
}
\author{
  Grady Weyenberg
}

