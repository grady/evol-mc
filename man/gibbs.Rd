\name{gibbs}
\alias{gibbs}
\title{Create a gibbs sampler object.}
\usage{
  gibbs(...)
}
\arguments{
  \item{...}{list of updating functions to be called in
  sequence.}
}
\value{
  a function which implements a Gibbs updater.
}
\description{
  This function accepts an arbitrary number of functions.
  It returns a function, which when called repeatedly
  implements a Gibbs updating scheme.
}
\details{
  Each function provided should accept the current state of
  the chain, and return a new state, updating some portion
  of the state using the full conditional.

  Each time the Gibbs sampler object is called, it will
  choose one of the provided functions (in succession) and
  use it to produce the new state.

  The signature of the returned function is

  function(state,...)

  the ... is passed to whichever function is chosen to do
  the updating.
}
\author{
  Grady Weyenberg
}

