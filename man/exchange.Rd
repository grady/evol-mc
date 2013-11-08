\name{exchange}
\alias{exchange}
\title{Exchange states from a population.}
\usage{
  exchange(state)
}
\arguments{
  \item{state}{current population state.}
}
\value{
  new state with two neighbors interchanged.
}
\description{
  A special proposal generator which interchanges two
  neighboring individuals from a population. Assumes that
  the state is a matrix, and that individuals are stored in
  rows. Useful for parallel tempering, etc.
}
\author{
  Grady Weyenberg
}

