\name{iterate}
\alias{iterate}
\title{Iteratively call a function.}
\usage{
  iterate(n, fn, init, ...)
}
\arguments{
  \item{n}{number of times to iterate.}

  \item{fn}{function to iterate.}

  \item{init}{initial value for chain.}

  \item{...}{unused for now.}
}
\value{
  a list of states at each iteration (length n+1).
}
\description{
  Iteratively call a function.
}
\author{
  Grady Weyenberg
}

