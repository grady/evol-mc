\name{mutate}
\alias{mutate}
\alias{reproduce}
\alias{weighted.reproduce}
\title{Evolution/Tempering functions}
\usage{
  mutate(ladder, rprop, dprop = NULL)

  reproduce(ladder)

  weighted.reproduce(ladder)
}
\arguments{
  \item{ladder}{density function.}

  \item{rprop}{mutation proposal function.}

  \item{dprop}{mutation density function, if required.}
}
\value{
  function implementing the step.
}
\description{
  Evolution/Tempering functions
}
\author{
  Grady Weyenberg
}

