\name{uniform.walk}
\alias{uniform.walk}
\title{Uniform jumping proposal functions}
\usage{
  uniform.walk(state, eps = 1)
}
\arguments{
  \item{state}{current state}

  \item{eps}{maximum magnitude of a jump}
}
\description{
  I'm not sure about if these should be like gaussian.walk
  or this. The function from metropolis returns
  function(state,...), and the ... is passed to the rprop
  and dprop functions. It allows for this to work, but in
  practice it seems to make code less self contained and
  more confusing, since you have to suppply arguments to
  iterate to control the walker. On the other hand this
  method means one less intermediate function to construct,
  and the opportunity to adapt without creating a new
  proposal function.
}
\examples{
tgt <- function(state)
  -mahalanobis(as.vector(state),colMeans(iris[1:4]),var(iris[1:4]))/2
iterate(10, metropolis(tgt,uniform.walk), rep(0,4), eps=0.5)
}
\author{
  Grady Weyenberg
}

