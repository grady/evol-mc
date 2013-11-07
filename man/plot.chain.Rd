\name{plot.chain}
\alias{plot.chain}
\title{Pretty pictures.}
\usage{
  \method{plot}{chain} (x, ...)
}
\arguments{
  \item{x}{a chain to plot.}

  \item{...}{unused}
}
\description{
  each element of chain should be a matrix with each row
  corresponding to an individual state of the chain and
  columns containing different variables. Each variable is
  plotted separately, with multiple individual chains
  overplotted.
}
\author{
  Grady Weyenberg
}

