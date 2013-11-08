\name{summary.chain}
\alias{summary.chain}
\title{Basic summary of a chain object.}
\usage{
  \method{summary}{chain} (object, discard = 0.1,
    q = c(0.025, 0.975), ...)
}
\arguments{
  \item{object}{object to be summarized.}

  \item{discard}{If >=1, remove that many elements from
  start of chain before summarizing. If <1.0, removes that
  fraction from the chain. Default is 10\%.}

  \item{q}{quantiles to find.}

  \item{...}{unused.}

  \item{chain}{list of states to be summarized.}
}
\value{
  matrix of summary statistics.
}
\description{
  Finds mean, sd, and the q-th quantiles of the chain,
  optionally discarding a portion of the beginning of the
  chain. Each element in the list of chain states should be
  a matrix with variables to be summarized in columns. If
  there are multiple individual chains in the rows they
  will be all contribute to the summary statistics.
}
\author{
  Grady Weyenberg
}

