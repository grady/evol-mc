\name{compose}
\alias{\%c\%}
\alias{compose}
\title{Function composure}
\usage{
  compose(...)

  g \%c\% f
}
\arguments{
  \item{...}{functions}

  \item{g}{applied second}

  \item{f}{applied first}
}
\value{
  a function
}
\description{
  Returns a function which is the composure of the
  functions provided. They are applied in the reverse order
  that they are supplied (to match traditional mathematical
  notation, g o f.

  Oh god what have I created? It returns a function that is
  the composition of g with f.
}
\author{
  Grady Weyenberg
}

