\name{statCrossLists}
\alias{statCrossLists}
\title{summary statistic across elements of lists}
\usage{
  statCrossLists(obj, STAT)
}
\arguments{
  \item{obj}{an object of one or more lists. List elements
  must be matrices of sample dimensions}

  \item{STAT}{the statistic to be computed for each entry
  in the matrix across lists. Default value: median.}
}
\value{
  \item{est}{a matrix of the estimated statistic across random draws.}
}
\description{
  Compute an estimate of F-R statistics across random draws
  of cell population comparison.
}
\examples{
xx <- list(x1=matrix(seq(1:10),nr=5,nc=2),
	   x2=matrix(seq(21:30),nr=5,nc=2))
res <- statCrossLists(xx,"median")
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}
