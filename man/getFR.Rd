\name{getFR}
\alias{getFR}
\title{F-R test for one population comparison}
\usage{
  getFR(xx1, xx2)
}
\arguments{
  \item{xx1}{matrix of events (rows) by features (columns)}

  \item{xx2}{matrix of events (rows) by features (columns)}
}
\value{
  \item{ww}{FR statistic}

  \item{runs}{total number of within-group subtrees (or 
  equivalently, total number of between-group edges plus 1)
  }
  \item{mu}{estimated mean of runs}

  \item{sigma2}{estimated variance of runs}

  \item{pNorm}{p-values of the F-R statitic assuming large 
  sample asymptotic normal assumption}
}
\description{
  Compute a minimal spanning tree using package ade4
  (optimized MST computation in C). Following Friedman and
  Rafsky (1979), the number of runs is standardized by
  substracting the expected number of runs and dividing the
  difference by the square root of estimated variance. Each
  FR statistic is dependent on the topology of the minimal
  spanning tree of the given population comparision.
}
\examples{
sam1 <- read.table(system.file("extdata/sample1.txt",package="flowMap")
  ,header=TRUE)
sam2 <- read.table(system.file("extdata/sample2.txt",package="flowMap")
  ,header=TRUE)
xx1 <- sam1[sam1$id==1,]; xx1 <- xx1[sample(nrow(xx1),20),]
xx2 <- sam2[sam2$id==1,]; xx2 <- xx2[sample(nrow(xx2),20),]
FROne <- getFR(xx1,xx2)
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

