\name{getFRest}
\alias{getFRest}
\title{F-R statistic estimates across two samples}
\usage{
  getFRest(XX1, XX2, iiTest, draws, sampleMethod,
    sampleSize, estStat)
}
\arguments{
  \item{XX1}{sample 1 txt file (cell populations are
  indexed by id)}

  \item{XX2}{sample 2 txt file (cell populations are
  indexed by id)}

  \item{iiTest}{an matrix of cell population pairs to be
  included in the cross-sample comparison.}

  \item{draws}{number of random draws}

  \item{sampleMethod}{methods of downsampling. The current
  default is equalSize, which samples an equal number of
  events from each group.}

  \item{sampleSize}{for the equalSize sampling method,
  specify the number of events to be sample from each
  group}

  \item{estStat}{statistic that used to estimate population
  F-R statistic (median is the default)}
}
\value{
  a object of class FRstats
}
\description{
  Compute an estimate of F-R statistics based on random
  draws of each population pair comparison. The computation
  is optimized by incorporating the RParallel package.
  Users can specify the number of processing cores to be
  used during computation.
}
\examples{
sam1 <- read.table(system.file("extdata/sample1.txt",package="flowMap")
  ,header=TRUE)
sam2 <- read.table(system.file("extdata/sample2.txt",package="flowMap")
  ,header=TRUE)
xx1 <- sam1[sam1$id==1,]; xx1 <- xx1[sample(nrow(xx1),20),]
xx2 <- sam2[sam2$id==1,]; xx2 <- xx2[sample(nrow(xx2),20),]
res <- getFRest(XX1=sam1, XX2=sam2, iiTest=NULL, draws=1, 
  sampleMethod="equalSize",sampleSize=10, estStat="median")
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

