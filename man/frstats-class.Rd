\name{FRstats-class}
\alias{FRstats-class}
\alias{getFRstats}
\alias{getFRstats,FRstats-method}
\alias{getPnorm}
\alias{getPnorm,FRstats-method}
\title{FR statistics and p-values generated from one single draw of the population pair comparison}
\usage{
  ## Accessors
  getFRstats(object)
  getPnorm(object)
}
\arguments{
  \item{object}{Object of class
  \code{\linkS4class{FRstats}}}
}
\description{
  This class stores the statistics required to compute
  median FR statistics across random draws.
}
\section{Accessors}{
  In the following code \code{object} is a \code{FRstats}
  object. \describe{ \item{\code{getFRstats}:}{matrix of
  median FR statistics across random draws}
  \item{\code{getPnorm}:}{matrix of median theoretical null
  p-values across random draws} }
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
getFRstats(res)
getPnorm(res)
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

