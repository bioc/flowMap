\name{matchMat-class}
\alias{getMatchMat}
\alias{getMatchMat,matchMat-method}
\alias{getMatchPairs}
\alias{getMatchPairs,matchMat-method}
\alias{matchMat-class}
\title{Matching information about a single cross-sample comparison}
\usage{
  ## Accessors 
  getMatchPairs(object)
  getMatchMat(object)
}
\arguments{
  \item{object}{Object of class
  \code{\linkS4class{matchMat}}}
}
\description{
  This class stores the information required to identify
  matched/mismatched cell population comparisons.
}
\section{Accessors}{
  In the following code \code{object} is a \code{matchMat}
  object. \describe{ \item{\code{getMatchedPairs}:}{a
  matrix of matched population labels with columns
  \describe{ \item{\code{testSample}:}{all population
  labels in the test sample}
  \item{\code{refSample}:}{matched reference sample
  population label} } } \item{\code{getMatchMat}:}{a matrix
  of 0 and 1 entires indexing matched/mismatched population
  pairs} }
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
permRes <- getFRvalsPerm(res,nperms=100,iiTest=NULL)  
matchRes <- getMatched(permRes,cutoff=10^(-5))
getMatchPairs(matchRes)
getMatchMat(matchRes)
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

