\name{FRvalsPerm-class}
\alias{FRvalsPerm-class}
\alias{getPnulls}
\alias{getPnulls,FRvalsPerm-method}
\alias{getWnulls}
\alias{getWnulls,FRvalsPerm-method}
\title{Statistics generated from the permuted distribution of the observed FR statistics}
\usage{
  ## Accessors 
  getPnulls(object) 
  getWnulls(object)
}
\arguments{
  \item{object}{Object of class
  \code{\linkS4class{FRvalsPerm}}}
}
\description{
  This class stores the statistics required to decide
  whether a population pair is matched or not.
}
\section{Accessors}{
  In the following code \code{object} is a
  \code{FRvalsPerm} object. \describe{
  \item{\code{getPnulls}:}{matrix of empirical null
  p-values (test sample by reference sample)}
  \item{\code{getWnulls}:}{matrix of the permuted FR
  statistics (used to calculate the empirical null
  p-values)} }
}
\examples{
sam1 <- read.table(system.file("extdata/sample1.txt",package="flowMap"),
  header=TRUE)
sam2 <- read.table(system.file("extdata/sample2.txt",package="flowMap"),
  header=TRUE)
xx1 <- sam1[sam1$id==1,]; xx1 <- xx1[sample(nrow(xx1),20),]
xx2 <- sam2[sam2$id==1,]; xx2 <- xx2[sample(nrow(xx2),20),]
res <- getFRest(XX1=sam1, XX2=sam2, iiTest=NULL, draws=1, 
  sampleMethod="equalSize",sampleSize=10, estStat="median")
permRes <- getFRvalsPerm(res,nperms=100,iiTest=NULL)      
getPnulls(permRes)
getWnulls(permRes)
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}
\seealso{
  \code{\linkS4class{FRstats}} for the class of objects
  from which the observed FR statistics are calculated.
  \cite{\link{getMatched}} is the method used to identify
  the matched pairs.
}

