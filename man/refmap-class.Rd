\name{refMap-class}
\alias{getPairList}
\alias{getPairList,refMap-method}
\alias{refMap-class}
\title{Information required to making a proxy reference sample}
\usage{
  ## Accessors 

  getPairList(object)
}
\arguments{
  \item{object}{Object of class
  \code{\linkS4class{refMap}}}
}
\description{
  This class stores the information used to generate a
  proxy reference sample.
}
\section{Accessors}{
  In the following code \code{object} is a \code{refMap}
  object. \describe{ \item{\code{getPairList}:}{a
  \code{data.frame} with columns \describe{
  \item{\code{SampleID}:}{an unique identifying number for
  the sample file} \item{\code{testSample}:}{cell
  population label in the test sample}
  \item{\code{refSample}:}{the match reference population
  label (\code{NA} if no match)} \item{\code{newID}:}{the
  new population label in the proxy reference sample} } } }
}
\examples{
sampleDir <- system.file("extdata",package="flowMap")
info <- makeSampleInfo(sampleDir=sampleDir,centroidDir=NULL)  
refMap <- makeRefMap(stats=info,draws=2,nPopFilt=3,cutoff=10^(-5)
  ,sampleMethod="equalSize",sampleSize=10,nperms=100)
getPairList(refMap)
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

