\name{refSample-class}
\alias{getNewPops}
\alias{getNewPops,refSample-method}
\alias{getRefData}
\alias{getRefData,refSample-method}
\alias{refSample-class}
\title{Data and summary information of the proxy reference sample}
\usage{
  ## Accessors 
  getNewPops(object)
  getRefData(object)
}
\arguments{
  \item{object}{Object of class
  \code{\linkS4class{refSample}}}
}
\description{
  This class stores the information used to generate a
  proxy reference sample as well as the resultant sample
  data.
}
\section{Accessors}{
  In the following code \code{object} is a \code{refSample}
  object. \describe{ \item{\code{getNewPops}:}{an integer,
  number of new populations added after the mapping
  process} \item{\code{getRefData}:}{a data.frame of flow
  cytometry data combinging samples used to create this
  proxy refernece sample; the rows are the events/cells,
  and the columns are the FCM features with the last column
  being the new population label} }
}
\examples{
sampleDir <- system.file("extdata",package="flowMap")
info <- makeSampleInfo(sampleDir=sampleDir,centroidDir=NULL)  
refSam <- makeRefSample(stats=info,draws=2,nPopFilt=3,cutoff=10^(-5),
  sampleMethod="equalSize",sampleSize=10,nperms=100)
getNewPops(refSam)
getRefData(refSam)
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

