\name{somePops-class}
\alias{getPopFilt}
\alias{getPopFilt,somePops-method}
\alias{somePops-class}
\title{List of population pairs to be compared during multiple-sample comparisons}
\usage{
  ## Accessors
  getPopFilt(object)
}
\arguments{
  \item{object}{Object of class
  \code{\linkS4class{somePops}}}
}
\description{
  This class stores the population pairs selected after the
  prefilteirng process.
}
\section{Accessors}{
  In the following code \code{object} is a \code{somePops}
  object. \describe{ \item{\code{getPopFilt}:}{a
  \code{list} with matrices as list elments. Each matrix
  contains labels of population pairs that are selected via
  prefiltering process.  } }
}
\examples{
sampleDir <- system.file("extdata",package="flowMap")
info <- makeSampleInfo(sampleDir=sampleDir,centroidDir=NULL)  
# this example is not run; 
# getSomePops(refCentroidDir,nPopFilt=3)
}\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

