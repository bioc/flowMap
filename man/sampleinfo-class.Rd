\name{SampleInfo-class}
\alias{SampleInfo-class}
\alias{getSampleInfo}
\alias{getSampleInfo,SampleInfo-method}
\title{List of directories and sample files}
\usage{
  ## Accessors

  getSampleInfo(object)
}
\arguments{
  \item{object}{Object of class (or inheriting from)
  \code{\linkS4class{SampleInfo}}}
}
\description{
  This class stores the information necessary to uniquly
  identify each test sample.
}
\section{Accessors}{
  In the following code \code{object} is a
  \code{SampleInfo} object. \describe{
  \item{\code{getSampleInfo}:}{a \code{data.frame} with
  columns \describe{ \item{\code{sampleID}:}{unique
  identifying number for the inputted sample files}
  \item{\code{sampleNames}:}{names of the inputted sample
  files} } } }
}
\examples{
sampleDir <- system.file("extdata",package="flowMap")
info <- makeSampleInfo(sampleDir=sampleDir,centroidDir=NULL)  
getSampleInfo(info)
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

