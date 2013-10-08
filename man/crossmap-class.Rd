
\name{crossMap-class}
\alias{crossMap-class}
\alias{getCrossList}
\alias{getCrossList,crossMap-method}
\alias{getFRmapPnorms}
\alias{getFRmapPnorms,crossMap-method}
\alias{getFRmapPnulls}
\alias{getFRmapPnulls,crossMap-method}
\alias{getFRmapStats}
\alias{getFRmapStats,crossMap-method}
\title{Statistics of mapping the test samples and the reference sample 
    (or the proxy reference)}
\usage{
  ## usage 
  ## Accessors 
  getFRmapStats(object)
  getFRmapPnorms(object) 
  getFRmapPnulls(object)
  getCrossList(object)
}
\arguments{
  \item{object}{Object of class
  \code{\linkS4class{crossMap}}}
}
\description{
  This class stores statistics of multiple-sample
  comparison.
}
\section{Accessors}{
  In the following code \code{object} is a \code{crossMap}
  object. \describe{ \item{\code{getFRmapStats}:}{list of
  matrices; each contains the observed FR statistics
  between a test sample and the reference}
  \item{\code{getFRmapPnorm}:}{list of matrices; each
  contains the theoretical p-values between a test sample
  and the reference} \item{\code{getFRmapPnulls}:}{list of
  matrices; each contains the empirical p-values between a
  test sample and the reference}
  \item{\code{getCrossList}:}{matrix of test and reference
  sample population labels. A \code{data.frame} with
  columns \describe{ \item{\code{SampleID}:}{an unique
  identifying number for the sample file}
  \item{\code{testSample}:}{cell population label in the
  test sample} \item{\code{refSample}:}{the match reference
  population label (\code{NA} if no match)}
  \item{\code{newID}:}{the new population label in the
  proxy reference sample} } } }
}
\examples{
sampleDir <- system.file("extdata",package="flowMap")
info <- makeSampleInfo(sampleDir=sampleDir,centroidDir=NULL)  
refDataDir <- system.file("extdata/refData",package="flowMap")
crossMap <- makeCrossMap(stats=info,refDataDir,filtRes=NULL,
  draws=2,cutoff=10^(-5),sampleMethod="equalSize",sampleSize=10,
  nperms=100)
getFRmapStats(crossMap)
getFRmapPnorms(crossMap) 
getFRmapPnulls(crossMap)
getCrossList(crossMap)
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

