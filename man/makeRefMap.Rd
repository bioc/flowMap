\name{makeRefMap}
\alias{makeRefMap}
\title{Make mapping information for the proxy reference sample}
\usage{
  makeRefMap(stats,draws,nPopFilt,cutoff,sampleMethod,sampleSize,nperms)
}
\arguments{
  \item{stats}{an object of SampleInfo class}

  \item{draws}{number of random draws to compute estimated
  FR statistics for a population pair comparison}

  \item{nPopFilt}{number of reference populations to be
  compared for a test population}

  \item{cutoff}{p-value cutoff for matched/mismatched
  population pairs}

  \item{sampleMethod}{sampling scheme for each population
  comparison}

  \item{sampleSize}{number of events to draw from each
  population in a population pair comparison}

  \item{nperms}{number of permutations required to make the
  empirical null distribution}
}
\value{
  an object of refMap class
}
\description{
  This function performs multiple sample comparison among a
  set of specified sample files and then creates mapping
  information for proxy reference sample accordingly.
}
\examples{
sampleDir <- system.file("extdata",package="flowMap")
info <- makeSampleInfo(sampleDir=sampleDir,centroidDir=NULL)  
refMap <- makeRefMap(stats=info,draws=2,nPopFilt=3,cutoff=10^(-5)
  ,sampleMethod="equalSize",sampleSize=10,nperms=100)
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

