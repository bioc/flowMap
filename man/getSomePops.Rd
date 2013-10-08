
\name{getSomePops}
\alias{getSomePops}
\title{Make prefiltering matrix for each test sample}
\usage{
  getSomePops(stats,refCentroidDir,nPopFilt)  
}
\arguments{
  \item{stats}{an object of \code{\linkS4class{SampleInfo}} class}

  \item{refCentroidDir}{directory where the centroid file
  of the reference samples are located}

  \item{nPopFilt}{number of reference populations to be
  compared for each test sample population}
}
\value{
  an object of somePops class.
}
\description{
  This method select the population pairs (within each test
  population) that are the most similar by centroid
  distance (Eudlidean).
}
\examples{
sampleDir <- system.file("extdata",package="flowMap")
info <- makeSampleInfo(sampleDir=sampleDir,centroidDir=NULL)  
\dontrun{
  getSomePops(info,refCentroidDir,nPopFilt=3)
}}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

