\name{makeSampleInfo}
\alias{makeSampleInfo}
\title{Prepare background files to downstream data processing}
\usage{
  makeSampleInfo(sampleDir, centroidDir = NULL)
}
\arguments{
  \item{sampleDir}{directory where sample files are
  located}

  \item{centroidDir}{directory where centroid files are
  located}
}
\value{an object of class SampleInfo
}
\description{
  Create a txt files of the location of sample directory,
  centroid directory, and output directory. Create a txt
  file of the sample file information, including the sample
  file names, identifying number for each sample file, and
  the number of subpopulations contained in each sample.
}
\examples{
sampleDir <- system.file("extdata",package="flowMap")
info <- makeSampleInfo(sampleDir=sampleDir,centroidDir=NULL)  
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

