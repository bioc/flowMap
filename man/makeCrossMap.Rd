\name{makeCrossMap}
\alias{makeCrossMap}
\title{Make mapping information for the meta set of populations}
\usage{
  makeCrossMap(stats,refDataDir,filtRes,draws,cutoff,sampleMethod,
    sampleSize,nperms)
}
\arguments{
  \item{stats}{an object of SampleInfo class}

  \item{refDataDir}{directory where the user-defined sample
  file is located}

  \item{filtRes}{list of matrices containing prefiltering
  population pairs for each cross-sample comparison}

  \item{draws}{number of random draws to compute estimated
  FR statistics for a population pair comparison}

  \item{cutoff}{p-value cutoff for identifying matched pairs}

  \item{sampleMethod}{sampling scheme for each population
  comparison}

  \item{sampleSize}{number of events to draw from each
  population in a population pair comparison}

  \item{nperms}{number of permutations required to make the
  empirical null distribution}
}
\value{
  an object of crossMap class.
}
\description{
  Make a referecen sample from a subset of the sample
  files. Map all sample files to the reference. Create an
  summary file of the sample file names, sample file IDs,
  reference sample IDs, original subpopulation IDs in each
  sample, and the corresponding new subpopulation IDs.
}
\examples{
sampleDir <- system.file("extdata",package="flowMap")
info <- makeSampleInfo(sampleDir=sampleDir,centroidDir=NULL)  
refDataDir <- system.file("extdata/refData",package="flowMap")
crossMap <- makeCrossMap(stats=info,refDataDir,filtRes=NULL,
  draws=2,cutoff=10^(-5),sampleMethod="equalSize",sampleSize=10,
  nperms=100)
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

