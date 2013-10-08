\name{FRmapping}
\alias{FRmapping}
\title{Multiple sample comparison with a user-defined reference}
\usage{
  FRmapping(outputDir, sampleDir, centroidDir,
    refDataDir = NULL, refCentroidDir = NULL,
    makeRefSampleDir = NULL, makeRefCentroidDir = NULL,
    nPopFilt = 3, draws, cutoff = 0.01/30,
    sampleMethod = "equalSize", sampleSize = 100,
    nperms = 1000)
}
\arguments{
  \item{outputDir}{directory where the output files will be
  saved.}

  \item{sampleDir}{directory where all sample files are
  located. Note that samples need to be in txt format with
  the cell populations indexed by the last column in the
  data (which needs to be called id)}

  \item{centroidDir}{directory where all centroid files
  corresponding to the sample files are located. These also
  need to be in txt forma.}

  \item{refDataDir}{directory where the reference sample
  file is located.}

  \item{refCentroidDir}{directory where the centroid file
  for the reference sample is located.}

  \item{makeRefSampleDir}{directory of sample files to be
  used in making a reference file}

  \item{makeRefCentroidDir}{directory of centroids
  correspoinding to the sample files in makeRefSampleDir}

  \item{nPopFilt}{to reduce running time, the users can
  choose to compute F-R statistic on some reference
  populations that are likely to be similar to the sample
  population (based on Euclidean distance between
  centroids). Default value: 3.}

  \item{draws}{number of random draws per population
  comparison. The F-R estimate is calculated as the median
  of the F-R statistics across random draws. Default value:
  100.}

  \item{cutoff}{Cut-off for the pvalues in determining
  matched vs mismatched populations. Default value:
  0.01/30.}

  \item{sampleMethod}{Downsampling method. Equal size
  sampling is the only available method at this point. The
  method takes an equal number of events from the two
  groups. For group sizes smaller than the default sample
  size, the group size is used in the computations. Default
  value: equalSize.}

  \item{sampleSize}{The number of events to be sampled from
  each group in the equalSize sampling method. Default
  value: 100.}

  \item{nperms}{Cell population labels within a single
  cross-sample comparisons are permuted to computed the
  empirical p-value for the F-R statistic. Results of 1000
  permutations are similar to results of 10,000
  permutations. Default value: 1000.}

}
\value{
  \item{refData}{name of the reference sample file}

  \item{testSet}{sample files mapped}

  \item{fileRes}{list of prefiltering matrix}

  \item{nNewPops}{number of new populations discovered during mapping}

  \item{pairList}{mapping results for each sample file (including 
  sample cell population IDs and reference sample IDs)}

  \item{Pnulls}{empirical p-values of FR statistics for each sample file}

  \item{Pnorms}{p-value of FR-statistics for each sample file}

  \item{WWobs}{observed F-R statistic for each smaple file}

  \item{Wnulls}{permuted distributions}
}
\description{
  Make a referecen sample from a subset of the sample
  files. Map all sample files to the reference. Create an
  summary file of the sample file names, sample file IDs,
  reference sample IDs, original subpopulation IDs in each
  sample, and the corresponding new subpopulation IDs.
}
\examples{
\dontrun{FRmapping(outputDir, sampleDir, centroidDir,
    refDataDir = NULL, refCentroidDir = NULL,
    makeRefSampleDir = NULL, makeRefCentroidDir = NULL,
    nPopFilt = 3, draws, cutoff = 0.01/30,
    sampleMethod = "equalSize", sampleSize = 100,
    nperms = 1000)
}}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

