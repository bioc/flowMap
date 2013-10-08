\name{FRmappingSimple}
\alias{FRmappingSimple}
\title{Multiple sample comparisons}
\usage{
  FRmappingSimple(samples, centroids, refSample,
    refCentroid, nPopFilt = 3, draws = 100,
    cutoff = 0.01/30, sampleMethod = "equalSize",
    sampleSize = 100, nperms = 1000)
}
\arguments{
  \item{samples}{list of matrices or data.frames. Each
  matrix or data.frame is a flow cytometry sample where the
  last column indicates population id (variable id)}

  \item{centroids}{list of matrices or data.frames. Each
  matrix or data.frame consits of centroids for the
  corresponding flow cytometry sample specified under the
  argument sample.}

  \item{refSample}{a matrix or data.frame of reference
  sample. Each sample specified under the argument sample
  will be compared against this reference sample using F-R
  test.}

  \item{refCentroid}{a matrix or data.frame of the
  centroids of the sample populations specified in the
  refSample argument.}

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
  \item{fileRes}{list of prefiltering matrices}

  \item{nNewPops}{number of new populations discovered during mapping.}

  \item{pairList}{mapping results for each sample file (including 
  sample cell population IDs and reference sample IDs)}

  \item{Pnulls}{empirical p-values of FR statistics for each sample file}

  \item{Pnorms}{p-value of FR-statistics for each sample file}

  \item{WWobs}{observed F-R statistic for each sample file}

  \item{Wnulls}{permuted distributions}
}
\description{
  Perform Friedman-Rafsky test on multiple flow cytometry
  samples. This function peforms similar tasks as
  FRmapping, except that FRmappingSimple does not generate
  text files and takes in matrices or list of matrices as
  input arguments instead of text file directories.
}
\examples{
sam1 <- read.table(system.file("extdata/sample1.txt",
  package="flowMap"),header=TRUE)
sam2 <- read.table(system.file("extdata/sample2.txt",
  package="flowMap"),header=TRUE)
res <- FRmappingSimple(samples=list(sam1),centroids=NULL,
          refSample=sam2,refCentroid=NULL,nPopFilt=NULL,
          draws=2,cutoff=10^(-5),sampleMethod="equalSize",
          sampleSize=10,nperm=100)   
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

