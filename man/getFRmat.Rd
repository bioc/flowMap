\name{getFRmat}
\alias{getFRmat}
\title{F-R tests for two-sampbetween two samples}
\usage{
  getFRmat(i, XX1, XX2, iiTest = NULL, sampleMethod,
    sampleSize)
}
\arguments{
  \item{i}{dummy variable to initialize parallel computing
  (package Rparallel)}

  \item{XX1}{matrix or data.frame of events by feature
  where the events are indexed by a variable id in the last
  column of the matrix or data.frame. This sample is
  consisted of the populations to be mapped.}

  \item{XX2}{matrix or data.frame of events by feature
  where the events are indexed by a variable id in the last
  column of the matrix or data.frame. This sample is the
  designated reference sample.  XX1 populations are mapped
  onto the XX2 populations during cross-sample comparions.}

  \item{iiTest}{matrix of 0/1, indexing the population
  pairs across the two samples that are likely to produce a
  match. Default is all pairs. The first column of iiTest
  indexes the population ID in XX1 and the subsequent
  columns index the XX2 populations to be compared with
  each XX1 population.}

  \item{sampleMethod}{method of downsampling. Currently,
  the only available method is equalSize. This method
  randomly selects the same number of events from each
  group.}

  \item{sampleSize}{given the method equalSize, specify the
  number of events to sample in each group of a population
  comparison.}
}
\value{
  \item{wmat}{FR statistics for each population comparison 
  (XX1 populations by XX2 populations).  The table entry is 
  marked as N/A if F-R test is not performed per filtering 
  table iiTest.}

  \item{runsmat}{the number of runs for each comparison (XX1 
  populations by XX2 populations)}

  \item{mumat}{the expected number of runs for each comparison 
  (XX1 populations by XX2 populations)}

  \item{sigma2mat}{the estimated variance of runs for each 
  comparison (XX1 populations by XX2 populations)}

  \item{pNormat}{the two-sided p-value associated with the F-R 
  statistic under asymptotic normal assumption of the F-R statistic 
  (XX1 populations by XX2 populations)}
}
\examples{
sam1 <- read.table(system.file("extdata/sample1.txt",package="flowMap")
  ,header=TRUE)
sam2 <- read.table(system.file("extdata/sample2.txt",package="flowMap")
  ,header=TRUE)
res <- getFRmat(i=1, XX1=sam1, XX2=sam2, iiTest = NULL, 
  sampleMethod="equalSize",sampleSize=10)
}
\description{
  Compute the F-R statistics for all possible population
  comparisons between two samples. The function also
  provides an argument iiTest that selects a subset of
  population comparions that are likely to produce a match.
  The computing time of this function grows with sampleSize
  and the number of population comparions.
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

