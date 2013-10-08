\name{permutePops}
\alias{permutePops}
\title{Permute cell population labels across two samples}
\usage{
  permutePops(XX1, XX2)
}
\arguments{
  \item{XX1}{sample 1 matrix of cell populations (indexed
  by the variable id, the last column of the matrix)}

  \item{XX2}{sample 2 matrix of cell populations (indexed
  by the variable id, the last column of the matrix)}
}
\value{
  \item{XX1}{sample 1 matrix based on permutated data}
  \item{XX2}{sample 2 matrix based on permuted data}
}
\examples{
sam1 <- read.table(system.file("extdata/sample1.txt",package="flowMap")
  ,header=TRUE)
sam2 <- read.table(system.file("extdata/sample2.txt",package="flowMap")
  ,header=TRUE)
permSam <- permutePops(sam1,sam2)
}
\description{
  In order to compute the empirical p-value of the F-R
  statistics, we permute the labels of the cell populations
  across samples.
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

