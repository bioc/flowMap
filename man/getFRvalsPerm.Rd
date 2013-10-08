\name{getFRvalsPerm}
\alias{getFRvalsPerm}
\title{Compute statistics for the empirical distribution}
\usage{
  getFRvalsPerm(stats,nperms,iiTest)
}
\arguments{
  \item{stats}{an object of class FRstats as produced by
  the getFRest function}

  \item{nperms}{number of permutations required to create
  the empirical distribution of the FR statistic}

  \item{iiTest}{prefiltering matrix indicating which
  populaiton pairs to perform testing on}

}
\value{
  a object of class FRvalsPerms
}
\description{
  This method permutes the observed FR statistics for each
  cross-sample comparison and creates an empirical null
  distribution of the FR statistics on which empirical
  values are decided.
}
\examples{
sam1 <- read.table(system.file("extdata/sample1.txt",package="flowMap")
  ,header=TRUE)
sam2 <- read.table(system.file("extdata/sample2.txt",package="flowMap")
  ,header=TRUE)
xx1 <- sam1[sam1$id==1,]; xx1 <- xx1[sample(nrow(xx1),20),]
xx2 <- sam2[sam2$id==1,]; xx2 <- xx2[sample(nrow(xx2),20),]
res <- getFRest(XX1=sam1, XX2=sam2, iiTest=NULL, draws=1, 
  sampleMethod="equalSize",sampleSize=10, estStat="median")
permRes <- getFRvalsPerm(res,nperms=100,iiTest=NULL)      
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

