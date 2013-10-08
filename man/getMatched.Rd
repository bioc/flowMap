\name{getMatched}
\alias{getMatched}
\title{Identify matched population pairs}
\usage{
  getMatched(stats,cutoff)
}
\arguments{
  \item{stats}{an oject of FRvalsPerm class}

  \item{cutoff}{cutoff for identifying significant
  p-values, i.e., which population pairs
  matched/mismatched}
}
\value{
  an object of class matchMat containing matched population
  pair labels
}
\description{
  This method applies a prespecified cutoff to the
  empirical p-values to identify the matched populations.
}
\examples{
sam1 <- read.table(system.file("extdata/sample1.txt",package="flowMap"),
  header=TRUE)
sam2 <- read.table(system.file("extdata/sample2.txt",package="flowMap"),
  header=TRUE)
xx1 <- sam1[sam1$id==1,]; xx1 <- xx1[sample(nrow(xx1),20),]
xx2 <- sam2[sam2$id==1,]; xx2 <- xx2[sample(nrow(xx2),20),]
res <- getFRest(XX1=sam1, XX2=sam2, iiTest=NULL, draws=1, 
  sampleMethod="equalSize",sampleSize=10, estStat="median")
permRes <- getFRvalsPerm(res,nperms=100,iiTest=NULL)  
matchRes <- getMatched(permRes,cutoff=10^(-5))
}
\author{
  Chiaowen Joyce Hsiao \email{joyce.hsiao1@gmail.com}
}

