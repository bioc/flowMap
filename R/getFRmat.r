#' F-R tests for two-sampbetween two samples
#'
#' Compute the F-R statistics for all possible population comparisons between two samples. 
#' The function also provides an argument iiTest that selects a subset of population comparions
#' that are likely to produce a match. The computing time of this function grows with sampleSize
#' and the number of population comparions.
#'  
#' @param i dummy variable to initialize parallel computing (package Rparallel)
#' @param XX1 matrix or data.frame of events by feature where the events are indexed by a variable id in the 
#'            last column of the matrix or data.frame. This sample is consisted of the populations to be mapped.
#' @param XX2 matrix or data.frame of events by feature where the events are indexed by a variable id in the 
#'            last column of the matrix or data.frame. This sample is the designated reference sample. 
#'            XX1 populations are mapped onto the XX2 populations during cross-sample comparions.
#' @param iiTest matrix of 0/1, indexing the population pairs across the two samples that are 
#'              likely to produce a match. Default is all pairs. The first column of iiTest indexes the 
#'              population ID in XX1 and the subsequent columns index the XX2 populations to be compared 
#'              with each XX1 population.
#' @param sampleMethod method of downsampling. Currently, the only available method is equalSize. This method
#'                    randomly selects the same number of events from each group. 
#' @param sampleSize given the method equalSize, specify the number of events to sample in each group 
#'                  of a population comparison.
#' 
#' @return wmat FR statistics for each population comparison (XX1 populations by XX2 populations). 
#'              The table entry is marked as N/A if F-R test is not performed per filtering table iiTest.
#' @return runsmat the number of runs for each comparison (XX1 populations by XX2 populations)
#' @return mumat the expected number of runs for each comparison (XX1 populations by XX2 populations)
#' @return sigma2mat the estimated variance of runs for each comparison (XX1 populations by XX2 populations)
#' @return pNormat the two-sided p-value associated with the F-R statistic under asymptotic normal assumption 
#'                  of the F-R statistic (XX1 populations by XX2 populations)
#'
#' @name getFRmat
#' 
#' @export
getFRmat <- function(i,XX1,XX2,iiTest=NULL,sampleMethod,sampleSize) { 
    XX1 <- as.data.frame(XX1)
    XX2 <- as.data.frame(XX2)

    XX1.list <-split(XX1,f=XX1$id)
    XX2.list <-split(XX2,f=XX2$id)

    lvl1 <- length(XX1.list)
    lvl2 <- length(XX2.list)

    F1 <- dim(XX1.list[[1]])[2]-1
    F2 <- dim(XX2.list[[1]])[2]-1 

    XX1.list <- lapply(XX1.list,function(x) {
      x <- x[,1:F1]
      return(x)})

    XX2.list <- lapply(XX2.list,function(x) {
      x <- x[,1:F2]
      return(x)})


    wmat <- matrix(NA,lvl1,lvl2)
    colnames(wmat)=as.numeric(sort(unique(XX2$id)))
    rownames(wmat)=as.numeric(sort(unique(XX1$id))) 

    mumat <- wmat
    sigma2mat <- wmat
    runsmat <- wmat
    pNormat <- wmat

    rep <- i

    if (is.null(iiTest)) {
        sps <- length(unique(XX1$id))
        ref.groups <- sort(unique(XX2$id))
        temp <- matrix(ref.groups,ncol=length(ref.groups),nrow=sps,byrow=T)
        iiTest <- data.frame(sp=sort(unique(XX1$id)),temp)
    }

    testPops <- as.numeric(as.character(iiTest[,1]))

    for (jj in seq_along(testPops)) {
      refPops <- iiTest[jj, c(2:ncol(iiTest)) ]

      for (kk in seq_along(refPops)) {
        j <- testPops[jj]
        k <- as.numeric(as.character( refPops )[kk])

        iirow <- which(rownames(wmat)==j)
        iicol <- which(colnames(wmat)==k)

        xx1 <- XX1.list[[iirow]]
        xx2 <- XX2.list[[iicol]]

        if (sampleMethod=="equalSize") {

          if (nrow(xx1) > sampleSize) {
           iisam1 <- sample(nrow(xx1),sampleSize)
          } else {
           iisam1 <- c(1:nrow(xx1))
          }
          if (nrow(xx2) > sampleSize) {
           iisam2 <- sample(nrow(xx2),sampleSize)
          } else {
           iisam2 <- c(1:nrow(xx2))
          }          
        } else  {
          message("warning: sampling method unspecified")
        }
        mat1 <- xx1[iisam1,]
        mat2 <- xx2[iisam2,]

        temp <- getFR(mat1,mat2)
        wmat[iirow,iicol] <- temp$ww
        mumat[iirow,iicol] <- temp$mu
        sigma2mat[iirow,iicol] <- temp$sigma2
        runsmat[iirow,iicol] <- temp$runs
        pNormat[iirow,iicol] <- temp$pNorm

      }      
    }

    list(wmat=wmat,runsmat=runsmat,mumat=mumat,sigma2mat=sigma2mat,pNormat=pNormat)  
}


