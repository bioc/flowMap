#' Permute cell population labels across two samples
#' 
#' In order to compute the empirical p-value of the F-R statistics, we permute the labels of the cell populations across samples.
#'  
#' @param XX1 sample 1 matrix of cell populations (indexed by the variable id, the last column of the matrix)
#' @param XX2 sample 2 matrix of cell populations (indexed by the variable id, the last column of the matrix)
#'     
#' @return XX1 
#' @return XX2 
#'
#' @name permutePops
#' 
#' @export
permutePops <- function(XX1,XX2) {
	labs_test0 <- paste("test_",XX1$id,sep="")
	labs_ref0 <- paste("ref_",XX2$id,sep="")
	labs <- c(labs_test0,labs_ref0)

	ii <- sample(labs,replace=F)
	ii <- strsplit(ii,split="_")

	grp <- sapply(1:length(ii),function(x) ii[[x]][1]=="ref")
	labs_test1 <- as.numeric(sapply(ii[!grp],"[[",2))
	labs_ref1 <- as.numeric(sapply(ii[grp],"[[",2))

	XX1$id <- labs_test1
	XX2$id <- labs_ref1

	list(XX1=XX1,XX2=XX2)
}
