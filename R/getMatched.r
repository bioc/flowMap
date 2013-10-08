#' Identify matched population pairs
#'
#' This method applies a prespecified cutoff to the empirical p-values to identify the matched populations.
#'  
#' @param stats an oject of FRvalsPerm class 
#' @param cutoff cutoff for identifying significant p-values, i.e., which population pairs matched/mismatched
#' @return an object of class matchMat containing matched population pair labels
#'
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#'
#' @export
getMatched <- function(stats,cutoff)
	{
	matchMat <- matrix(0,ncol=dim(stats@pnulls)[2],nrow=dim(stats@pnulls)[1])
    colnames(matchMat)=colnames(stats@pnulls)
    rownames(matchMat)=rownames(stats@pnulls)  

    cutMatch <- sapply(1:nrow(stats@pnulls),function(i) {
    	xx <- stats@pnulls[i,]
    	if (sum(xx>cutoff,na.rm=T)==0) {
    		rr <- NA
    	} else if (sum(xx>cutoff,na.rm=T)>0) {
    		rr <- intersect(which(xx>cutoff),which.max(xx))
    	} 
    		rr
    	})
	iiMatch <- data.frame(testSample=seq(1:nrow(stats@pnulls)),refSample=cutMatch)

	matchMat[as.matrix(iiMatch)] <- 1

	matchPairs <- iiMatch
	matchPairs[,1] <- rownames(stats@pnulls)[as.numeric(matchPairs[,1])]
	matchPairs[,2] <- colnames(stats@pnulls)[as.numeric(matchPairs[,2])]

	matchPairs <- as.matrix(matchPairs)
	iiMatch <- as.matrix(iiMatch)
	# colnames(matchPairs) <- c("testSample","refSample")

	new("matchMat",
		cutoff=cutoff,
		iiMatch=iiMatch,
		matchPairs=matchPairs,
		matchMat=matchMat)
	}


