#' Compute statistics for the empirical distribution
#'
#' This method permutes the observed FR statistics for each cross-sample comparison and creates an empirical 
#' null distribution of the FR statistics on which empirical values are decided.
#'  
#' @param stats an object of class FRstats as produced by the getFRest function 
#' @param nperms number of permutations required to create the empirical distribution of the FR statistic
#' @param iiTest prefiltering matrix indicating which populaiton pairs to perform testing on
#' @return a object of class FRvalsPerms 
#'
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#' 
#' @export
getFRvalsPerm <- function(stats,nperms,iiTest)
	{
     newpops <- permutePops(stats@XX1,stats@XX2)

     ncores <- detectCores()
     registerDoParallel(cores=ncores)     
     message(paste("request",ncores,"processing cores \n"))

     message("computing the empirical distribution for F-R statistics... \n")

     message(paste(nperms,"permutations of the cell population labels are generated... \n"))

	 mat <- foreach(i=1:nperms) %dopar% flowMap::getFRmat(i,XX1=newpops$XX1,XX2=newpops$XX2,iiTest,sampleMethod=stats@sampleMethod,sampleSize=stats@sampleSize)$wmat
     closeSockets()

     wnulls <- abind(mat,along=3)
     pnulls <- matrix(0,ncol=ncol(stats@ww),nrow=nrow(stats@ww))
     for (i in 1:nrow(stats@ww)) {
        for (j in 1:ncol(stats@ww)) {
            if (is.na(stats@ww[i,j])) {
              pnulls[i,j] <- NA
            } else {
              pnulls[i,j] <- 10^(-45)+ (sum(wnulls[i,j,] < stats@ww[i,j])/nperms)
            }
        }
     }
     colnames(pnulls)=colnames(stats@ww)
     rownames(pnulls)=rownames(stats@ww)  

	 new("FRvalsPerm",
		sampleMethod=stats@sampleMethod,
		sampleSize=stats@sampleSize,
		ncores=ncores,
		wnulls=wnulls,
		pnulls=pnulls,
		nperms=nperms)
	}


