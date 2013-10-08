#' F-R statistic estimates across two samples 
#'
#' Compute an estimate of F-R statistics based on random draws of each population pair comparison. The 
#' computation is optimized by incorporating the RParallel package. Users can specify the number of processing
#' cores to be used during computation.
#'  
#' @param XX1 sample 1 txt file (cell populations are indexed by id)
#' @param XX2 sample 2 txt file (cell populations are indexed by id)
#' @param iiTest an matrix of cell population pairs to be included in the cross-sample comparison.
#' @param draws number of random draws 
#' @param sampleMethod methods of downsampling. The current default is equalSize, which samples 
#'		an equal number of events from each group.      
#' @param sampleSize for the equalSize sampling method, specify the number of events to be sample from each group
#' @param estStat statistic that used to estimate population F-R statistic (median is the default)
#' 
#' @return wmat estimated F-R statistics across random draws (XX1 populations by XX2 populations)
#' @return runsmat estimated number of within-group subtrees across randomd draws (XX1 populations by XX2 populations)
#' @return mumat estimated nubmer of expected runs across random draws (XX1 populations by XX2 populations)
#' @return sigma2mat estimated variance of runs across randomd draws (XX1 populations by XX2 populations)
#'
#' @examples
#' ## see vignettes
#'
#' @name getFRest
#' 
#' @export
getFRest <- function(XX1,XX2,iiTest,draws,sampleMethod,sampleSize,estStat) {

    ncores <- detectCores()
    registerDoParallel(cores=ncores)

    message("computing FR statistics between sample populations... \n")
    mat <- foreach(i=1:draws) %dopar% flowMap::getFRmat(i,XX1,XX2,iiTest,sampleMethod,sampleSize)
    closeSockets()

    npop1 <- length(unique(XX1$id))
    npop2 <- length(unique(XX2$id))
    wmat <- lapply(mat,"[[",1) 
    wmat <- statCrossLists(wmat,estStat)
    colnames(wmat)=as.numeric(as.character(sort(unique(XX2$id))))
    rownames(wmat)=as.numeric(as.character(sort(unique(XX1$id))))

    runsmat <- lapply(mat,"[[",2) 
    runsmat <- statCrossLists(runsmat,estStat)

    mumat <- lapply(mat,"[[",3) 
    mumat <- statCrossLists(mumat,estStat)

    sigma2mat <- lapply(mat,"[[",4) 
    sigma2mat <- statCrossLists(sigma2mat,estStat)

    pNormat <- lapply(mat,"[[",5) 
    pNormat <- statCrossLists(pNormat,estStat)

    new("FRstats",
    	XX1=XX1,
    	XX2=XX2,
    	sampleMethod=sampleMethod,
    	sampleSize=sampleSize,
    	ncores=ncores,
    	draws=draws,
    	npop1=npop1,
    	npop2=npop2,
    	pop1Labels=sort(unique(XX1$id)),
    	pop2Labels=sort(unique(XX2$id)),
    	ww=wmat,
    	runs=runsmat,
    	mu=mumat,
    	sigma2=sigma2mat,
    	pNorm=pNormat)
}





