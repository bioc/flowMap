#' Multiple sample comparisons 
#'
#' Perform Friedman-Rafsky test on multiple flow cytometry samples. This function peforms similar tasks as
#' FRmapping, except that FRmappingSimple does not generate text files and takes in matrices or list of matrices
#' as input arguments instead of text file directories.
#' 
#' @param samples list of matrices or data.frames. Each matrix or data.frame is a flow cytometry sample where
#'				the last column indicates population id (variable id)
#' @param centroids list of matrices or data.frames. Each matrix or data.frame consits of centroids for the 
#'				corresponding flow cytometry sample specified under the argument sample.
#' @param refSample a matrix or data.frame of reference sample. Each sample specified under the argument sample 
#'				will be compared against this reference sample using F-R test.
#' @param refCentroid a matrix or data.frame of the centroids of the sample populations specified in the refSample 
#'				argument. 
#' @param nPopFilt to reduce running time, the users can choose to compute F-R statistic on some reference populations 
#' 			that are likely to be similar to the sample population (based on Euclidean distance between centroids). Default value: 3.
#' @param draws number of random draws per population comparison. The F-R estimate is calculated as the median of the 
#' 			F-R statistics across random draws. Default value: 100.
#' @param cutoff Cut-off for the pvalues in determining matched vs mismatched populations. Default value: 0.01/30.
#' @param sampleMethod Downsampling method. Equal size sampling is the only available method at this point. The method 
#'			takes an equal number of events from the two groups. For group sizes smaller than the default sample size, the group size is used in the computations. Default value: equalSize.
#' @param sampleSize The number of events to be sampled from each group in the equalSize sampling method. Default value: 100.
#' @param nperms Cell population labels within a single cross-sample comparisons are permuted to computed the empirical p-value 
#'		for the F-R statistic. Results of 1000 permutations are similar to results of 10,000 permutations. Default value: 1000.
#' 
#' @return fileRes
#' @return nNewPops number of new populations discovered during mapping.
#' @return pairList mapping results for each sample file (including sample cell population IDs and reference sample IDs)
#' @return Pnulls empirical p-values of FR statistics for each sample file
#' @return Pnorms p-value of FR-statistics for each sample file
#' @return WWobs observed F-R statistic for each smaple file
#' @return Wnulls permuted distributions 
#' @examples
#' ## see vignettes
#'
#' @name FRmappingSimple
#'
#' @export
FRmappingSimple <- function(samples, centroids,
	refSample,refCentroid,
	nPopFilt=3,draws=100,cutoff=0.01/30,sampleMethod="equalSize",sampleSize=100,nperms=1000) 
{
	nn <- length(samples)

	if (!is.null(centroids) ) {

	# nn <- length(centroids)
	filtRes <- lapply(1:nn,function(i) {

		# the last column of the centroid matrix is population id
		data <- data.frame(centroids[[i]])
		poplabs <- data$id

		distmat <- as.matrix(dist(rbind(data,refCentroid),diag=T,upper=T))
		distmat <- distmat[1:nrow(data),(nrow(data)+1):(nrow(data)+nrow(refCentroid))]

		colnames(distmat) <- rownames(refCentroid)
		rownames(distmat) <- rownames(data)	

 		ordMat <- apply(distmat,1,function(x) as.numeric(colnames(distmat)[order(x)[c(1:nPopFilt)]]) )

 		mat <- data.frame(sampleID=i,sp=poplabs,t(ordMat))
 		mat
 	})
	} 

	XX2 <- refSample
	nrefs <- length(unique(XX2$id))

	pairMat <- lapply(1:nn, function(i) {
		if (is.null(centroids)) {
			IItest <- NULL
		} else {
			IItest <- filtRes[[i]][,-1]
		}
		
		XX1 <- samples[[i]]

		FRdata <- getFRest(XX1=XX1,XX2=XX2,iiTest=IItest,draws,sampleMethod="equalSize",
						sampleSize,estStat="median")
		FRperm <- getFRvalsPerm(FRdata,nperms=nperms,iiTest=IItest)
		matchRes <- getMatched(FRperm,cutoff)
		matchPairs <- getMatchPairs(matchRes)
		
		list(pairRes=cbind(i,matchPairs),Pnorms=getPnorm(FRdata),Pnulls=getPnulls(FRperm),
			WWobs=getFRstats(FRdata),Wnulls=getWnulls(FRperm))
	})

	Pnorms <- lapply(pairMat,"[[",2)
	Pnulls <- lapply(pairMat,"[[",3)
	WWobs <- lapply(pairMat,"[[",4)
	Wnulls <- lapply(pairMat,"[[",5)

	pairList <- lapply(pairMat,"[[",1)
	pairList <- data.frame(do.call(rbind,pairList))
	colnames(pairList)[1] <- "sampleID"
	pairList$refSample <- as.numeric(as.character(pairList$refSample))
	pairList$testSample <- as.numeric(as.character(pairList$testSample))
	pairList$sampleID <- as.numeric(as.character(pairList$sampleID))

	pairList$newID <- pairList$refSample
	pairList$newID[is.na(pairList$newID)] <- nrefs+seq(1:sum(is.na(pairList$newID)))

	nNewPops <- sum(is.na(pairList$refSample))
	# refSeed <- as.character(refSeed)
	# testSet <- as.character(testSet)

	if (!is.null(centroids) ) {
    new("crossMap",
	    refData=NULL,
    	testSet=NULL,
    	filtRes=filtRes,
    	nNewPops=nNewPops,
    	pairList=pairList,
    	Pnulls=Pnulls,
    	Pnorms=Pnorms,
    	WWobs=WWobs,
    	Wnulls=Wnulls)
	} else {
    new("crossMap",
	    refData=NULL,
    	testSet=NULL,
    	filtRes=NULL,
    	nNewPops=nNewPops,
    	pairList=pairList,
    	Pnulls=Pnulls,
    	Pnorms=Pnorms,
    	WWobs=WWobs,
    	Wnulls=Wnulls)
	}
}





