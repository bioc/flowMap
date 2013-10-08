#' Make mapping information for the proxy reference sample
#'
#' This function performs multiple sample comparison among a set of specified sample files and then creates mapping
#' information for proxy reference sample accordingly.
#'
#' @param stats an object of SampleInfo class
#' @param draws number of random draws to compute estimated FR statistics for a population pair comparison
#' @param nPopFilt number of reference populations to be compared for a test population
#' @param cutoff p-value cutoff for matched/mismatched population pairs
#' @param sampleMethod sampling scheme for each population comparison
#' @param sampleSize number of events to draw from each population in a population pair comparison
#' @param nperms number of permutations required to make the empirical null distribution
#' @return an object of refMap class
#'
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#' @export
makeRefMap <- function(stats,draws,nPopFilt,cutoff,sampleMethod,sampleSize,nperms)
	{
	info <- stats@info
	sampleDir <- stats@sampleDir

	refSeed <- info$sampleNames[which.max(info$npop)]
	testSet <- info$sampleNames[-which.max(info$npop)]
	sampleID <- info$sampleID[-which.max(info$npop)]

	XX2 <- read.table(paste(sampleDir,"/",refSeed,sep=""),header=T,sep="\t")
	nrefs <- length(unique(XX2$id))

	refSampleInfo <- stats
	refSampleInfo@nsamples <- length(sampleID)
	refSampleInfo@info <- info[which(info$sampleID %in% sampleID),]

	if (!is.null(refSampleInfo@centroidDir)) {	
		refSeedCentroid <- info$centroidNames[which.max(info$npop)]	
		filtRes <- getSomePops(refSampleInfo,as.character(refSeedCentroid),nPopFilt)
		iiTestList <- getPopFilt(filtRes)
		} else {
		filtRes <- NULL
		}

	message(paste("cross-samping population matching with",refSeed,"as reference \n"))

	pairMat <- lapply(1:length(testSet), function(i) {
# i=1
		message(paste("matching",testSet[i],"\n"))

		XX1 <- read.table(paste(sampleDir,"/",testSet[i],sep=""),header=T,sep="\t")
		
		if (is.null(filtRes)) {
			IItest <- NULL
		} else {
			IItest <- iiTestList[[i]][,-1]
		}

		FRdata <- getFRest(XX1=XX1,XX2=XX2,iiTest=IItest,draws=draws,sampleMethod="equalSize",
						sampleSize=sampleSize,estStat="median")
		FRperm <- getFRvalsPerm(FRdata,nperms=nperms,iiTest=NULL)
		matchRes <- getMatched(FRperm,cutoff)
		matchPairs <- getMatchPairs(matchRes)
		cbind(sampleID[i],matchPairs)
	})
	# names(pairMat) <- testSet

	pairList <- data.frame(do.call(rbind,pairMat))

	colnames(pairList)[1] <- "sampleID"
	pairList$refSample <- as.numeric(as.character(pairList$refSample))
	pairList$testSample <- as.numeric(as.character(pairList$testSample))
	pairList$sampleID <- as.numeric(as.character(pairList$sampleID))

	pairList$newID <- pairList$refSample
	pairList$newID[is.na(pairList$newID)] <- nrefs+seq(1:sum(is.na(pairList$newID)))

	refInfo <- data.frame(sampleID=rep(info$sampleID[which.max(info$npop)],nrefs),
					testSample=seq(1:nrefs),refSample=seq(1:nrefs),newID=seq(1:nrefs))
	pairList <- rbind(pairList,refInfo)

	nNewPops <- sum(is.na(pairList$refSample))
	refSeed <- as.character(refSeed)
	testSet <- as.character(testSet)

	    new("refMap",
    	refSeed=refSeed,
    	testSet=testSet,
    	filtRes=filtRes,
    	nNewPops=nNewPops,
    	pairList=pairList)
    }

