#' Make mapping information for the meta set of populations
#'
#' Make a referecen sample from a subset of the sample files. Map all sample files to the reference. 
#' Create an summary file of the sample file names, sample file IDs, reference sample IDs, 
#' original subpopulation IDs in each sample, and the corresponding new subpopulation IDs.
#'
#' @param stats an object of SampleInfo class
#' @param refDataDir directory where the user-defined sample file is located
#' @param filtRes list of matrices containing prefiltering population pairs for each cross-sample comparison
#' @param draws number of random draws to compute estimated FR statistics for a population pair comparison
#' @param sampleMethod sampling scheme for each population comparison
#' @param sampleSize number of events to draw from each population in a population pair comparison
#' @param nperms number of permutations required to make the empirical null distribution
#' @return an object of crossMap class.
#'
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#'
#' @export
makeCrossMap <- function(stats,refDataDir,filtRes,draws,cutoff,sampleMethod,sampleSize,nperms)
	{
	info <- stats@info
	sampleDir <- stats@sampleDir

	testSet <- info$sampleNames
	sampleID <- info$sampleID

	if (is.data.frame(refDataDir)==FALSE) {
		refDataName <- list.files(path=refDataDir,pattern="*.txt",include.dirs=F,full.names=TRUE)
		refData <- read.table(refDataName,header=T,sep="\t")	
	} else {
		refData <- refDataDir
		refDataName <- "referenceProxy"
	}

	XX2 <- refData

	nrefs <- length(unique(XX2$id))

	if (!is.null(filtRes)) { iiTestList <- getPopFilt(filtRes) }
	
	message(paste("cross-samping population matching \n"))

	pairMat <- lapply(1:length(testSet), function(i) {

		message(paste("matching",testSet[i],"\n"))

		XX1 <- read.table(paste(sampleDir,"/",testSet[i],sep=""),header=T,sep="\t")

		if (is.null(filtRes)) {
			IItest <- NULL
		} else {
			IItest <- iiTestList[[i]][,-1]
		}
		FRdata <- getFRest(XX1=XX1,XX2=XX2,iiTest=IItest,draws,sampleMethod="equalSize",
						sampleSize,estStat="median")
#		getPnorm(FRdata)

		FRperm <- getFRvalsPerm(FRdata,nperms=nperms,iiTest=IItest)

#		getWnulls(FRperm)
#		getPnulls(FRperm)

		matchRes <- getMatched(FRperm,cutoff)
		matchPairs <- getMatchPairs(matchRes)
		
		list(pairRes=cbind(sampleID[i],matchPairs),Pnorms=getPnorm(FRdata),Pnulls=getPnulls(FRperm),WWobs=getFRstats(FRdata),Wnulls=getWnulls(FRperm))
	})
	# pairList <- data.frame(do.call(rbind,pairMat))

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

	# refInfo <- data.frame(sampleID=rep(info$sampleID[which.max(info$npop)],nrefs),
	# 				testSample=seq(1:nrefs),refSample=seq(1:nrefs),newID=seq(1:nrefs))
	# pairList <- rbind(pairList,refInfo)

	nNewPops <- sum(is.na(pairList$refSample))
	# refSeed <- as.character(refSeed)
	testSet <- as.character(testSet)

	    new("crossMap",
	    refData=refDataName,
    	testSet=testSet,
    	filtRes=filtRes,
    	nNewPops=nNewPops,
    	pairList=pairList,
    	Pnulls=Pnulls,
    	Pnorms=Pnorms,
    	WWobs=WWobs,
    	Wnulls=Wnulls)
    }
