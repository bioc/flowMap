#' Make a proxy reference sample
#'
#' This function appiles the refMap method and generates a proxy reference sample accordingly.
#'
#' @param stats an object of SampleInfo class
#' @param draws number of random draws to compute estimated FR statistics for a population pair comparison
#' @param nPopFilt number of reference populations to be compared for a test population
#' @param cutoff p-value cutoff for matched/mismatched population pairs
#' @param sampleMethod sampling scheme for each population comparison
#' @param sampleSize number of events to draw from each population in a population pair comparison
#' @param nperms number of permutations required to make the empirical null distribution
#' @return an object of refSample class
#'
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
makeRefSample <- function(stats,draws,nPopFilt,cutoff,sampleMethod,sampleSize,nperms)
	{
	# SampleInfo <- makeSampleInfo(sampleDir,centroidDir)
	iniRes <- makeRefMap(stats,draws,nPopFilt,cutoff,sampleMethod,sampleSize,nperms)
	iniPops <- getPairList(iniRes)

	info <- getSampleInfo(stats)
	sampleNames <- info$sampleNames
	sampleIDs <- info$sampleID

	refData <- lapply(1:nrow(info),function(i) {	
		nn <- as.character(sampleNames[i])
		dd <- sampleIDs[i]

		oldIDs <- iniPops[iniPops$sampleID==dd,"testSample"]
		newIDs <- iniPops[iniPops$sampleID==dd,"newID"]

		# y0 <- read.table(paste(stats@sampleDir,"/",nn,sep=""),header=T,sep="\t")
		yy <- read.table(paste(stats@sampleDir,"/",nn,sep=""),header=T,sep="\t")
		ii <- match(yy$id,oldIDs)
		updateIDs <- newIDs[ii]
		yy$id <- updateIDs
		yy
		})
	refData <- do.call(rbind,refData)

    new("refSample",
    	refSeed=iniRes@refSeed,
    	testSet=iniRes@testSet,
    	filtRes=iniRes@filtRes,
    	nNewPops=iniRes@nNewPops,
    	pairList=iniRes@pairList,
    	refData=refData)
   }


