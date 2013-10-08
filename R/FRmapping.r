#' Multiple sample comparison with a user-defined reference
#'
#' Make a referecen sample from a subset of the sample files. Map all sample files to the reference. 
#' Create an summary file of the sample file names, sample file IDs, reference sample IDs, 
#' original subpopulation IDs in each sample, and the corresponding new subpopulation IDs.
#'
#' @param outputDir directory where the output files will be saved. 
#' @param sampleDir directory where all sample files are located. Note that samples need to be in txt format with 
#'			the cell populations indexed by the last column in the data (which needs to be called id)
#' @param centroidDir directory where all centroid files corresponding to the sample files are located. These 
#'			also need to be in txt forma.
#' @param refDataDir directory where the reference sample file is located.
#' @param refCentroidDir directory where the centroid file for the reference sample is located.
#' @param makeRefSampleDir directory of sample files to be used in making a reference file
#' @param makeRefCentroidDir directory of centroids correspoinding to the sample files in makeRefSampleDir
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
#' @return refData name of the reference sample file
#' @return testSet sample files mapped
#' @return fileRes
#' @return nNewPops number of new populations discovered during mapping.
#' @return pairList mapping results for each sample file (including sample cell population IDs and reference sample IDs)
#' @return Pnulls empirical p-values of FR statistics for each sample file
#' @return Pnorms p-value of FR-statistics for each sample file
#' @return WWobs observed F-R statistic for each smaple file
#' @return Wnulls permuted distributions 
#'
#' @name FRmapping
#'
#' @export
FRmapping <- function(outputDir,sampleDir,centroidDir,
	refDataDir=NULL,refCentroidDir=NULL,
	makeRefSampleDir=NULL,makeRefCentroidDir=NULL,
	nPopFilt=3,draws,cutoff=0.01/30,sampleMethod="equalSize",sampleSize=100,nperms=1000) {

	if (is.null(makeRefSampleDir)) {

	sampleInfo <- makeSampleInfo(sampleDir,centroidDir)

	message("write the sample information to sampleInfo.txt in", outputDir, "\n")

	outputFile <- paste(outputDir,"/","sampleInfo.txt",sep="")
	if (file.exists(outputFile)) { file.remove( outputFile) }
	write.table(getSampleInfo(sampleInfo),file=outputFile,row.names=FALSE,col.names=T,quote=FALSE,sep="\t")

	filtRes <- getSomePops(sampleInfo,refCentroidDir,nPopFilt)

	message("write the prefiltering information to filtering.txt in", outputDir, "\n")
	filtTab <- do.call(rbind,getPopFilt(filtRes))

	outputFile <- paste(outputDir,"/","filtering.txt",sep="")
	if (file.exists(outputFile)) { file.remove( outputFile) }
	write.table(filtTab,file=outputFile,row.names=FALSE,col.names=T,quote=FALSE,sep="\t")

	crossMap <- makeCrossMap(sampleInfo,refDataDir,filtRes,draws,cutoff,sampleMethod,sampleSize,nperms)


	message("write the mapping information to finalMappingInfo.txt in",outputDir,"\n")

	outputFile <- paste(outputDir,"/","finalMappingInfo.txt",sep="")
	if (file.exists(outputFile)) { file.remove( outputFile) }
	write.table(getCrossList(crossMap),file=outputFile,row.names=FALSE,col.names=T,quote=FALSE,sep="\t")

	message("write the updated sample files to",outputDir,"\n")

	sampleFiles <- crossMap@testSet
	mappingInfo <- as.data.frame(getCrossList(crossMap))

	for (i in 1:length(sampleFiles)) {
		sampleInput <- paste(sampleDir,"/",sampleFiles[i],sep="")
		sampleOutput <- paste(outputDir,"/",sampleFiles[i],sep="")
		if (file.exists(sampleOutput)) { file.remove(sampleOutput) }

		inData <- read.table(sampleInput,header=T,sep="\t")
		inData <- as.data.frame(inData)
		outData <- inData

		# iiMap <- mappingInfo$testSample[mappingInfo$sampleID==i]
		# iiReplace <- match(inData$id,iiMap)
		# # iiReplace <- match(inData$id,mappingInfo$testSample)
		# outData$id <- mappingInfo$refSample[iiReplace]

		iiMapIn <- mappingInfo$testSample[mappingInfo$sampleID==i]
		iiMapOut <- mappingInfo$refSample[mappingInfo$sampleID==i]
		iiReplace <- match(inData$id,iiMapIn)
		outData$id <- iiMapOut[iiReplace]

		write.table(outData,sampleOutput,row.names=FALSE,col.names=T,quote=FALSE,sep="\t") 
	}

	    new("crossMap",
	    refData=crossMap@refData,
    	testSet=crossMap@testSet,
    	filtRes=crossMap@filtRes,
    	nNewPops=crossMap@nNewPops,
    	pairList=crossMap@pairList,
    	Pnulls=crossMap@Pnulls,
    	Pnorms=crossMap@Pnorms,
    	WWobs=crossMap@WWobs,
    	Wnulls=crossMap@Wnulls)
	

	} else if (!is.null(makeRefSampleDir)) {

	message("begin to make a reference file \n")

	refSampleInfo <- makeSampleInfo(makeRefSampleDir,makeRefCentroidDir)
	
	# if (!is.null(makeRefCentroidDir)) {
	# 		filtRes <- getSomePops(sampleInfo,makeRefCentroidDir,nPopFilt) 
	# 	} else {
	# 		filtRes <- NULL
	# 	}

	message("from sample files located in", refSampleInfo@sampleDir ,"\n")

	iniRefSample <- makeRefSample(refSampleInfo,draws,nPopFilt,cutoff,sampleMethod,sampleSize,nperms)

	message("write the information of reference making samples to refSampleInfo.txt in", outputDir, "\n")

	outputFile <- paste(outputDir,"/","refSampleInfo.txt",sep="")
	if (file.exists(outputFile)) { file.remove( outputFile) }
	write.table(getSampleInfo(refSampleInfo),file=outputFile,row.names=FALSE,col.names=T,quote=FALSE,sep="\t")

	message("write the prefiltering information during the reference making process to refFiltering.txt in", outputDir, "\n")
	filtTab <- do.call(rbind,getPopFilt(iniRefSample@filtRes))

	outputFile <- paste(outputDir,"/","refFiltering.txt",sep="")
	if (file.exists(outputFile)) { file.remove( outputFile) }
	write.table(filtTab,file=outputFile,row.names=FALSE,col.names=T,quote=FALSE,sep="\t")

	message("reference making completed *O* \n")
	message(getNewPops(iniRefSample),"new populations are generated \n")
	message("saving reference mapping information to",outputDir,"\n")
	message("saving the reference proxy file to",outputDir,"\n")

	outputFileRefMap <- paste(outputDir,"/","makeRefMappingInfo.txt",sep="")
	if (file.exists(outputFileRefMap)) { file.remove( outputFileRefMap) }
	write.table(iniRefSample@pairList,file=outputFileRefMap,row.names=FALSE,col.names=T,quote=FALSE,sep="\t")

	outputFileRefProxy <- paste(outputDir,"/","refProxy.txt",sep="")
	if (file.exists(outputFileRefProxy)) { file.remove( outputFileRefProxy) }
	write.table(getRefData(iniRefSample),file=outputFileRefProxy,row.names=FALSE,col.names=T,quote=FALSE,sep="\t")


	message("mapping reference proxy against all sample files \n")

	iniRefData <- getRefData(iniRefSample)
	sampleInfo <- makeSampleInfo(sampleDir,centroidDir)

	message("write the sample information to sampleInfo.txt in", outputDir, "\n")

	outputFile <- paste(outputDir,"/","sampleInfo.txt",sep="")
	if (file.exists(outputFile)) { file.remove( outputFile) }
	write.table(getSampleInfo(sampleInfo),file=outputFile,row.names=FALSE,col.names=T,quote=FALSE,sep="\t")

	# if (!is.null(centroidDir)) {
	# 	filtRes <- getSomePops(sampleInfo,centroidDir,nPopFilt) 
	# } else {
	# 	filtRes <- NULL
	# }

	# fixme
	# now assume no prefiltring for mapping the new reference with the sample files
	iniCross <- makeCrossMap(sampleInfo,iniRefData,filtRes=NULL,draws,cutoff,sampleMethod,sampleSize,nperms)

	message("write the mapping information to finalMappingInfo.txt in",outputDir,"\n")

	outputFile <- paste(outputDir,"/","finalMappingInfo.txt",sep="")
	if (file.exists(outputFile)) { file.remove( outputFile) }
	write.table(getCrossList(iniCross),file=outputFile,row.names=FALSE,col.names=T,quote=FALSE,sep="\t")

	message("write the updated sample files to",outputDir,"\n")

	sampleFiles <- iniCross@testSet
	mappingInfo <- as.data.frame(getCrossList(iniCross))

	for (i in 1:length(sampleFiles)) {
		sampleInput <- paste(sampleDir,"/",sampleFiles[i],sep="")
		sampleOutput <- paste(outputDir,"/",sampleFiles[i],sep="")
		if (file.exists(sampleOutput)) { file.remove(sampleOutput) }

		inData <- read.table(sampleInput,header=T,sep="\t")
		inData <- as.data.frame(inData)
		outData <- inData

		iiMapIn <- mappingInfo$testSample[mappingInfo$sampleID==i]
		iiMapOut <- mappingInfo$refSample[mappingInfo$sampleID==i]
		iiReplace <- match(inData$id,iiMapIn)
		outData$id <- iiMapOut[iiReplace]

		write.table(outData,sampleOutput,row.names=FALSE,col.names=T,quote=FALSE,sep="\t") 
	}

	    new("crossMap",
	    refData=iniRefSample,
    	testSet=sampleFiles,
    	filtRes=NULL,
    	nNewPops=iniCross@nNewPops,
    	pairList=iniCross@pairList,
    	Pnulls=iniCross@Pnulls,
    	Pnorms=iniCross@Pnorms,
    	WWobs=iniCross@WWobs,
    	Wnulls=iniCross@Wnulls)
	}


}





