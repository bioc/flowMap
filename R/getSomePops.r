#' Make prefiltering matrix for each test sample
#'
#' This method select the population pairs (within each test population) that are the most similar by centroid distance (Eudlidean).
#'
#' @param stats an object of SampleInfo class
#' @param refCentroidDir directory where the centroid file of the reference samples are located
#' @param nPopFilt number of reference populations to be compared for each test sample population
#' @return an object of somePops class.
#'
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#'
#' @export
getSomePops <- function(stats,refCentroidDir,nPopFilt)
	{
	if (is.na(file.info(refCentroidDir)$isdir) ) { 
		refCentroidFile <- paste(stats@centroidDir,"/",as.character(refCentroidDir),sep="")
		refCentroid <- read.table(refCentroidFile,header=F,sep="\t")
	} else if ( (file.info(refCentroidDir)$isdir==FALSE) ) {
		refCentroidFile <- paste(stats@centroidDir,"/",as.character(refCentroidDir),sep="")
		refCentroid <- read.table(refCentroidFile,header=F,sep="\t")
	} else if (  (file.info(refCentroidDir)$isdir==TRUE) ) {
		refCentroidFile <- list.files(path=refCentroidDir,pattern="*.txt",include.dirs=F,full.names=TRUE)
		refCentroid <- read.table(refCentroidFile,header=F,sep="\t")
	}
	rownames(refCentroid) <- refCentroid[,1]
	refCentroid <- refCentroid[,-1]
	
	info <- getSampleInfo(stats)
	testSet <- info$centroidNames
	sampleIDs <- info$sampleID
	centroidDir <- stats@centroidDir

	iiTest <- lapply(1:length(testSet),function(i) {
		centroidFile <- as.character(testSet[i])
		sampleID <- sampleIDs[i]
		
		data <- read.table(paste(centroidDir,"/",centroidFile,sep=""),header=F,sep="\t")
		rownames(data) <- data[,1]
		data <- data[,-1]

		distmat <- as.matrix(dist(rbind(data,refCentroid),diag=T,upper=T))
		distmat <- distmat[1:nrow(data),(nrow(data)+1):(nrow(data)+nrow(refCentroid))]

		colnames(distmat) <- rownames(refCentroid)
		rownames(distmat) <- rownames(data)	

 		ordMat <- apply(distmat,1,function(x) as.numeric(colnames(distmat)[order(x)[c(1:nPopFilt)]]) )
 		# ordMat <- which((distmat < quantile(distmat,prob=0.20))==TRUE,arr.ind=T)
 		# ordMat <- ordMat[order(ordMat[,1]),]

 		mat <- data.frame(sampleID=sampleID,sp=rownames(data),t(ordMat))
 		mat
 	})
	    new("somePops",
    	nPopFilt=nPopFilt,
    	refCentroid=refCentroidFile,
    	filtList=iiTest)
}


