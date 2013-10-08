#' Prepare background files to downstream data processing.
#'
#' Create a txt files of the location of sample directory, centroid directory, and output directory. 
#' Create a txt file of the sample file information, including the sample file names, identifying number for each 
#' sample file, and the number of subpopulations contained in each sample.
#'
#' @param sampleDir directory where sample files are located
#' @param centroidDir directory where centroid files are located 
#'
#' @name makeSampleInfo
#' @rdname makeSampleInfo
#'
#' @export
makeSampleInfo <- function(sampleDir,centroidDir=NULL) {

	# fixme
	# check if the sample path ends with /, if so, remove it
	sampleNames <- list.files(path=sampleDir,pattern="*.txt",include.dirs=F,full.names=FALSE)
	nsamples <- length(sampleNames)
	
	# fixme
	# need to figure out a way to match up sample files and centroid files
	info <- data.frame(sampleID=seq(1:nsamples),sampleNames=sampleNames)

	if (!is.null(centroidDir)) {
		info$centroidNames <- list.files(path=centroidDir,pattern="*.txt",include.dirs=F,full.names=FALSE)
	}
	
	sampleNamesFull <- list.files(path=sampleDir,pattern="*.txt",include.dirs=F,full.names=TRUE)
	popCount <- sapply(1:length(sampleNamesFull), function(i) {
			yy <- read.table(sampleNamesFull[i],header=TRUE,sep="\t")
			cbind(nrow(yy),length(unique(yy$id)))
	})
	info$N <- popCount[1,]
	info$npop <- popCount[2,]

    new("SampleInfo",
    	sampleDir=sampleDir,
    	centroidDir=centroidDir,
    	nsamples=nsamples,
    	info=info)
}


