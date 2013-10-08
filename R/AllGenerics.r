#' @export
setGeneric("getFRstats",
           function(object) standardGeneric("getFRstats"),
           signature=c("object"))

#' @export
setGeneric("getPnorm",
           function(object) standardGeneric("getPnorm"),
           signature=c("object"))

#' @export
setGeneric("getPnulls",
           function(object) standardGeneric("getPnulls"),
           signature=c("object"))
#' @export
setGeneric("getWnulls",
           function(object) standardGeneric("getWnulls"),
           signature=c("object"))

#' @export
setGeneric("getSampleInfo",
           function(object) standardGeneric("getSampleInfo"),
           signature=c("object"))

#' @export
setGeneric("getMatchPairs",
           function(object) standardGeneric("getMatchPairs"),
           signature=c("object"))
#' @export
setGeneric("getMatchMat",
           function(object) standardGeneric("getMatchMat"),
           signature=c("object"))

#' @export
setGeneric("getNewPops",
           function(object) standardGeneric("getNewPops"),
           signature=c("object"))
#' @export
setGeneric("getRefData",
           function(object) standardGeneric("getRefData"),
           signature=c("object"))

#' @export
setGeneric("getPairList",
           function(object) standardGeneric("getPairList"),
           signature=c("object"))

#' @export
setGeneric("getFRmapStats",
      function(object) standardGeneric("getFRmapStats"),
      signature=c("object"))
#' @export
setGeneric("getFRmapPnorms",
      function(object) standardGeneric("getFRmapPnorms"),
      signature=c("object"))
#' @export
setGeneric("getFRmapPnulls",
      function(object) standardGeneric("getFRmapPnulls"),
      signature=c("object"))
#' @export
setGeneric("getCrossList",
           function(object) standardGeneric("getCrossList"),
           signature=c("object"))

#' @export
setGeneric("getPopFilt",
           function(object) standardGeneric("getPopFilt"),
           signature=c("object"))







# #' Compute statistics for the empirical distribution
# #'
# #' This method permutes the observed FR statistics for each cross-sample comparison and creates an empirical 
# #' null distribution of the FR statistics on which empirical values are decided.
# #'  
# #' @param stats an object of class FRstats as produced by the getFRest function 
# #' @param nperms number of permutations required to create the empirical distribution of the FR statistic
# #' @param iiTest prefiltering matrix indicating which populaiton pairs to perform testing on
# #' @param ncores number of processing cores to be used during computatino
# #' @return a object of class FRvalsPerms 
# #'
# #' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
# #' 
# #' @export
# #' @docType methods
# #' @rdname getFRvalsPerm-methods
# setGeneric("getFRvalsPerm",
#   function(stats,nperms,iiTest,ncores,...) standardGeneric("getFRvalsPerm"),
#   signature=c("stats"))








# #' Identify matched population pairs
# #'
# #' This method applies a prespecified cutoff to the empirical p-values to identify the matched populations.
# #'  
# #' @param stats an oject of FRvalsPerm class 
# #' @param cutoff cutoff for identifying significant p-values, i.e., which population pairs matched/mismatched
# #' @return an object of class matchMat containing matched population pair labels
# #'
# #' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
# #'
# #' @export
# #' @docType methods
# #' @rdname getMatched-methods
# setGeneric("getMatched",
#   function(stats,cutoff,...) standardGeneric("getMatched"),
#   signature=c("stats"))




# #' Make prefiltering matrix for each test sample
# #'
# #' This method select the population pairs (within each test population) that are the most similar by centroid distance (Eudlidean).
# #'
# #' @param stats an object of SampleInfo class
# #' @param refCentroidDir directory where the centroid file of the reference samples are located
# #' @param nPopFilt number of reference populations to be compared for each test sample population
# #' @return an object of somePops class.
# #'
# #' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
# #'
# #' @export
# #' @docType methods
# #' @rdname getSomePops-methods
# setGeneric("getSomePops",
#   function(stats,refCentroidDir,nPopFilt,...) standardGeneric("getSomePops"),
#   signature=c("stats","refCentroidDir","nPopFilt"))




# #' Make mapping information for the meta set of populations
# #'
# #' Make a referecen sample from a subset of the sample files. Map all sample files to the reference. 
# #' Create an summary file of the sample file names, sample file IDs, reference sample IDs, 
# #' original subpopulation IDs in each sample, and the corresponding new subpopulation IDs.
# #'
# #' @param stats an object of SampleInfo class
# #' @param refDataDir directory where the user-defined sample file is located
# #' @param filtRes list of matrices containing prefiltering population pairs for each cross-sample comparison
# #' @param draws number of random draws to compute estimated FR statistics for a population pair comparison
# #' @param sampleMethod sampling scheme for each population comparison
# #' @param sampleSize number of events to draw from each population in a population pair comparison
# #' @param nperms number of permutations required to make the empirical null distribution
# #' @param ncores number of processing cores requested
# #' @return an object of crossMap class.
# #'
# #' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
# #'
# #' @export
# #' @docType methods
# #' @rdname makeCrossMap-methods
# setGeneric("makeCrossMap",
#   function(stats,refDataDir,filtRes,draws,sampleMethod,sampleSize,nperms,ncores,...) standardGeneric("makeCrossMap"),
#   signature=c("stats"))




# #' Make mapping information for the proxy reference sample
# #'
# #' This function performs multiple sample comparison among a set of specified sample files and then creates mapping
# #' information for proxy reference sample accordingly.
# #'
# #' @param stats an object of SampleInfo class
# #' @param draws number of random draws to compute estimated FR statistics for a population pair comparison
# #' @param nPopFilt number of reference populations to be compared for a test population
# #' @param cutoff p-value cutoff for matched/mismatched population pairs
# #' @param sampleMethod sampling scheme for each population comparison
# #' @param sampleSize number of events to draw from each population in a population pair comparison
# #' @param nperms number of permutations required to make the empirical null distribution
# #' @param ncores number of processing cores requested
# #' @return an object of refMap class
# #'
# #' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
# #' @export
# #' @docType methods
# #' @rdname makeRefMap-methods
# setGeneric("makeRefMap",
#   function(stats,draws,nPopFilt,cutoff,sampleMethod,sampleSize,nperms,ncores,...) standardGeneric("makeRefMap"),
#   signature=c("stats"))




# #' Make a proxy reference sample
# #'
# #' This function appiles the refMap method and generates a proxy reference sample accordingly.
# #'
# #' @param stats an object of SampleInfo class
# #' @param draws number of random draws to compute estimated FR statistics for a population pair comparison
# #' @param nPopFilt number of reference populations to be compared for a test population
# #' @param cutoff p-value cutoff for matched/mismatched population pairs
# #' @param sampleMethod sampling scheme for each population comparison
# #' @param sampleSize number of events to draw from each population in a population pair comparison
# #' @param nperms number of permutations required to make the empirical null distribution
# #' @param ncores number of processing cores requested
# #' @return an object of refSample class
# #'
# #' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
# #' @docType methods
# #' @rdname makeRefSample-methods
# setGeneric("makeRefSample",
#   function(stats,draws,nPopFilt,cutoff,sampleMethod,sampleSize,nperms,ncores,...) standardGeneric("makeRefSample"),
#   signature=c("stats"))
























