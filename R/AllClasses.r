#' FR statistics and p-values generated from one single draw of the population pair comparison
#'
#' This class stores the statistics required to compute median FR statistics across random draws.
#'
#' @section FIXME:
#' ## usage
#' ## Accessors
#' getFRstats(object)
#' getPnorm(object)
#'
#' @name FRstats-class
#' @rdname frstats-class
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#' @examples
#' ## see vignettes
#'
#' @export
setClass("FRstats",
         representation=representation(XX1="data.frame",
           XX2="data.frame",
           sampleMethod="character",
           sampleSize="numeric",
           ncores="numeric",
           draws="numeric",
           npop1="numeric",
           npop2="numeric",
           pop1Labels="numeric",
           pop2Labels="numeric",
           ww="matrix",
           runs="matrix",
           mu="matrix",
           sigma2="matrix",
           pNorm="matrix"))



#' Statistics generated from the permuted distribution of the observed FR statistics 
#'
#' This class stores the statistics required to decide whether a population pair is matched or not.
#'
#' @seealso \code{\linkS4class{FRstats}} for the class of objects from which the observed FR statistics are calculated. \cite{\link{getMatched}}
#'   is the method used to identify the matched pairs.
#'
#' @section FIXME:
#' ## usage
#' ## Accessors
#' getPnulls(object)
#' getWnull(object)
#'
#' @name FRvalsPerm-class
#' @rdname frvalsperm-class
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#' @examples
#' ## see vignettes
#'
#' @export
setClass("FRvalsPerm",
         representation=representation(sampleMethod="character",
           sampleSize="numeric",
           ncores="numeric",
           wnulls="array",
           pnulls="matrix",
           nperms="numeric"))



#' List of directories and sample files.
#'
#' This class stores the information necessary to uniquly identify each test sample.
#'
#' @section FIXME:
#' ## usage
#' ## Accessors
#' getSampleInfo(object)
#'
#' @name SampleInfo-class
#' @rdname sampleinfo-class
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#' @examples
#' ## see vignettes
#'
#' @export
setClass("SampleInfo",
         representation=representation(sampleDir="character",
           centroidDir="ANY",
           nsamples="numeric",
           info="data.frame"))



#' Matching information about a single cross-sample comparison.
#'
#' This class stores the information required to identify matched/mismatched cell population comparisons.
#'
#' @section FIXME:
#' ## usage
#' ## Accessors
#' getMatchPairs(object)
#' getMatchMat(object)
#'
#' @name matchMat-class
#' @rdname matchmat-class
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#' @examples
#' ## see vignettes
#'
#' @export
setClass("matchMat",
         representation=representation(cutoff="numeric",
           iiMatch="matrix",
           matchPairs="matrix",
           matchMat="matrix"))




#' Data and summary information of the proxy reference sample
#'
#' This class stores the information used to generate a proxy reference sample as well as the resultant sample data.
#'
#' @section FIXME:
#' ## usage
#' ## Accessors
#' getNewPops(object)
#' getRefData(object)
#'
#' @name refSample-class
#' @rdname refsample-class
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#' @examples
#' ## see vignettes
#'
#' @export
setClass("refSample",
        representation=representation(refSeed="character",
      testSet="character",
      filtRes="ANY",
      nNewPops="numeric",
      pairList="data.frame",
      refData="data.frame"))



#' Information required to making a proxy reference sample
#'
#' This class stores the information used to generate a proxy reference sample.
#'
#' @section FIXME:
#' ## usage
#' ## Accessors
#' getPairList(object)
#'
#' @name refMap-class
#' @rdname refmap-class
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#' @examples
#' ## see vignettes
#'
#' @export
setClass("refMap",
        representation=representation(refSeed="character",
      testSet="character",
      filtRes="ANY",
      nNewPops="numeric",
      pairList="data.frame"))




#' Statistics of mapping the test samples and the reference sample (or the proxy reference)
#'
#' This class stores statistics of multiple-sample comparison.
#'
#' @section FIXME:
#' ## usage
#' ## Accessors
#' getFRmapStats(object)
#' getFRmapPnorms(object)
#' getFRmapPnulls(object)
#' getCrossList(object)
#'
#' @name crossMap-class
#' @rdname crossmap-class
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#' @examples
#' ## see vignettes
#'
#' @export
setClass("crossMap",
        representation=representation(refData="ANY",
      testSet="ANY",
      filtRes= "ANY",
      nNewPops="numeric",
      pairList="data.frame",
      Pnulls="list",
      Pnorms="list",
      WWobs="list",
      Wnulls="list"))


#' List of population pairs to be compared during multiple-sample comparisons
#'
#' This class stores the population pairs selected after the prefilteirng process.
#' 
#' @section FIXME:
#' ## usage
#' ## Accessors
#' getPopFilt(object)
#'
#' @name somePops-class
#' @rdname somepops-class
#' @author Chiaowen Joyce Hsiao \email{joyce.hsiao1@@gmail.com}
#' @examples
#' ## see vignettes
#'
#' @export
setClass("somePops", representation=representation(nPopFilt="numeric",
    refCentroid="character",
    filtList="list"))

