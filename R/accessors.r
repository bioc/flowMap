# Accessor for the FRstats class
#
#' @section Accessors: In the following code \code{object} is a \code{FRstats} object. \describe{
#'   \item{\code{getFRstats}:}{matrix of median FR statistics across random draws}
#'   \item{\code{getPnorm}:}{matrix of median theoretical null p-values across random draws}
#' }
#' 
#' @param object Object of class \code{\linkS4class{FRstats}}
#' @aliases getFRstats,FRstats-method
#' @rdname frstats-class
#' @export
setMethod(getFRstats,signature=c(object="FRstats"),
          function(object) {
            return(object@ww)
          })

#' @aliases getPnorm,FRstats-method
#' @rdname frstats-class
#' @export
setMethod(getPnorm,signature=c(object="FRstats"),
          function(object) {
            return(object@pNorm)
          })


# Accessor for the FRvalsPerm class
#
#' @section Accessors: In the following code \code{object} is a \code{FRvalsPerm} object. \describe{
#'   \item{\code{getPnulls}:}{matrix of empirical null p-values (test sample by reference sample)}
#'   \item{\code{getWnulls}:}{matrix of the permuted FR statistics (used to calculate the empirical null p-values)}
#' }
#' 
#' @param object Object of class \code{\linkS4class{FRvalsPerm}}
#' @aliases getPnulls,FRvalsPerm-method
#' @rdname frvalsperm-class
#' @export
setMethod(getPnulls,signature=c(object="FRvalsPerm"),
          function(object) {
            object@pnulls
          })
#' @aliases getWnulls,FRvalsPerm-method
#' @rdname frvalsperm-class
#' @export
setMethod(getWnulls,signature=c(object="FRvalsPerm"),
          function(object) {
            object@wnulls
          })



# Accessor for the SampleInfo class
#
#' @section Accessors: In the following code \code{object} is a \code{SampleInfo} object. \describe{
#'   \item{\code{getSampleInfo}:}{a \code{data.frame} with columns
#'    \describe{
#'    \item{\code{sampleID}:}{unique identifying number for the inputted sample files}
#'    \item{\code{sampleNames}:}{names of the inputted sample files}
#'    }
#'  }
#' }
#' 
#' @param object Object of class (or inheriting from) \code{\linkS4class{SampleInfo}}
#' @aliases getSampleInfo,SampleInfo-method
#' @rdname sampleinfo-class
#' @export
setMethod(getSampleInfo,signature=c(object="SampleInfo"),
          function(object) {
            object@info
          })





# Accessor for the matchMat class
#
#' @section Accessors: In the following code \code{object} is a \code{matchMat} object. \describe{
#'   \item{\code{getMatchedPairs}:}{a matrix of matched population labels with columns
#'    \describe{
#'        \item{\code{testSample}:}{all population labels in the test sample}
#'        \item{\code{refSample}:}{matched reference sample population label}
#'    }
#'  }
#'   \item{\code{getMatchMat}:}{a matrix of 0 and 1 entires indexing matched/mismatched population pairs} 
#' }
#' 
#' @param object Object of class \code{\linkS4class{matchMat}}
#' @aliases getMatchPairs,matchMat-method
#' @rdname matchmat-class
#' @export
setMethod(getMatchPairs,signature=c(object="matchMat"),
          function(object) {
            object@matchPairs
          })

#' @aliases getMatchMat,matchMat-method
#' @rdname matchmat-class
#' @export
setMethod(getMatchMat,signature=c(object="matchMat"),
          function(object) {
            object@matchMat
          })







# Accessor for the refSample class
#
#' @section Accessors: In the following code \code{object} is a \code{refSample} object. \describe{
#'   \item{\code{getNewPops}:}{an integer, number of new populations added after the mapping process}
#'   \item{\code{getRefData}:}{a data.frame of flow cytometry data combinging samples used to create this proxy refernece sample;
#'      the rows are the events/cells, and the columns are the FCM features with the last column being the new population label} 
#'  }
#' 
#' @param object Object of class \code{\linkS4class{refSample}}
#' @aliases getNewPops,refSample-method
#' @rdname refsample-class
#' @export
setMethod(getNewPops,signature=c(object="refSample"),
          function(object) {
            object@nNewPops
          })

#' @aliases getRefData,refSample-method
#' @rdname refsample-class
#' @export
setMethod(getRefData,signature=c(object="refSample"),
          function(object) {
            object@refData
          })









# Accessor for the refMap class
#
#' @section Accessors: In the following code \code{object} is a \code{refMap} object. \describe{
#'   \item{\code{getPairList}:}{a \code{data.frame} with columns
#'     \describe{
#'       \item{\code{SampleID}:}{an unique identifying number for the sample file}
#'       \item{\code{testSample}:}{cell population label in the test sample}
#'       \item{\code{refSample}:}{the match reference population label (\code{NA} if no match)}
#'       \item{\code{newID}:}{the new population label in the proxy reference sample}
#'    }
#'   }
#'  }
#' 
#' @param object Object of class \code{\linkS4class{refMap}}
#' @aliases getPairList,refMap-method
#' @rdname refmap-class
#' @export
setMethod(getPairList,signature=c(object="refMap"),
          function(object) {
            object@pairList
          })









# Accessor for the crossMap class
#
#' @section Accessors: In the following code \code{object} is a \code{crossMap} object. \describe{
#'   \item{\code{getFRmapStats}:}{list of matrices; each contains the observed FR statistics between a test sample and the reference}
#'   \item{\code{getFRmapPnorm}:}{list of matrices; each contains the theoretical p-values between a test sample and the reference} 
#'   \item{\code{getFRmapPnulls}:}{list of matrices; each contains the empirical p-values between a test sample and the reference} 
#'   \item{\code{getCrossList}:}{matrix of test and reference sample population labels. A \code{data.frame} with columns
#'     \describe{
#'       \item{\code{SampleID}:}{an unique identifying number for the sample file}
#'       \item{\code{testSample}:}{cell population label in the test sample}
#'       \item{\code{refSample}:}{the match reference population label (\code{NA} if no match)}
#'       \item{\code{newID}:}{the new population label in the proxy reference sample}
#'    }
#'   } 
#'  }
#' 
#' @param object Object of class \code{\linkS4class{crossMap}}
#' @aliases getFRmapStats,crossMap-method
#' @rdname crossmap-class
#' @export
setMethod(getFRmapStats,signature=c(object="crossMap"),
      function(object) {
      object@WWobs
      })

#' @aliases getFRmapPnorms,crossMap-method
#' @rdname crossmap-class
#' @export
setMethod(getFRmapPnorms,signature=c(object="crossMap"),
      function(object) {
      object@Pnorms
      })

#' @aliases getFRmapPnulls,crossMap-method
#' @rdname crossmap-class
#' @export
setMethod(getFRmapPnulls,signature=c(object="crossMap"),
      function(object) {
      object@Pnulls
      })

#' @aliases getCrossList,crossMap-method
#' @rdname crossmap-class
#' @export
setMethod(getCrossList,signature=c(object="crossMap"),
          function(object) {
            object@pairList
          })







# Accessor for the somePops class
#
#' @section Accessors: In the following code \code{object} is a \code{somePops} object. \describe{
#'   \item{\code{getPopFilt}:}{a \code{list} with matrices as list elments. Each matrix contains labels of population pairs that
#'        are selected via prefiltering process.
#'  }
#' }
#' 
#' @param object Object of class \code{\linkS4class{somePops}}
#' @aliases getPopFilt,somePops-method
#' @rdname somepops-class
#' @export
setMethod(getPopFilt,signature=c(object="somePops"),
          function(object) {
            object@filtList
          })
































