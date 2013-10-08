.printHead <- function(x)
  {
    n <- nrow(x)
    if (n>20) {
      print(x[1:20,])
      cat(n-20, "more lines ...\n")
    } else {
      print(x)
    }
  }


setMethod("show","FRstats",function(object) { 
            cat("FR statistics object on a",object@npop1,"by",object@npop2,"two-sample comparison\n")
            cat("Based on",object@draws,"random draws\n")
            # # .printHead(object@normalTissues)
            cat("with", object@sampleMethod, "sampling method\n")
            # # .printHead(object@cancerTissues)
            cat("of", object@sampleSize, "events from each group\n")
          })


setMethod("show","FRvalsPerm",function(object) { 
            cat("FR statistics from ",object@nperms,"permuations \n")
            cat("with", object@sampleMethod, "sampling method\n")
            cat("of", object@sampleSize, "events from each group\n")
          })


setMethod("show","matchMat",function(object) { 
            cat("matched Populations (test sample by reference sample) \n")
            show(object@matchPairs)
          })



setMethod("show","somePops",function(object) { 
            cat("Total of",length(object@filtList),"samples \n")
            cat("For each sample population, find the reference populations that are likely to be similar \n")
            cat("top",object@nPopFilt,"most similar by Euclidean distance are selected \n")
            cat("below prints the matching list for the first sample \n")
            show(object@filtList[[1]])            
          })


setMethod("show","crossMap",function(object) { 
            cat("population matching results \n")
            cat(object@nNewPops,"new populations are identified \n")
            .printHead(object@pairList)
          })


setMethod("show","refMap",function(object) { 
            cat("population matching results \n")
            cat(object@nNewPops,"new populations are identified \n")
            .printHead(object@pairList)
          })

setMethod("show","refSample",function(object) { 
            cat("Combined data of matched and mismatched populations\n")
            cat(object@refSeed,"was used as the reference in matching cell populations\n")
            cat(object@nNewPops,"new populations are added\n")
          })


setMethod("show","SampleInfo",function(object) { 
            cat("Sample information summary on ",object@nsamples,"samples\n")
            .printHead(object@info)
          })

