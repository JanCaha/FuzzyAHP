#' Function to calculate fuzzy weights based on comparison matrix
#'
#' @description This functions calculates \code{\linkS4class{Weights}} or \code{\linkS4class{FuzzyWeights}}
#' based on input pairwise comparison matrix.
#'
#' @param comparisonMatrix object of either \linkS4class{PairwiseComparisonMatrix} or \linkS4class{FuzzyPairwiseComparisonMatrix}
#'
#' @seealso \link{PairwiseComparisonMatrix-class}
#'
#' @export
#' @rdname calculateWeights-methods
#' @name calculateWeights
setGeneric("calculateWeights",
           function(comparisonMatrix) standardGeneric("calculateWeights"))

#' @rdname calculateWeights-methods
#' @aliases calculateWeights,PairwiseComparisonMatrix-method
setMethod(
  f="calculateWeights",
  signature(comparisonMatrix="PairwiseComparisonMatrix"),
  definition=function(comparisonMatrix)
  {
    weightsCount = nrow(comparisonMatrix@values)

    weights = vector(mode = "numeric", length = weightsCount)

    for(i in 1:weightsCount){
      weights[i] = prod(comparisonMatrix@values[i,])^(1/weightsCount)
    }

    sumWeights = sum(weights)

    weights = weights/sumWeights

    rNames = c()
    for (i in 1:length(weights)){
      rNames = append(rNames, paste("w",i, sep = ""))
    }

    names(weights) = rNames

    return (new("Weights", weights = weights))
  }
)

#' @rdname calculateWeights-methods
#' @aliases calculateWeights,FuzzyPairwiseComparisonMatrix-method
setMethod(
  f="calculateWeights",
  signature(comparisonMatrix="FuzzyPairwiseComparisonMatrix"),
  definition=function(comparisonMatrix)
  {
    weightsCount = nrow(comparisonMatrix@fnMin)

    mMin = c()
    mModal = c()
    mMax = c()

    for(i in 1:weightsCount){
      mMin = append(mMin, prod(comparisonMatrix@fnMin[i,])^(1/weightsCount))   #append(mMin, gm_mean(AHPFuzzyMatrix@fnMin[i,]))
      mModal = append(mModal, prod(comparisonMatrix@fnModal[i,])^(1/weightsCount))  #append(mModal, gm_mean(AHPFuzzyMatrix@fnModal[i,]))
      mMax = append(mMax, prod(comparisonMatrix@fnMax[i,])^(1/weightsCount))  #append(mMax, gm_mean(AHPFuzzyMatrix@fnMax[i,]))
    }

    # vzorec 3.11
    wMin = c()
    wModal = c()
    wMax = c()

    for(i in 1:weightsCount){
      wMin = append(wMin, (mMin[i])/(mMin[i] + sum(mMax[-i])))
      wModal = append(wModal, (mModal[i])/(mModal[i] + sum(mModal[-i])))
      wMax = append(wMax, (mMax[i])/(mMax[i] + sum(mMin[-i])))
    }

#     weights = cbind(wMin, wModal, wMax)
#     colnames(weights) = c("weight_min","weight_modal","weight_max")
#
#
#     rNames = c()
#     for (i in 1:weightsCount){
#       rNames = append(rNames, paste("w",i, sep = ""))
#     }
#
#     row.names(weights) = rNames

    return (new("FuzzyWeights", fnMin = wMin, fnModal = wModal, fnMax = wModal))
  }
)
