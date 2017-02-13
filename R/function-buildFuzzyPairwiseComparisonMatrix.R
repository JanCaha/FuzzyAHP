#' Function that builds Fuzzy Pairwise Comparison Matrix based on list of Pairwise Comparison Matrices
#'
#' @description
#' This functions builds Fuzzy Pairwise Comparison Matrix based on list of Pairwise Comparison Matrices
#' the resulting Fuzzy Pairwise Comparison Matrix is calculated as minimum, geometric mean and maximum of
#' each cell of all Pairwise Comparison Matrices in \code{listOfMatrices}.
#'
#' @param listOfMatrices An object of \code{\linkS4class{list}}.
#'
#' @return An object of class \code{\linkS4class{FuzzyPairwiseComparisonMatrix}}
#'
#' @export
#' @rdname buildFuzzyPairwiseComparisonMatrix-methods
#' @name buildFuzzyPairwiseComparisonMatrix
setGeneric("buildFuzzyPairwiseComparisonMatrix",
           function(listOfMatrices) standardGeneric("buildFuzzyPairwiseComparisonMatrix"))

#' @rdname buildFuzzyPairwiseComparisonMatrix-methods
#' @aliases buildFuzzyPairwiseComparisonMatrix,list-method
setMethod(
  f="buildFuzzyPairwiseComparisonMatrix",
  signature(listOfMatrices = "list"),
  definition=function(listOfMatrices)
  {
    number = length(listOfMatrices)
    size = nrow(listOfMatrices[[1]]@values)

    for(i in 1:number){
      if (class(listOfMatrices[[i]]) != "PairwiseComparisonMatrix"){
        stop(paste0("Element on position ", i, " is not of class PairwiseComparisonMatrix. Its type is ", class(listOfMatrices[[i]]), "."))
      }

      if (dim(listOfMatrices[[1]]@values)[1] != dim(listOfMatrices[[i]]@values)[1] &&
          dim(listOfMatrices[[1]]@values)[2] != dim(listOfMatrices[[i]]@values)[2]){

        stop(paste0("PairwiseComparisonMatrices do not have the same sizes: [", dim(listOfMatrices[[1]]@values)[1], ",",
                    dim(listOfMatrices[[1]]@values)[2], "] != [", dim(listOfMatrices[[i]]@values)[1], ",",
                    dim(listOfMatrices[[1]]@values)[2], "]."))
      }
    }

    modalValues = matrix(data = 0, nrow = size, ncol = size)
    lowerValues = matrix(data = 0, nrow = size, ncol = size)
    upperValues = matrix(data = 0, nrow = size, ncol = size)

    for (i in 1:size){
      for (j in 1:size){
        values = c()

        for (k in 1:number){
          values = c(values, listOfMatrices[[k]]@values[i, j])
        }

        modalValues[i, j] = prod(values)^(1/number)
        lowerValues[i, j] = min(values)
        upperValues[i, j] = max(values)
      }
    }

    return(new("FuzzyPairwiseComparisonMatrix", fnMin = lowerValues, fnModal = modalValues,
               fnMax = upperValues, variableNames = listOfMatrices[[1]]@variableNames))
  }
)
