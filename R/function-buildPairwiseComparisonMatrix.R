#' Function that builds Pairwise Comparison Matrix based on list of Pairwise Comparison Matrices
#'
#' @description
#' This functions builds Pairwise Comparison Matrix based on list of Pairwise Comparison Matrices
#' the resulting Pairwise Comparison Matrix is calculated as geometric mean of all
#' Pairwise Comparison Matrices in \code{listOfMatrices}.
#'
#' @param listOfMatrices An object of \code{\linkS4class{list}}.
#'
#' @return An object of class \code{\linkS4class{PairwiseComparisonMatrix}}
#'
#' @export
#' @rdname buildPairwiseComparisonMatrix-methods
#' @name buildPairwiseComparisonMatrix
setGeneric("buildPairwiseComparisonMatrix",
           function(listOfMatrices) standardGeneric("buildPairwiseComparisonMatrix"))

#' @rdname buildPairwiseComparisonMatrix-methods
#' @aliases buildPairwiseComparisonMatrix,list-method
setMethod(
  f="buildPairwiseComparisonMatrix",
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

    resultMatrix = listOfMatrices[[1]]@values

    for (i in 1:size){
      for (j in 1:size){
        vector = c()

        for (k in 1:number){
          vector = c(vector, listOfMatrices[[k]]@values[i, j])
        }

        resultMatrix[i, j] = prod(vector)^(1/number)
      }
    }

    textMatrix = .textMatrixRepresentation(resultMatrix)

    return(new("PairwiseComparisonMatrix", valuesChar = textMatrix, values = resultMatrix,
               variableNames = listOfMatrices[[1]]@variableNames))
  }
)
