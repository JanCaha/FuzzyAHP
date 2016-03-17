#' Class "FuzzyPairwiseComparisonMatrix"
#' @description  An S4 class to represent a fuzzy pairwise comparison matrix.
#'
#' @slot fnMin A matrix of minimal values of fuzzy preferences.
#' @slot fnModal A matrix of modal values of fuzzy preferences.
#' @slot fnMax A matrix of maximal values of fuzzy preferences.
#'
#' @export
#' @include class-PairwiseComparisonMatrix.R
#'
setClass(
  Class="FuzzyPairwiseComparisonMatrix",

  representation(
    fnMin = "matrix",
    fnModal = "matrix",
    fnMax = "matrix"
  )
)





#'  Function that creates Fuzzy Pairwise Comparions Matrix
#'
#' @description
#' This methods construct object \linkS4class{FuzzyPairwiseComparisonMatrix} based on provided
#' \linkS4class{PairwiseComparisonMatrix} and an optional fuzzy scale.
#'
#' @details
#' Parameter fuzzyScale is expected as a vector containing n*3 values that represent triangular fuzzy
#' numbers used as fuzzy intensity of importance (only the values equal or higher than 1, inverse values
#' are calculated automatically). The values need to be ordered by fuzzy values. Default value of this
#' parameter is \code{as.double(c(1/2,1,2,1,2,3,2,3,4,3,4,5,4,5,6,5,6,7,6,7,8,7,8,9,8,9,9))}. Another
#' possibility is eg. \code{as.double(c(1/3,1,3,1,3,5,3,5,7,5,7,9,7,9,9))}
#'
#' @param pairwiseComparisonMatrix \linkS4class{PairwiseComparisonMatrix}.
#' @param ... Place to pass fuzzyScale
#'
#' @usage FuzzyPairwiseComparisonMatrix(pairwiseComparisonMatrix, ...)
#'
#' @return Object of class \linkS4class{FuzzyPairwiseComparisonMatrix}
#'
#' @export
#'
#' @include class-PairwiseComparisonMatrix.R
#'
setGeneric("FuzzyPairwiseComparisonMatrix",
           function(pairwiseComparisonMatrix, ...) standardGeneric("FuzzyPairwiseComparisonMatrix"))

setMethod(
  f="FuzzyPairwiseComparisonMatrix",
  signature(pairwiseComparisonMatrix = "PairwiseComparisonMatrix"),
  definition=function(pairwiseComparisonMatrix, fuzzyScale = as.double(c(1/2,1,2,
                                                                         1,2,3,
                                                                         2,3,4,
                                                                         3,4,5,
                                                                         4,5,6,
                                                                         5,6,7,
                                                                         6,7,8,
                                                                         7,8,9,
                                                                         8,9,9))
                      )
  {

    if(!(length(fuzzyScale)%%3==0)){
      stop(paste("The fuzzy scale lenght has to be x*3. This fuzzy scale does not fulfill this condition."))
    }

    size = nrow(pairwiseComparisonMatrix@values)
    # prepare 3 matrices for fuzzy values, each hase size of the original matrix
    fnMin = matrix(data = 0, nrow = size, ncol = size)
    fnModal = matrix(data = 0, nrow = size, ncol = size)
    fnMax = matrix(data = 0, nrow = size, ncol = size)

    # diagonal fuzzy values are crips ones
    v1diagonal = c(1,1,1)

    # transfor fuzzy scale into matrix
    #valuesMatrix = matrix(data = fuzzyScale, nrow = 9, ncol = 3, byrow = TRUE)
    valuesMatrix = matrix(data = fuzzyScale, nrow = length(fuzzyScale)/3, ncol = 3, byrow = TRUE)


    # prepare matrix of inverted fuzzy values from the scale
    invertedValuesMatrix = matrix(data = 1.0, nrow = length(fuzzyScale)/3, ncol = 3, byrow = TRUE ) / valuesMatrix
    invertedValuesMatrix = cbind(invertedValuesMatrix[,3], invertedValuesMatrix[,2], invertedValuesMatrix[,1])

    for (i in 1:size){
      for (j in 1:size){

        charIntensity = pairwiseComparisonMatrix@valuesChar[i,j]

        if (i==j & charIntensity == "1"){
          fnMin[i,j] = v1diagonal[1]
          fnModal[i,j] = v1diagonal[2]
          fnMax[i,j] = v1diagonal[3]
        }
        else if (nchar(charIntensity)==3 & substr(charIntensity,1,2)=="1/"){
          number = which(valuesMatrix[,2] == as.integer(substr(charIntensity,3,4))) #as.integer(substr(charIntensity,3,4))

          if(length(number)==0){
            stop(paste("Value ",charIntensity," does not exist in fuzzy scale!", sep = ""))
          }

          fnMin[i,j] = invertedValuesMatrix[number,1]
          fnModal[i,j] = invertedValuesMatrix[number,2]
          fnMax[i,j] = invertedValuesMatrix[number,3]
        }
        else if (nchar(charIntensity)==1){
          number = which(valuesMatrix[,2] == as.integer(substr(charIntensity,1,2))) #as.integer(substr(charIntensity,1,2))

          if(length(number)==0){
            stop(paste("Value ",charIntensity," does not exist in fuzzy scale!", sep = ""))
          }

          fnMin[i,j] = valuesMatrix[number,1]
          fnModal[i,j] = valuesMatrix[number,2]
          fnMax[i,j] = valuesMatrix[number,3]
        }
        else{
          stop("This should never happen. Error in format of pairwise comparison matrix.")
        }
      }
    }

    return(new("FuzzyPairwiseComparisonMatrix", fnMin = fnMin, fnModal = fnModal, fnMax = fnMax))
  }
)
