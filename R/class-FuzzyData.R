#' Class "FuzzyData"
#'
#' @description An S4 class to represent fuzzy data.
#'
#' @slot fnMin A numeric vector of minimal values of fuzzy data.
#' @slot fnModal A numeric vector of modal values of fuzzy data.
#' @slot fnMax A numeric vector of maximal values of fuzzy data.
#'
#' @export
#' @include class-PairwiseComparisonMatrix.R
setClass(
  Class="FuzzyData",

  representation(
    fnMin = "matrix",
    fnModal = "matrix",
    fnMax = "matrix"
  ),
  validity=function(object)
  {
    if(ncol(object@fnMin)!=ncol(object@fnMax) || ncol(object@fnModal)!=ncol(object@fnMax) || ncol(object@fnMin)!=ncol(object@fnModal)){
      return("The colums do not have the same number of columns.")
    }

    if(nrow(object@fnMin)!=nrow(object@fnMax) || nrow(object@fnModal)!=nrow(object@fnMax) || nrow(object@fnMin)!=nrow(object@fnModal)){
      return("The colums do not have the same number of rows.")
    }

    if(length(which(object@fnMin>object@fnModal)) != 0){
      return("Cannot create fuzzy data set. Minimal and modal values are not aligned correctly.")
    }

    if(length(which(object@fnModal>object@fnMax)) != 0){
      return("Cannot create fuzzy data set. Modal and maximal values are not aligned correctly.")
    }

    return(TRUE)
  }
)

#' Function that creates FuzzyData
#'
#' @description  This methods construct object \code{\linkS4class{FuzzyData}} based on provided \code{matrix}.
#' The matrix needs to be have rows represent individual fuzzy numbers and three colums that
#' represent minimal, modal and maximal value of fuzzy number.
#'
#' @param data A \code{matrix} with 3 colums.
#'
#' @return An object of class \code{\linkS4class{FuzzyData}}
#' @export
#'
#' @seealso \linkS4class{FuzzyData}
#'
#' @usage fuzzyData(data)
#'
setGeneric("fuzzyData",
           function(data) standardGeneric("fuzzyData"))

setMethod(
  f="fuzzyData",
  signature(data = "matrix"),
  definition=function(data)
  {
    if(ncol(data)!=3){
      stop("Input dataset has to have three colums!")
    }

    return(new("FuzzyData", fnMin = matrix[,1], fnModal = matrix[,2], fnMax = matrix[,3]))
  }
)
