#' Function to determine Consistency Ratio
#'
#' @description
#' This methods calculates Consistency Ratio for \code{\linkS4class{PairwiseComparisonMatrix}}.
#' The consistency ratio can only be provided for \code{\linkS4class{PairwiseComparisonMatrix}} with
#' less than 10 rows. For bigger matrices the value is not known.
#'
#' @param comparisonMatrix A \code{\linkS4class{PairwiseComparisonMatrix}}
#'
#' @return A numeric value of Consistency Ratio, for \code{\linkS4class{PairwiseComparisonMatrix}} with more than 10
#' rows a value of Consistency index, that needs to be further evaluated.
#'
#' @export
#'
#' @include class-PairwiseComparisonMatrix.R
#'
setGeneric("consistencyRatio",
           function(comparisonMatrix) standardGeneric("consistencyRatio"))

setMethod(
  f="consistencyRatio",
  signature(comparisonMatrix = "PairwiseComparisonMatrix"),
  definition=function(comparisonMatrix)
  {
    randomIndex = c(0, 0, 0.52, 0.89, 1.11, 1.25, 1.35, 1.4, 1.45, 1.49)

    CI = (Re(eigen(comparisonMatrix@values)$values[1]) - nrow(comparisonMatrix@values)) / (nrow(comparisonMatrix@values) - 1)

    if(nrow(comparisonMatrix@values)<11){
      CR = CI / (randomIndex[nrow(comparisonMatrix@values)])

      if(CR<0.1){
        cat(paste("Consistency ratio is: ", CR, ". The pairwise comparison matrix is consistent for calculations.", sep=""))
        cat("\n")
      }else{
        warning(paste("Consistency ratio is: ", CR, ". It should be lower than: ", randomIndex[nrow(comparisonMatrix@values)],
                      ". The pairwise comparison matrix is not consistent enough for correct calculations. Please consider redefining the matrix!",
                      sep = ""),call. = FALSE)
      }
      return(CR)

    }else{
      warning(paste("Cannot calculate Consistency Ratio for matrices with more then 10 rows! The Consistency index is ",
                    CI, ".", sep = ""), call. = FALSE)
      return(CI)
    }
  }
)
