#' Function to create text representation of (Fuzzy) Pairwise comparison matrix
#'
#' @description
#' This methods creates text representation of Pairwise comparison matrices.
#'
#' @param x An object of class \code{\linkS4class{FuzzyPairwiseComparisonMatrix}}
#' or \code{\linkS4class{PairwiseComparisonMatrix}}
#' @param whole A boolean object specifying if the whole matrix should be created
#' or only its upper half should be filled.
#'
#' @return A \code{dataframe} of \code{character} type.
#'
#' @export
#' @importFrom MASS fractions
#' @rdname textRepresentation-methods
#' @name textRepresentation
setGeneric("textRepresentation",
           function(x, whole=TRUE) standardGeneric("textRepresentation"))



#' @rdname textRepresentation-methods
#' @aliases textRepresentation,FuzzyPairwiseComparisonMatrix,logical-method
setMethod(
  f="textRepresentation",
  signature(x = "FuzzyPairwiseComparisonMatrix"),
  definition=function(x, whole)
  {
    rows = nrow(x@fnMin)

    matrix = matrix("", nrow = rows, ncol = rows)

    for (i in 1:rows){
      for (j in 1:rows){

          min = as.character(fractions(x@fnMin[i, j]))
          mod = as.character(fractions(x@fnModal[i, j]))
          max = as.character(fractions(x@fnMax[i, j]))

        matrix[i,j] = gsub( " ", "", paste0("(", min, ";", mod, ";", max, ")") )
      }
    }

    if(!whole){
      matrix[lower.tri(matrix)] = ""
    }

    data = as.data.frame(matrix, stringsAsFactors=FALSE)

    if(length(x@variableNames) == rows){
      colnames(data) = x@variableNames
      row.names(data) = x@variableNames
    }

    return(data)
  }
)



#' @rdname textRepresentation-methods
#' @aliases PairwiseComparisonMatrix,logical-method
setMethod(
  f="textRepresentation",
  signature(x = "PairwiseComparisonMatrix"),
  definition=function(x, whole)
  {
      rows = nrow(x@values)

      matrix = matrix("", nrow = rows, ncol = rows)

      for (i in 1:rows){
        for (j in 1:rows){

          matrix[i,j] = gsub( " ", "",  as.character(fractions(x@values[i, j])))
        }
      }

      if(!whole){
        matrix[lower.tri(matrix)] = ""
      }

      data = as.data.frame(matrix, stringsAsFactors=FALSE)

      if(length(x@variableNames) == rows){
        colnames(data) = x@variableNames
        row.names(data) = x@variableNames
      }

      return(data)
  }
)
