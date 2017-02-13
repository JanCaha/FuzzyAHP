#' Class "PairwiseComparisonMatrix"
#'
#' @description An S4 class to represent a pairwise comparison matrix.
#'
#' @slot valuesChar A pairwise comparison matrix based on Saaty's method as characters.
#' @slot values A pairwise comparison matrix based on Saaty's method as numeric.
#' @slot variableNames Names of variables in the pariwise comparison matrix obtained either as colnames or rownames.
#'
#' @export
setClass(
  Class="PairwiseComparisonMatrix",

  slots = c(
    valuesChar = "matrix",
    values = "matrix",
    variableNames = "character"
  ),

  validity=function(object)
  {

    if(nrow(object@valuesChar)!=ncol(object@valuesChar)){
      return(paste("The pairwise comparison matrix is not a square matrix. Dimensions are - ncol = ", ncol(object@valuesChar),
                   ", nrow = ", nrow(object@valuesChar), ".", sep = ""))
    }

    for(i in 1:nrow(object@valuesChar)){
      if(!(object@valuesChar[i,i] == "1")){
        return(paste("The elements on the main diagonal of the pairwise comparison matrix must be equal to 1. Position ",
                     i, ",", i, " is not equal to 1.", sep = ""))
      }
    }

    for(i in 1:nrow(object@values)){
      for(j in i:nrow(object@values)){

        if (i!=j){
          #if (object@values[j,i] != (1/object@values[i,j])){
          if (!isTRUE(all.equal(object@values[j,i], (1/object@values[i,j])))){
            return(paste("The pairwise comparison matrix is not reciprocal. Elements [",i,",",j,
                         "]  and [",j,",",i,"] are not reciprocal. ", object@values[i,j], " is not reciprocal to ",
                         object@values[j,i],". Because ", object@values[i,j], " != ", 1/object@values[j,i],
                         sep = ""))
          }
        }
      }
    }

    if(max(object@values) > 9){
      warning(paste("The maximal value in the pairwise comparison matrix should not be higher than 9, however,",
                   "the value ", max(object@values), " was found.", sep = ""))
    }

    # OK
    return(TRUE)
  }
)





#' Function that creates Pairwise Comparions Matrix
#'
#' @description
#' This methods construct object \code{\linkS4class{PairwiseComparisonMatrix}} based on provided \code{matrix}.
#' The matrix needs to be square and reciprocal with the intensity of importance
#' (comparisons).
#' Since the version 0.6.9 the comparsions can be represented as either characters (e.g. "1", "9", "1/9")
#' or numeric (e.g. 1, 9, 1/9) .
#'
#' @param matrix A reciprocal square matrix with ones on the main diagonal.
#'
#' @return An object of class \code{\linkS4class{PairwiseComparisonMatrix}}
#' @export
#'
#' @seealso \linkS4class{PairwiseComparisonMatrix}
#'
#' @usage pairwiseComparisonMatrix(matrix)
#'
#' @examples
#' comparisonMatrixValues = c("1","9","5","1/9","1","1/3","1/5","3","1")
#' comparisonMatrix = matrix(comparisonMatrixValues, nrow = 3, ncol = 3, byrow = TRUE)
#' matrix = pairwiseComparisonMatrix(comparisonMatrix)
#'
#' comparisonMatrixValues = c(1,9,5,1/9,1,1/3,1/5,3,1)
#' comparisonMatrix = matrix(comparisonMatrixValues, nrow = 3, ncol = 3, byrow = TRUE)
#' matrix = pairwiseComparisonMatrix(comparisonMatrix)
#'
#' @rdname pairwiseComparisonMatrix-methods
#' @name pairwiseComparisonMatrix
#' @importFrom MASS fractions
setGeneric("pairwiseComparisonMatrix",
           function(matrix) standardGeneric("pairwiseComparisonMatrix"))

#' @rdname pairwiseComparisonMatrix-methods
#' @aliases pairwiseComparisonMatrix,matrix-method
setMethod(
  f="pairwiseComparisonMatrix",
  signature(matrix = "matrix"),
  definition=function(matrix)
  {
    if(typeof(matrix)=="character"){

      values = .parseCharacterMatrix(matrix)
      names = .getVariableNames(matrix)
      colnames(matrix) = NULL
      matrix = .textMatrixRepresentation(values)

    }else if(typeof(matrix)=="double"){

      values = .parseDoubleMatrix(matrix)
      names = .getVariableNames(matrix)
      matrix = matrix(as.character(values), nrow = nrow(values), ncol=ncol(values))
      colnames(matrix) = NULL
      matrix = .textMatrixRepresentation(values)
    }

    return(new("PairwiseComparisonMatrix", valuesChar = matrix, values = values,
               variableNames = names))
  }
)

setGeneric(".textMatrixRepresentation",
           function(matrix) standardGeneric(".textMatrixRepresentation"))

setMethod(
  f=".textMatrixRepresentation",
  signature(matrix = "matrix"),
  definition=function(matrix)
  {
    values = matrix(data = "", nrow = nrow(matrix), ncol = ncol(matrix))

    for (i in 1:nrow(matrix)){
      for (j in 1:ncol(matrix)){

        values[i, j] = as.character(fractions(matrix[i, j]))
      }
    }

    return(values)
  }
)

setGeneric(".parseDoubleMatrix",
           function(matrix) standardGeneric(".parseDoubleMatrix"))
setMethod(
  f=".parseDoubleMatrix",
  signature(matrix = "matrix"),
  definition=function(matrix)
  {
    values = matrix(data = 0, nrow = nrow(matrix), ncol = ncol(matrix))

    for (i in 1:nrow(matrix)){
      for (j in 1:ncol(matrix)){

        if(is.na(matrix[i, j]) || matrix[i, j]==0){
          values[i, j] = 0
        }else{
          values[i, j] = matrix[i, j]
        }

      }
    }

    if(all(values[upper.tri(values)] == 0)){
      values[upper.tri(values)] = 1/t(values)[upper.tri(1/t(values))]
    }
    if(all(values[lower.tri(values)] == 0)){
      values[lower.tri(values)] = 1/t(values)[lower.tri(1/t(values))]
    }

    return(values)
  }
)

setGeneric(".parseCharacterMatrix",
           function(matrix) standardGeneric(".parseCharacterMatrix"))
setMethod(
  f=".parseCharacterMatrix",
  signature(matrix = "matrix"),
  definition=function(matrix)
  {
    values = matrix(data = 0, nrow = nrow(matrix), ncol = ncol(matrix))

    for (i in 1:nrow(matrix)){
      for (j in 1:ncol(matrix)){

        cell = gsub(" ", "", matrix[i,j])

        if(cell == "" || is.na(cell)){

          values[i, j] = 0
        }else if(grepl("/", cell)){

          numbers = unlist(strsplit(cell, "/"))
          if(suppressWarnings(is.na(as.numeric(numbers[1]))) || suppressWarnings(is.na(as.numeric(numbers[2])))){
            stop(paste0("Element [", i, ",", j,"] is not a number - ", cell, "."))
          }
          values[i,j] = as.numeric(numbers[1]) / as.numeric(numbers[2])
        }else{

          values[i,j] = as.numeric(cell)
        }
      }
    }

    if(all(values[upper.tri(values)] == 0)){
      values[upper.tri(values)] = 1/t(values)[upper.tri(1/t(values))]
    }
    if(all(values[lower.tri(values)] == 0)){
      values[lower.tri(values)] = 1/t(values)[lower.tri(1/t(values))]
    }

    return(values)
  }
)

# new internal function for obtaining names from matrix
setGeneric(".getVariableNames",
           function(matrix) standardGeneric(".getVariableNames"))


setMethod(
  f=".getVariableNames",
  signature(matrix = "matrix"),
  definition=function(matrix)
  {
    if(length(colnames(matrix))>0){
      variableNames = colnames(matrix)
    }else if(length(rownames(matrix))>0){
      variableNames = rownames(matrix)
    }else{
      variableNames = c()
      for(i in 1:nrow(matrix)){
        variableNames = c(variableNames, paste0("C_",i))
      }
    }

    return(variableNames)
  }
)
