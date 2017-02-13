#' Function to create Fuzzy Scale
#'
#' @description
#' This methods creates fuzzy scale that are used while fuzzifying Piecewise comparison matrix.
#'
#' Full scale is:
#' \tabular{ccc}{
#' 1/2 \tab 1 \tab 2 \cr
#' 1 \tab 2 \tab 3 \cr
#' 2 \tab 3 \tab 4 \cr
#' 3 \tab 4 \tab 5 \cr
#' 4 \tab 5 \tab 6 \cr
#' 5 \tab 6 \tab 7 \cr
#' 6 \tab 7 \tab 8 \cr
#' 7 \tab 8 \tab 9 \cr
#' 8 \tab 9 \tab 9
#' }
#'
#' Basic scale is:
#' \tabular{ccc}{
#' 1/3 \tab 1 \tab 3 \cr
#' 1 \tab 3 \tab 5 \cr
#' 3 \tab 5 \tab 7 \cr
#' 5 \tab 7 \tab 9 \cr
#' 7 \tab 9 \tab 9
#' }
#'
#' @param type An object of class \code{character}. Two values are possible \code{"full"} and
#' \code{"basic"}.
#'
#' @return A \code{matrix} representing the fuzzy scale.
#'
#' @export
#' @rdname getFuzzyScale-methods
#' @name getFuzzyScale
setGeneric("getFuzzyScale",
           function(type) standardGeneric("getFuzzyScale"))

#' @rdname getFuzzyScale-methods
#' @aliases getFuzzyScale,character-method
setMethod(
  f="getFuzzyScale",
  signature(type = "character"),
  definition=function(type)
  {
    if(type == "full"){
      fuzzyScale = as.double(c(1/2,1,2,
                               1,2,3,
                               2,3,4,
                               3,4,5,
                               4,5,6,
                               5,6,7,
                               6,7,8,
                               7,8,9,
                               8,9,9))
    }else if(type == "basic"){
      fuzzyScale = as.double(c(1/3,1,3,
                               1,3,5,
                               3,5,7,
                               5,7,9,
                               7,9,9))
    }else{
      stop(paste0("Unknown type of fuzzy scale - ", type, ". Valid types are full and basic."))
    }

    fuzzyScale = matrix(data = fuzzyScale, nrow = length(fuzzyScale)/3, ncol = 3, byrow = TRUE)
    return(fuzzyScale)
  }
)
