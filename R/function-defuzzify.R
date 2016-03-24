#' Function to defuzzify fuzzy data
#'
#' @description
#' This function defuzzifies \code{\linkS4class{FuzzyData}} into single value.
#' The \code{\linkS4class{FuzzyData}} can only contain one fuzzy number, otherwise the defuzzification is not possible.
#'
#' @param fuzzyData A \code{\linkS4class{FuzzyData}}
#' @param type A \code{"character"} representing type of defuzzification. Currently implemented methods are
#' \code{"Yager"} and default.
#'
#' @return A numeric value of defuzzified value, based on deffuzification method.
#'
#' @export
#' @rdname defuzziffy-methods
#' @name defuzziffy
setGeneric("defuzziffy",
           function(fuzzyData, type) standardGeneric("defuzziffy"))

#' @rdname defuzziffy-methods
#' @aliases defuzziffy,FuzzyData,character-method
setMethod(
  f="defuzziffy",
  signature(fuzzyData = "FuzzyData", type = "character"),
  definition=function(fuzzyData, type)
  {

    if(ncol(fuzzyData@fnMin)>1){
      stop("Defuzzification is only possible for datasets representing one fuzzy number.")
    }

    numberRows = nrow(fuzzyData@fnModal)

    result = matrix(NA, nrow = numberRows, ncol = 1)

    if(type == "Yager"){
      result[,1] = (((fuzzyData@fnModal[,1]-fuzzyData@fnMin[,1])*(fuzzyData@fnMin[,1]+(2/3)*(fuzzyData@fnModal[,1]-fuzzyData@fnMin[,1]))+((fuzzyData@fnMax[,1]-fuzzyData@fnModal[,1])*(fuzzyData@fnModal[,1]+(1/3)*(fuzzyData@fnMax[,1]-fuzzyData@fnModal[,1]))))/((fuzzyData@fnModal[,1]-fuzzyData@fnMin[,1])+(fuzzyData@fnMax[,1]-fuzzyData@fnModal[,1])))
    }
    else{
      result[,1] = (fuzzyData@fnMin[,1] + fuzzyData@fnModal[,1] + fuzzyData@fnMax[,1])/3
    }

    return(result)
  }
)
