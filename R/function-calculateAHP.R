#' Function to calculate result of AHP
#'
#' @description  This function calculates output of AHP based on \code{\linkS4class{Weights}}
#' or \code{\linkS4class{FuzzyWeights}} on data represented either by \code{matrix} or
#' \code{\linkS4class{FuzzyData}}.
#'
#' @param weights object of class \linkS4class{Weights} or \linkS4class{FuzzyWeights}.
#' @param data matrix or \linkS4class{FuzzyData} with number of colums equal to number of rows in \code{weights}.
#'
#' @return Either a matrix (if \linkS4class{Weights} and \code{matrix} were used as inputs) or
#' \linkS4class{FuzzyData} (if \linkS4class{FuzzyWeights} were used).
#'
#' @export
setGeneric("calculateAHP",
           function(weights, data) standardGeneric("calculateAHP"))

setMethod(
  f="calculateAHP",
  signature(weights = "Weights", data = "matrix"),
  definition=function(weights, data)
  {
    if(!(ncol(data)==length(weights@weights))){
      stop("Can not multiply the data by the weights, the number of data columns does not match the number of weights.")
    }

    # pb = txtProgressBar(min=0, max=1, initial = 0, style = 3)

    numberRows = nrow(data)

    result = matrix(nrow = numberRows, ncol = 1)

    result[, 1] = rowSums(t(t(data[,]) * weights@weights))

#       for (i in 1:nrow(data)){
#
#         if (any(is.na(data[i,]))){
#           result[i, 1] = NA
#         } else {
#           result[i,1] = sum(data[i,] * w@weights)
#         }
#
#         setTxtProgressBar(pb, ((i/numberRows)))
#       }

      colnames(result) = c("result")
      return(result)
  }
)

setMethod(
  f="calculateAHP",
  signature(weights = "FuzzyWeights", data = "matrix"),
  definition=function(weights, data)
  {
    if(!(ncol(data)==length(weights@fnMin))){
      stop("Can not multiply the data by the weights, the number of data columns does not match the number of weights.")
    }

    # pb = txtProgressBar(min=0, max=1, initial = 0, style = 3)

    numberRows = nrow(data)

    result = matrix(nrow = numberRows, ncol = 3)

    result[, 1] = rowSums(t(t(data[,]) * weights@fnMin))
    result[, 2] = rowSums(t(t(data[,]) * weights@fnModal))
    result[, 3] = rowSums(t(t(data[,]) * weights@fnMax))

#     for (i in 1:nrow(data)){
#
#       if (any(is.na(data[i,]))){
#         result[i, 1] = NA
#         result[i, 2] = NA
#         result[i, 3] = NA
#
#       }else{
#
#         result[i, 1] = sum(data[i,] * weights@weights[,1])
#         result[i, 2] = sum(data[i,] * weights@weights[,2])
#         result[i, 3] = sum(data[i,] * weights@weights[,3])
#       }
#
#       #if(i%%10000 == 0){
#       #  print(paste("Calculated ", (i/numberRows)*100, "% of data."))
#       #}
#
#       setTxtProgressBar(pb, ((i/numberRows)))
#
#     }
#     close(pb)

    #colnames(result) = c("result_minimum", "result_modal", "result_maximum")
    return(new("FuzzyData", fnMin = as.matrix(result[,1]), fnModal = as.matrix(result[,2]), fnMax = as.matrix(result[,3])))
  }
)

setMethod(
  f="calculateAHP",
  signature(weights = "FuzzyWeights", data = "FuzzyData"),
  definition=function(weights, data)
  {
    if(!(ncol(data@fnModal)==length(weights@fnModal))){
      stop("Can not multiply the fuzzy data by the weights, the number of data columns does not match the number of weights.")
    }

    # pb = txtProgressBar(min=0, max=1, initial = 0, style = 3)

    numberRows = nrow(data@fnModal)

    result = matrix(nrow = numberRows, ncol = 3)

    result[, 1] = rowSums(t(t(data@fnMin[,]) * weights@fnMin))
    result[, 2] = rowSums(t(t(data@fnModal[,]) * weights@fnModal))
    result[, 3] = rowSums(t(t(data@fnMax[,]) * weights@fnMax))

#     for (i in 1:nrow(data)){
#
#       if (any(is.na(data[i,]))){
#         result[i, 1] = NA
#         result[i, 2] = NA
#         result[i, 3] = NA
#
#       }else{
#
#         result[i, 1] = sum(data@fnMin[i,] * weights@weights[,1])
#         result[i, 2] = sum(data@fnModal[i,] * weights@weights[,2])
#         result[i, 3] = sum(data@fnMax[i,] * weights@weights[,3])
#       }
#
#       setTxtProgressBar(pb, ((i/numberRows)))
#     }

    #colnames(result) = c("result_minimum", "result_modal", "result_maximum")
    return(new("FuzzyData", fnMin = as.matrix(result[,1]), fnModal = as.matrix(result[,2]), fnMax = as.matrix(result[,3])))
  }
)
