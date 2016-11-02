#' Function to calculate fuzzy weights based on comparison matrix
#'
#' @description This functions calculates \code{\linkS4class{Weights}} or \code{\linkS4class{FuzzyWeights}}
#' based on input pairwise comparison matrix.
#'
#' @param comparisonMatrix object of either \linkS4class{PairwiseComparisonMatrix} or \linkS4class{FuzzyPairwiseComparisonMatrix}
#'
#' @seealso \link{PairwiseComparisonMatrix-class}
#'
#' @references
#' KREJČÍ, Jana, PAVLAČKA, Ondřej and TALAŠOVÁ, Jana, 2016, A fuzzy extension of Analytic Hierarchy Process based on the constrained fuzzy arithmetic. Fuzzy Optimization and Decision Making. 2016. DOI 10.1007/s10700-016-9241-0.
#'
#' @export
#' @rdname calculateWeights-methods
#' @name calculateWeights
setGeneric("calculateWeights",
           function(comparisonMatrix) standardGeneric("calculateWeights"))

#' @rdname calculateWeights-methods
#' @aliases calculateWeights,PairwiseComparisonMatrix-method
setMethod(
  f="calculateWeights",
  signature(comparisonMatrix="PairwiseComparisonMatrix"),
  definition=function(comparisonMatrix)
  {
    weightsCount = nrow(comparisonMatrix@values)

    weights = vector(mode = "numeric", length = weightsCount)

    for(i in 1:weightsCount){
      weights[i] = prod(comparisonMatrix@values[i,])^(1/weightsCount)
    }

    sumWeights = sum(weights)

    weights = weights/sumWeights

    rNames = c()
    for (i in 1:length(weights)){
      rNames = append(rNames, paste("w_", comparisonMatrix@variableNames[i], sep = ""))
    }

    names(weights) = rNames

    return (new("Weights", weights = weights))
  }
)

#' @rdname calculateWeights-methods
#' @aliases calculateWeights,FuzzyPairwiseComparisonMatrix-method
setMethod(
  f="calculateWeights",
  signature(comparisonMatrix="FuzzyPairwiseComparisonMatrix"),
  definition=function(comparisonMatrix)
  {
    p = nrow(comparisonMatrix@fnMin)

    wMin = c()
    wModal = c()
    wMax = c()

    for(i in 1:p){
      limits = .weightsLimits(comparisonMatrix,i)
      wMin = append(wMin, limits[1])
      wMax = append(wMax, limits[2])

      sum = 0

      for(k in 1:p){
        sum = sum + prod(comparisonMatrix@fnModal[k,])^(1/p)
      }

      wM = ((prod(comparisonMatrix@fnModal[i,])^(1/p)) / sum)

      wModal = append(wModal, wM)
    }

    return (new("FuzzyWeights", fnMin = wMin, fnModal = wModal, fnMax = wMax))
  }
)

#' Function to calculate fuzzy weights based on comparison matrix using older approaches
#'
#' @description This functions calculates \code{\linkS4class{FuzzyWeights}}
#' based on input fuzzy pairwise comparison matrix.
#'
#' @param comparisonMatrix object of \linkS4class{FuzzyPairwiseComparisonMatrix}
#' @param type A \code{"character"} representing type of method used for weights or fuzzy weights
#' determination. Currently implemented methods are
#' \code{"Chang"}, \code{"Wang"} and \code{"Tesfamariam"}. The default value is \code{"Chang"}.
#'
#' @return \code{\linkS4class{FuzzyWeights}}. If fuzzy weighting vector is to be obtained
#' please see \code{\link{calculate_weighting_vector}} function.
#'
#' @seealso \code{\link{calculate_weighting_vector}}
#'
#' @references
#' CHANG, Da-Yong, 1996, Applications of the extent analysis method on fuzzy AHP. European Journal of Operational Research. 1996. Vol. 95, no. 3, p. 649–655. DOI 10.1016/0377-2217(95)00300-2.
#'
#' TESFAMARIAM, Solomon and SADIQ, Rehan, 2006, Risk-based environmental decision-making using fuzzy analytic hierarchy process (F-AHP). Stochastic Environmental Research and Risk Assessment. 2006. Vol. 21, no. 1, p. 35–50. DOI 10.1007/s00477-006-0042-9.
#'
#' WANG, Tien-Chin and CHEN, Yueh-Hsiang, 2008, Applying fuzzy linguistic preference relations to the improvement of consistency of fuzzy AHP. Information Sciences [online]. October 2008. Vol. 178, no. 19, p. 3755–3765. DOI 10.1016/j.ins.2008.05.028.
#'
#' @export
#' @rdname calculateWeights_old_methods-methods
#' @name calculateWeights_old_methods
setGeneric("calculateWeights_old_methods",
           function(comparisonMatrix, type = "Chang") standardGeneric("calculateWeights_old_methods"))

#' @rdname calculateWeights_old_methods-methods
#' @aliases calculateWeights_old_methods, FuzzyPairwiseComparisonMatrix-method, character-method
setMethod(
  f="calculateWeights_old_methods",
  signature(comparisonMatrix = "FuzzyPairwiseComparisonMatrix"),
  definition=function(comparisonMatrix, type)
  {
    if(typeof(type) != "character"){
      stop("Variable type must be character!")
    }

    if(type == "Chang"){
      return(.weights_Chang(comparisonMatrix))
    }else if (type == "Tesfamariam"){
      return(.weights_Tesfamariam(comparisonMatrix))
    }else if (type == "Wang"){
      return(.weights_Wang(comparisonMatrix))
    }else{
      stop("Method name not recognized. Can not calculate the weights!")
    }

  }
)


#' Function to calculate fuzzy weighting vector
#'
#' @description This functions calculates fuzzy weighting vector from \code{\linkS4class{FuzzyWeights}}.
#' The calculation was first described by Chang (1996).
#'
#' @param fuzzyWeights  object of \linkS4class{FuzzyWeights}
#'
#' @return weighting vector for defined \linkS4class{FuzzyWeights}.
#'
#' @references
#' CHANG, Da-Yong, 1996, Applications of the extent analysis method on fuzzy AHP. European Journal of Operational Research. 1996. Vol. 95, no. 3, p. 649–655. DOI 10.1016/0377-2217(95)00300-2.
#'
#' @export
#' @rdname calculate_weighting_vector-methods
#' @name calculate_weighting_vector
setGeneric("calculate_weighting_vector",
           function(fuzzyWeights) standardGeneric("calculate_weighting_vector"))

#' @rdname calculate_weighting_vector-methods
#' @aliases calculate_weighting_vector, FuzzyWeights-method
setMethod(
  f="calculate_weighting_vector",
  signature(fuzzyWeights = "FuzzyWeights"),
  definition=function(fuzzyWeights)
  {
    return(.weighting_vector(fuzzyWeights))
  }
)
