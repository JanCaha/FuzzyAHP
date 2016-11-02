######################################################
# this file contains internal functions
# for calculation of fuzzy weights using methods described
# by Chang (1996), Wang et al.(2008) and
# Tesfamariam and Sadiq (2006)
######################################################
setGeneric(".weights_Chang",
           function(comparisonMatrix) standardGeneric(".weights_Chang"))

setMethod(
  f=".weights_Chang",
  signature(comparisonMatrix = "FuzzyPairwiseComparisonMatrix"),
  definition=function(comparisonMatrix)
  {
    numberRows = nrow(comparisonMatrix@fnMin)

    factorMin = sum(comparisonMatrix@fnMin)
    factorModal = sum(comparisonMatrix@fnModal)
    factorMax = sum(comparisonMatrix@fnMax)

    wMin = c()
    wModal = c()
    wMax = c()

    for(i in 1:numberRows){

      rowProdMin = 0
      rowProdModal = 0
      rowProdMax = 0

      for(j in 1:numberRows){
        rowProdMin = rowProdMin + comparisonMatrix@fnMin[i,j]
        rowProdModal = rowProdModal + comparisonMatrix@fnModal[i,j]
        rowProdMax = rowProdMax + comparisonMatrix@fnMax[i,j]
      }


      wMin = append(wMin, (rowProdMin / factorMax))
      wModal = append(wModal, (rowProdModal/ factorModal))
      wMax = append(wMax, (rowProdMax  / factorMin))
    }

    return(new("FuzzyWeights", fnMin = wMin, fnModal = wModal, fnMax = wMax))
  }
)

setGeneric(".weights_Wang",
           function(comparisonMatrix) standardGeneric(".weights_Wang"))

setMethod(
  f=".weights_Wang",
  signature(comparisonMatrix = "FuzzyPairwiseComparisonMatrix"),
  definition=function(comparisonMatrix)
  {
    numberRows = nrow(comparisonMatrix@fnMin)

    wMin = c()
    wModal = c()
    wMax = c()

    for(i in 1:numberRows){

      rowSumMin = 0
      rowSumModal = 0
      rowSumMax = 0

      restSumMin = 0
      restSumModal = 0
      restSumMax = 0

      for(j in 1:numberRows){
        rowSumMin = rowSumMin + comparisonMatrix@fnMin[i,j]
        rowSumModal = rowSumModal + comparisonMatrix@fnModal[i,j]
        rowSumMax = rowSumMax + comparisonMatrix@fnMax[i,j]

        for(k in 1:numberRows){
          if(k != i){
            restSumMin = restSumMin + comparisonMatrix@fnMin[k,j]
            restSumModal = restSumModal + comparisonMatrix@fnModal[k,j]
            restSumMax = restSumMax + comparisonMatrix@fnMax[k,j]
          }
        }
      }


      wMin = append(wMin, rowSumMin / (rowSumMin + restSumMax))
      wModal = append(wModal, rowSumModal / (rowSumModal + restSumModal))
      wMax = append(wMax, rowSumMax / (rowSumMax + restSumMin))
    }

    return(new("FuzzyWeights", fnMin = wMin, fnModal = wModal, fnMax = wMax))
  }
)

setGeneric(".weights_Tesfamariam",
           function(comparisonMatrix) standardGeneric(".weights_Tesfamariam"))

setMethod(
  f=".weights_Tesfamariam",
  signature(comparisonMatrix = "FuzzyPairwiseComparisonMatrix"),
  definition=function(comparisonMatrix)
  {
    numberRows = nrow(comparisonMatrix@fnMin)

    factorMin = 0
    factorModal = 0
    factorMax = 0

    for(i in 1:numberRows){

      rowProdMin = 1
      rowProdModal = 1
      rowProdMax = 1

      for (j in 1:numberRows){
        rowProdMin = rowProdMin * comparisonMatrix@fnMin[i,j]
        rowProdModal = rowProdModal * comparisonMatrix@fnModal[i,j]
        rowProdMax = rowProdMax * comparisonMatrix@fnMax[i,j]
      }

      factorMin = factorMin + (rowProdMin ^ (1/numberRows))
      factorModal = factorModal + (rowProdModal ^ (1/numberRows))
      factorMax = factorMax + (rowProdMax ^ (1/numberRows))

    }

    wMin = c()
    wModal = c()
    wMax = c()

    for(i in 1:numberRows){

      rowProdMin = 1
      rowProdModal = 1
      rowProdMax = 1

      for(j in 1:numberRows){
        rowProdMin = rowProdMin * comparisonMatrix@fnMin[i,j]
        rowProdModal = rowProdModal * comparisonMatrix@fnModal[i,j]
        rowProdMax = rowProdMax * comparisonMatrix@fnMax[i,j]
      }


      wMin = append(wMin, (rowProdMin ^ (1/numberRows)) / factorMax)
      wModal = append(wModal, (rowProdModal ^ (1/numberRows)) / factorModal)
      wMax = append(wMax, (rowProdMax ^ (1/numberRows)) / factorMin)
    }

    return(new("FuzzyWeights", fnMin = wMin, fnModal = wModal, fnMax = wMax))
  }
)

setGeneric(".weighting_vector",
           function(weights) standardGeneric(".weighting_vector"))

setMethod(
  f=".weighting_vector",
  signature(weights = "FuzzyWeights"),
  definition=function(weights)
  {
    rows = length(weights@fnModal)
    compares = c()

    for (i in 1:rows){

      values = c()

      for (j in 1:rows){
        if (i != j){
          values = append(values, .compareFN_values(weights@fnMin[j], weights@fnModal[j], weights@fnMax[j],
                                                    weights@fnMin[i], weights@fnModal[i], weights@fnMax[i]))
        }
      }
      compares = append(compares, min(values))
    }

    sum = sum(compares)
    weights = as.vector(compares / sum)


    return (new("Weights", weights = weights))
  }
)

.compareFN_values <- function(l1, m1, u1, l2, m2, u2){
  if (m2 > m1){
    return (1)
  }else if (l1>u2){
    return (0)
  }else{
    return ( (l1-u2)/((m2-u2)-(m1-l1)))
  }
}
