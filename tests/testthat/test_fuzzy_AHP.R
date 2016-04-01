require("testthat")

test_that("Tests of fuzzy AHP calculation", {

  folder = "./" # ./tests/testthat/
  file = "testing_pairwise_comparison_matrix.txt"

  comparisonMatrix = read.csv(paste(folder, file, sep = ""), sep = ";",
                              stringsAsFactors = FALSE, header = FALSE, strip.white = TRUE)

  comparisonMatrix = as.matrix(comparisonMatrix)

  comparisonMatrix = pairwiseComparisonMatrix(comparisonMatrix)

  #print(comparisonMatrix)

  CR = consistencyRatio(comparisonMatrix)

  #print(calculateWeights(comparisonMatrix))

  fuzzyComparisonMatrix = fuzzyPairwiseComparisonMatrix(comparisonMatrix)

  #print(fuzzyComparisonMatrix)

  weights = calculateWeights(fuzzyComparisonMatrix)

  #print(weights)

  expect_is(fuzzyComparisonMatrix, "FuzzyPairwiseComparisonMatrix")
  expect_is(weights, "FuzzyWeights")
})


