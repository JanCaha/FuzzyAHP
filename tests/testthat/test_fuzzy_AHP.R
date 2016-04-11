require("testthat")

test_that("Tests of fuzzy AHP calculation", {

  folder = "./"
  # folder = "./tests/testthat/"
  file = "testing_pairwise_comparison_matrix.txt"

  comparisonMatrix = read.csv(paste(folder, file, sep = ""), sep = ";",
                              stringsAsFactors = FALSE, header = FALSE, strip.white = TRUE)

  comparisonMatrix = as.matrix(comparisonMatrix)

  comparisonMatrix = pairwiseComparisonMatrix(comparisonMatrix)

  CR = consistencyRatio(comparisonMatrix, print.report = FALSE)

  fuzzyComparisonMatrix = fuzzyPairwiseComparisonMatrix(comparisonMatrix)

  CRF = consistencyRatio(fuzzyComparisonMatrix, print.report = FALSE)

  expect_equal(CR, CRF, tolerance = 1e-07)

  weights = calculateWeights(fuzzyComparisonMatrix)

  expect_is(fuzzyComparisonMatrix, "FuzzyPairwiseComparisonMatrix")
  expect_is(weights, "FuzzyWeights")
})
