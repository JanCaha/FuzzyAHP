require("testthat")

# example 2 from Krejčí, Pavlačka and Talašová
test_that("Tests of fuzzy AHP calculation", {

  folder = "./"
  # folder = "./tests/testthat/"
  file = "testing_fuzzy_pairwise_comparison_matrix.txt"

  comparisonMatrix = read.csv(paste(folder, file, sep = ""), sep = ";",
                              stringsAsFactors = FALSE, header = FALSE, strip.white = TRUE)

  comparisonMatrix = as.matrix(comparisonMatrix)

  comparisonMatrix = fuzzyPairwiseComparisonMatrix(comparisonMatrix)

  weights = calculateWeights(comparisonMatrix)

  print(weights)

  file = "testing_fuzzy_pairwise_comparison_matrix_alternatives_C1.txt"

  comparisonMatrixAlternatives1 = read.csv(paste(folder, file, sep = ""), sep = ";",
                              stringsAsFactors = FALSE, header = FALSE, strip.white = TRUE)

  comparisonMatrixAlternatives1 = fuzzyPairwiseComparisonMatrix(as.matrix(comparisonMatrixAlternatives1))

  print(calculateWeights(comparisonMatrixAlternatives1))

  file = "testing_fuzzy_pairwise_comparison_matrix_alternatives_C2.txt"

  comparisonMatrixAlternatives2 = read.csv(paste(folder, file, sep = ""), sep = ";",
                                           stringsAsFactors = FALSE, header = FALSE, strip.white = TRUE)

  comparisonMatrixAlternatives2 = fuzzyPairwiseComparisonMatrix(as.matrix(comparisonMatrixAlternatives2))

  print(calculateWeights(comparisonMatrixAlternatives2))

  l = list()
  l[["1"]] = calculateWeights(comparisonMatrixAlternatives1)
  l[["2"]] = calculateWeights(comparisonMatrixAlternatives2)
})
