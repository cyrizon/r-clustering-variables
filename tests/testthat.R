# ==============================================================================
# Unit-Tests for the R package clustVarACC
# ==============================================================================
# This file automatically runs all unit tests for the package
# when running devtools::test() or R CMD check
#
# To run the tests:
# - From R: devtools::test()
# - From the command line: R CMD check
# ==============================================================================

library(testthat)
library(clustVarACC)

# Run all tests in the testthat/ directory
test_check("clustVarACC")
