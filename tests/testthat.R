Sys.setenv("R_TESTS" = "")
library(testthat)
library(empCop)
test_check("empCop")
