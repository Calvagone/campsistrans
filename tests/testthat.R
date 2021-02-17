Sys.setenv("R_TESTS" = "")
library(testthat)
library(pmxtrans)
test_check("pmxtrans")
