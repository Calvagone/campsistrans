Sys.setenv("R_TESTS" = "")
library(testthat)
library(campsistrans)
test_check("campsistrans")
