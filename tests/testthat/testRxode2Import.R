library(testthat)
library(campsis)

context("Test the rxode2 import on a few models")

testFolder <-  file.path(getwd(), test_path())
overwriteNonRegressionFiles <- FALSE

nonRegressionFolderPath <- function(folder) {
  return(file.path(testFolder, "non_regression", "rxode2", folder))
}

test_that("Test the rxode2 parser", {
  basicExample <- "
  HELLO=A
  HELLO=B # Yes
  
  # This is a comment
  d/dt(A_BJH)=12
  if (A==0) BASIC=0
  if (A==0 || (A==1 && A==3)) {
    OUTPUT=1 
  } else if (A==1) {
  OUTPUT=2 
  } else {
  OUTPUT=3
  }
  UNKNOWN_CODE"
  
  lexer  <- rly::lex(Rxode2Lexer)
  parser <- rly::yacc(Rxode2Parser)
  
  res <- parser$parse(basicExample, lexer)

  expected <- list() %>%
    append(Equation("HELLO", "A")) %>%
    append(Equation("HELLO", "B", "Yes")) %>%
    append(LineBreak()) %>%
    append(Comment("This is a comment")) %>%
    append(Ode("A_BJH", "12")) %>%
    append(IfStatement("A==0", Equation("BASIC", "0"))) %>%
    append(ComplexIfStatement() %>%
             add(IfStatement("A==0 || (A==1 && A==3)", Equation("OUTPUT", "1"))) %>%
             add(ElseIfStatement("A==1", Equation("OUTPUT", "2"))) %>%
             add(ElseStatement(Equation("OUTPUT", "3")))) %>%
    append(UnknownStatement("UNKNOWN_CODE"))

  expect_equal(res, expected)

# myString <- "if (A==0 || (A==1 && A==3)) { OUTPUT=1 } else if (A==1) { OUTPUT=2 } else {OUTPUT=3}"
# res2 <- parser$parse(myString2, lexer)

# # Works well
# lexer$input(s = myString2 )
# repeat {
#   tok <- lexer$token()
#   if (is.null(tok)) break
# }
})
