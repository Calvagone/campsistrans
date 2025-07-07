library(testthat)
library(campsis)
library(rxode2)

context("Test the rxode2 code parser")

testFolder <-  file.path(getwd(), test_path())
overwriteNonRegressionFiles <- FALSE

nonRegressionRxode2Path <- function(folder) {
  return(file.path(testFolder, "non_regression", "rxode2", folder))
}

toModelStatements <- function(...) {
  x <- list(...)
  retValue <- ModelStatements()
  retValue@list <- list() %>%
    append(x) %>%
    unlist()
  return(retValue)
}

test_that("Test the rxode2 parser (complex if statements, equations, line breaks, etc.)", {
  example <- "
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
  
  res <- parser$parse(example, lexer ) %>%
    unlist()
  
  expected <- list() %>%
    append(Equation("HELLO", "A")) %>%
    append(Equation("HELLO", "B", "Yes")) %>%
    append(LineBreak()) %>%
    append(Comment("This is a comment")) %>%
    append(Ode("A_BJH", "12")) %>%
    append(IfStatement("A==0", Equation("BASIC", "0"))) %>%
    append(ComplexIfElseStatement() %>%
             add(ExtendedIfStatement("A==0 || (A==1 && A==3)", toModelStatements(Equation("OUTPUT", "1")))) %>%
             add(ElseIfStatement("A==1", toModelStatements(Equation("OUTPUT", "2")))) %>%
             add(ElseStatement(toModelStatements(Equation("OUTPUT", "3"))))) %>%
    append(UnknownStatement("UNKNOWN_CODE"))
  
  expect_equal(res, expected)
  
  complexIfStatement <- expected[[7]]
  complexIfStatementStrA <- campsismod::toString(complexIfStatement) # Default destination
  complexIfStatementStrB <- campsismod::toString(complexIfStatement, dest="NONMEM") # Test the dest attribute
  expect_error(campsismod::toString(complexIfStatement, dest="unknown"))
  
  expect_equal(complexIfStatementStrA,
               c("if (A==0 || (A==1 && A==3)) {\n  OUTPUT=1\n}", "else if (A==1) {\n  OUTPUT=2\n}", "else {\n  OUTPUT=3\n}"))
  expect_equal(complexIfStatementStrB,
               c("IF (A==0 || (A==1 && A==3)) {\n  OUTPUT=1\n}", "ELSE IF (A==1) {\n  OUTPUT=2\n}", "ELSE {\n  OUTPUT=3\n}"))
  
  
  # Test the parser with a non-regression example
  complexIfElse <- "if (NbCibleEH == 0) {     tNbCibleEH = \"G_0\" } else if (NbCibleEH == 1 || NbCibleEH == 2 || NbCibleEH == 3 || NbCibleEH == 4 || NbCibleEH == 5) {     tNbCibleEH = \"G_1_2_3_4_5\" } else {     tNbCibleEH = \"G_0\" }\n"
  
  res <- parser$parse(complexIfElse, lexer) %>%
    unlist()
  
  expected <- list() %>%
    append(ComplexIfElseStatement() %>%
             add(ExtendedIfStatement("NbCibleEH == 0", toModelStatements(Equation("tNbCibleEH", "\"G_0\"")))) %>%
             add(ElseIfStatement("NbCibleEH == 1 || NbCibleEH == 2 || NbCibleEH == 3 || NbCibleEH == 4 || NbCibleEH == 5",
                                 toModelStatements(Equation("tNbCibleEH", "\"G_1_2_3_4_5\"")))) %>%
             add(ElseStatement(toModelStatements(Equation("tNbCibleEH", "\"G_0\"")))))
  
  expect_equal(res, expected)
})

test_that("Test the rxode2 parser (multiple and successive if statements)", {
  example <- "
if (DVID == 2) {
  CONC=CONCX
  PROP_RUV=PROPX
} 
if (DVID == 3) {
  CONC=CONCY
  PROP_RUV=PROPY
} 
if (DVID == 6) {
  CONC=CONCZ
  PROP_RUV=PROPZ
}
A=5
"
  lexer  <- rly::lex(Rxode2Lexer)
  parser <- rly::yacc(Rxode2Parser)
  
  res <- parser$parse(example, lexer) %>%
    unlist()
  
  expected <- list() %>%
    append(ComplexIfElseStatement() %>%
             add(ExtendedIfStatement("DVID == 2", toModelStatements(Equation("CONC", "CONCX"), Equation("PROP_RUV", "PROPX"))))) %>%
    append(ComplexIfElseStatement() %>%
             add(ExtendedIfStatement("DVID == 3", toModelStatements(Equation("CONC", "CONCY"), Equation("PROP_RUV", "PROPY"))))) %>%
    append(ComplexIfElseStatement() %>%
             add(ExtendedIfStatement("DVID == 6", toModelStatements(Equation("CONC", "CONCZ"), Equation("PROP_RUV", "PROPZ"))))) %>%
    append(Equation("A", "5"))
  expect_equal(res, expected)
})

test_that("Test the rxode2 error model parser", {
  lexer  <- rly::lex(Rxode2ErrorModelLexer)
  parser <- rly::yacc(Rxode2ErrorModelParser)
  
  line <- "y1 ~ add(add.sd) + prop(prop.sd)  + combined1()"
  
  res <- parser$parse(line, lexer)
  
  expected <- Rxode2ErrorModel(add="add.sd", prop="prop.sd", combined1=TRUE, endpoint="y1")
  
  expect_equal(res, expected)
})
