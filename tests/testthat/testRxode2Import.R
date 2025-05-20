library(testthat)
library(campsis)
library(rxode2)

context("Test the rxode2 import on a few models")

testFolder <-  file.path(getwd(), test_path())
overwriteNonRegressionFiles <- FALSE

nonRegressionRxode2Path <- function(folder) {
  return(file.path(testFolder, "non_regression", "rxode2", folder))
}

toModelStatements <- function(x) {
  retValue <- ModelStatements()
  retValue@list <- list() %>%
    append(x) %>%
    unlist()
  return(retValue)
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
    append(ComplexIfElseStatement() %>%
             add(ExtendedIfStatement("A==0 || (A==1 && A==3)", toModelStatements(Equation("OUTPUT", "1")))) %>%
             add(ElseIfStatement("A==1", toModelStatements(Equation("OUTPUT", "2")))) %>%
             add(ElseStatement(toModelStatements(Equation("OUTPUT", "3"))))) %>%
    append(UnknownStatement("UNKNOWN_CODE"))

  expect_equal(res, expected)

  
  complexIfElse <- "if (NbCibleEH == 0) {     tNbCibleEH = \"G_0\" } else if (NbCibleEH == 1 || NbCibleEH == 2 || NbCibleEH == 3 || NbCibleEH == 4 || NbCibleEH == 5) {     tNbCibleEH = \"G_1_2_3_4_5\" } else {     tNbCibleEH = \"G_0\" }\n"
  
  res <- parser$parse(complexIfElse, lexer)
  
  expected <- list() %>%
    append(ComplexIfElseStatement() %>%
             add(ExtendedIfStatement("NbCibleEH == 0", toModelStatements(Equation("tNbCibleEH", "\"G_0\"")))) %>%
             add(ElseIfStatement("NbCibleEH == 1 || NbCibleEH == 2 || NbCibleEH == 3 || NbCibleEH == 4 || NbCibleEH == 5",
                                 toModelStatements(Equation("tNbCibleEH", "\"G_1_2_3_4_5\"")))) %>%
             add(ElseStatement(toModelStatements(Equation("tNbCibleEH", "\"G_0\"")))))
  
  expect_equal(res, expected)
  
  # complexIf <- ComplexIfElseStatement() %>%
  #   add(ExtendedIfStatement("A==0 || (A==1 && A==3)", toModelStatements(Equation("OUTPUT", "1")))) %>%
  #   add(ElseIfStatement("A==1", toModelStatements(Equation("OUTPUT", "2")))) %>%
  #   add(ElseStatement(toModelStatements(Equation("OUTPUT", "3"))))
  # 
  # complexIf %>% getName()
  # 
  # complexIf <- ComplexIfElseStatement() %>%
  #   add(ExtendedIfStatement("NbCibleEH == 0", toModelStatements(Equation("tNbCibleEH", "\"G_0\"")))) %>%
  #   add(ElseIfStatement("NbCibleEH == 1 || NbCibleEH == 2 || NbCibleEH == 3 || NbCibleEH == 4 || NbCibleEH == 5",
  #                       toModelStatements(Equation("tNbCibleEH", "\"G_1_2_3_4_5\"")))) %>%
  #   add(ElseStatement(toModelStatements(Equation("tNbCibleEH", "\"G_0\""))))
  # 
  # complexIf %>% getName()
})

test_that("Test the rxode2 error model parser", {
  lexer  <- rly::lex(Rxode2ErrorModelLexer)
  parser <- rly::yacc(Rxode2ErrorModelParser)
  
  line <- "y1 ~ add(add.sd) + prop(prop.sd)  + combined1()"
  
  res <- parser$parse(line, lexer)
  
  expected <- Rxode2ErrorModel(add="add.sd", prop="prop.sd", combined1=TRUE, endpoint="y1")
  
  expect_equal(res, expected)
})

generateModel <- function(rxmod, folder) {
  model <- importRxode2(rxmod())
  
  if (overwriteNonRegressionFiles) {
    model %>% write(file=nonRegressionRxode2Path(folder))
  }
  return(model)
}

test_that("Import a simple model from rxode2 (single subject)", {
  folder <- "simple_model_single_subject"
  rxmod <- function() {
    ini({
      # central 
      KA=2.94E-01
      CL=1.86E+01
      V2=4.02E+01
      # peripheral
      Q=1.05E+01
      V3=2.97E+02
      # effects
      Kin=1
      Kout=1
      EC50=200 
    })
    model({
      C2 <- centr/V2
      C3 <- peri/V3
      d/dt(depot) <- -KA*depot
      d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
      d/dt(peri)  <- Q*C2 - Q*C3
      eff(0) <- 1
      d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
    })
  }
  model <- generateModel(rxmod, folder)
  expect_equal(model, read.campsis(nonRegressionRxode2Path(folder)))
})


test_that("Import a simple model from rxode2 (population simulation)", {
  folder <- "simple_model_population_simulation"
  rxmod <- function() {
    ini({
      KA <- 2.94E-01
      TCl <- 1.86E+01
      # between subject variability
      eta.Cl ~ 0.4^2
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
    })
    model({
      C2 <- centr/V2
      C3 <- peri/V3
      CL <-  TCl*exp(eta.Cl) ## This is coded as a variable in the model
      d/dt(depot) <- -KA*depot
      d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
      d/dt(peri)  <-                    Q*C2 - Q*C3
      d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
      eff(0) <- 1
    })
  }
  model <- generateModel(rxmod, folder)
  expect_equal(model, read.campsis(nonRegressionRxode2Path(folder)))
})
