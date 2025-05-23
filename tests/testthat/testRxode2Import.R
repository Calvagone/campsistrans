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
      # RUV
      add.sd <- 10
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
      C2 ~ add(add.sd)
    })
  }
  model <- generateModel(rxmod, folder)
  expect_equal(model, read.campsis(nonRegressionRxode2Path(folder)))
})

test_that("Unsupported error models are ignored (warning raised)", {
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
      # RUV
      add.sd <- 10
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
      C2 ~ add(add.sd) + dcauchy()
    })
  }

  model <- expect_warning(importRxode2(rxmod()), regexp="Syntax error in error model code. Error model ignored.")
  error <- model %>% find(ErrorRecord())
  expect_true(is.null(error)) # No error model
})

test_that("thetaMat in rxode2 is automatically converted to a variance-covariance matrix in Campsis", {
  folder <- "warfarin_pk"
  rxmod <- function() {
    description <- "The administration is extravascular with a first order absorption (rate constant ka) and a lag time (Tlag).\nThe PK model has one compartment (volume V) and a linear elimination (clearance Cl)."
    thetaMat <- lotri({
      Tlag_pop ~ c(Tlag_pop = 0.0358724)
      ka_pop ~ c(Tlag_pop = 0.0048502, ka_pop = 0.0840683)
      V_pop ~ c(Tlag_pop = 0.000338525, ka_pop = -0.000664941, 
                V_pop = 0.00077728)
      beta_V_logtWT ~ c(Tlag_pop = -7.37595e-05, ka_pop = -0.00173821, 
                        V_pop = -0.000187249, beta_V_logtWT = 0.0207254)
      Cl_pop ~ c(Tlag_pop = 0.000359372, ka_pop = -0.000799374, 
                 V_pop = 1.20039e-05, beta_V_logtWT = 1.73146e-06, 
                 Cl_pop = 0.00208597)
      beta_Cl_logtAGE ~ c(Tlag_pop = -0.000427923, ka_pop = 0.00137366, 
                          V_pop = -4.68529e-05, beta_V_logtWT = -8.19417e-05, 
                          Cl_pop = 0.00056905, beta_Cl_logtAGE = 0.0232414)
      beta_Cl_logtWT ~ c(Tlag_pop = -0.000294548, ka_pop = 0.000904956, 
                         V_pop = -3.88107e-05, beta_V_logtWT = -8.14008e-06, 
                         Cl_pop = -0.000583146, beta_Cl_logtAGE = 0.0035915, 
                         beta_Cl_logtWT = 0.0588315)
      omega_Tlag ~ c(Tlag_pop = -0.00862339, ka_pop = 0.00195888, 
                     V_pop = -0.000140117, beta_V_logtWT = -0.000156667, 
                     Cl_pop = -0.000128981, beta_Cl_logtAGE = 0.00013627, 
                     beta_Cl_logtWT = 0.000328784, omega_Tlag = 0.0138484)
      omega_ka ~ c(Tlag_pop = 0.00356298, ka_pop = 0.00210609, 
                   V_pop = 0.000128544, beta_V_logtWT = 0.000277115, 
                   Cl_pop = 0.000878141, beta_Cl_logtAGE = -0.00306933, 
                   beta_Cl_logtWT = -0.00279933, omega_Tlag = -0.00128196, 
                   omega_ka = 0.057906)
      omega_V ~ c(Tlag_pop = -0.000101699, ka_pop = -0.000324991, 
                  V_pop = 4.75454e-06, beta_V_logtWT = 0.000133395, 
                  Cl_pop = -3.00809e-06, beta_Cl_logtAGE = -2.41747e-05, 
                  beta_Cl_logtWT = -1.92085e-05, omega_Tlag = 0.000107963, 
                  omega_ka = 0.000143227, omega_V = 0.000490203)
      omega_Cl ~ c(Tlag_pop = -3.65558e-05, ka_pop = -8.56155e-05, 
                   V_pop = 7.19071e-06, beta_V_logtWT = 5.34602e-06, 
                   Cl_pop = 1.4775e-05, beta_Cl_logtAGE = 7.56588e-05, 
                   beta_Cl_logtWT = -1.88607e-05, omega_Tlag = 4.12211e-05, 
                   omega_ka = 2.07224e-05, omega_V = 1.27613e-05, omega_Cl = 0.00109385)
      a ~ c(Tlag_pop = 0.000301054, ka_pop = 0.000409364, V_pop = -3.53596e-05, 
            beta_V_logtWT = -0.000226686, Cl_pop = 2.4098e-05, 
            beta_Cl_logtAGE = 0.000212168, beta_Cl_logtWT = 0.000308927, 
            omega_Tlag = -6.1295e-05, omega_ka = -0.000635849, 
            omega_V = 3.66105e-06, omega_Cl = -9.13216e-06, a = 0.00135834)
      b ~ c(Tlag_pop = -6.48436e-05, ka_pop = -8.32097e-05, 
            V_pop = 5.99048e-06, beta_V_logtWT = 3.39069e-05, 
            Cl_pop = -8.82669e-06, beta_Cl_logtAGE = -1.45752e-05, 
            beta_Cl_logtWT = -2.97896e-05, omega_Tlag = -1.15585e-05, 
            omega_ka = -0.000152184, omega_V = -1.2131e-05, omega_Cl = -2.96343e-06, 
            a = -0.00013078, b = 4.91237e-05)
    })
    ini({
      Cl_pop <- -2.01829170163338
      Tlag_pop <- -0.171888019201211
      V_pop <- 2.06026390639737
      ka_pop <- 0.321812129091816
      beta_Cl_logtAGE <- 0.338204486151462
      beta_Cl_logtWT <- 0.645718603330012
      beta_V_logtWT <- 0.917505734062465
      a <- c(0, 0.291776854477768)
      b <- c(0, 0.0719446080618757)
      omega_Cl ~ 0.0627688213988444
      omega_Tlag ~ 0.239824233260291
      omega_V ~ 0.0199182346397427
      omega_ka ~ 0.731546596683407
    })
    model({
      cmt(depot)
      cmt(central)
      Cl <- exp(Cl_pop + beta_Cl_logtAGE * log(AGE/30.4353) + 
                  beta_Cl_logtWT * log(WT/68.1178) + omega_Cl)
      Tlag <- exp(Tlag_pop + omega_Tlag)
      V <- exp(V_pop + beta_V_logtWT * log(WT/68.1178) + omega_V)
      ka <- exp(ka_pop + omega_ka)
      d/dt(depot) <- -ka * depot
      alag(depot) <- Tlag
      d/dt(central) <- +ka * depot - Cl/V * central
      Cc <- central/V
      y1 <- Cc
      y1 ~ add(a) + prop(b) + combined2()
    })
  }
  
  model <- generateModel(rxmod, folder)
  expect_equal(model, read.campsis(nonRegressionRxode2Path(folder)))
})
