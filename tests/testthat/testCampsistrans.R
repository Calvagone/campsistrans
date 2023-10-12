library(testthat)
library(campsismod)
library(campsis)
library(ggplot2)

context("Test campsistrans")

testFolder <<- ""

test_that("ADVAN3 TRANS4 - simulation", {

  # Import your NONMEM model using pharmpy
  object <- importNONMEM(getNONMEMModelTemplate(3, 4))
  
  # Convert to RxODE model
  rxodeMod <- object %>% export(dest="campsis") %>% export(dest="RxODE")

  # Loading model in RxODE
  mod <- rxode2::RxODE(paste0(rxodeMod@code, collapse="\n"))

  # Dosing regimen
  ev <- rxode2::et(amount.units="mg", time.units="hours") %>%
    rxode2::et(amt=1000, cmt="A_CENTRAL")
 
  # Sampling time
  ev <- ev %>% rxode2::et(0, 48, length.out=100)
 
  # Simulate
  sim  <- rxode2::rxSolve(mod, params=rxodeMod@theta, ev, omega=rxodeMod@omega, sigma=rxodeMod@sigma, nSub=100)
 
  # Plotting CONC
  plot(sim, CONC) +
    ylab("Concentration") 
})

test_that("ADVAN4 TRANS4 - simulation", {
  
  # Import your NONMEM model using pharmpy
  object <- importNONMEM(getNONMEMModelTemplate(4, 4))
  
  # Convert to RxODE model
  rxodeMod <- object %>% export(dest="campsis") %>% export(dest="RxODE")
  
  # Loading model in RxODE
  mod <- rxode2::RxODE(paste0(rxodeMod@code, collapse="\n"))
  
  # Dosing regimen
  ev <- rxode2::et(amount.units="mg", time.units="hours") %>%
    rxode2::et(amt=1000, cmt="A_DEPOT")
  
  # Sampling time
  ev <- ev %>% rxode2::et(0, 48, length.out=100)
  
  # Simulate
  sim  <- rxode2::rxSolve(mod, params=rxodeMod@theta, ev, omega=rxodeMod@omega, sigma=rxodeMod@sigma, nSub=100)
  
  # Plotting CONC
  plot(sim, CONC) +
    ylab("Concentration")
})

test_that("Custom test with RxODE", {
  
  mapping <- mapping(theta=c("CL"=1, "V1"=2, "V2"=3, "Q"=4),
                     omega=c("CL"=1, "V1"=2, "V2"=3, "Q"=4),
                     sigma=c("PROP"=1))
  
  # Import your NONMEM model using pharmpy
  object <- importNONMEM(getNONMEMModelTemplate(3, 4), mapping)
  
  # Convert to RxODE model
  rxodeMod <- object %>% export(dest="campsis") %>% export(dest="RxODE")
  
  mod <- rxode2::RxODE("
    CL=THETA_CL*exp(ETA_CL)
    V1=THETA_V1*exp(ETA_V1)
    if(V1 < 80) V1=40
    V2=THETA_V2*exp(ETA_V2)
    Q=THETA_Q*exp(ETA_Q)
    S1=V1
    d/dt(A_CENTRAL) = Q*A_PERIPHERAL/V2 +
     (-CL/V1 - Q/V1)*A_CENTRAL
    d/dt(A_PERIPHERAL) = -Q*A_PERIPHERAL/V2 + Q*A_CENTRAL/V1
    d/dt(A_OUTPUT) = CL*A_CENTRAL/V1
    F=A_CENTRAL/S1
    CP=F
    OBS_CP=CP*(EPS_PROP + 1)
    Y=OBS_CP
  ")

  # Dosing regimen
  ev <- rxode2::et(amount.units="mg", time.units="hours") %>%
    rxode2::et(amt=1000, cmt="A_CENTRAL")
  
  # Sampling time
  ev <- ev %>% rxode2::et(0, 48, length.out=100)
  
  # Simulate
  sim  <- rxode2::rxSolve(mod, params=rxodeMod@theta, ev, omega=rxodeMod@omega, sigma=rxodeMod@sigma, nSub=100)
  
  results <- as.data.frame(sim) 
  results <- results %>% dplyr::mutate(time=as.numeric(time)) %>% dplyr::filter(time==0)
  
  # Plotting C2
  plot(sim, CP) +
    ylab("Concentration") 
})

test_that("removePiecewiseStatements method works as expected", {
  model <- campsismod::model_suite$nonmem$advan1_trans2

  # This is an example of piecewise statement added by Pharmpy
  model_ <- model %>% replace(Ode("A_CENTRAL", "-CL*A_CENTRAL/V + Piecewise((RATE, t < AMT/RATE), (0, True))"))
  
  expect_equal(model, model_ %>% removePiecewiseStatements())
})