library(testthat)
library(campsismod)
library(campsis)
library(ggplot2)

context("Test campsistrans")

test_that("ADVAN3 TRANS4 - simulation", {

  # Import your NONMEM model using pharmpy
  object <- importNONMEM2(getNONMEMModelTemplate(3, 4))
  
  # Convert to RxODE model
  rxodeMod <- object %>% export(dest="campsis") %>%
    export(dest="rxode2")

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
  expect_no_error(plot(sim, CONC) +
    ylab("Concentration"))
})

test_that("ADVAN4 TRANS4 - simulation", {
  
  # Import your NONMEM model using pharmpy
  object <- importNONMEM2(getNONMEMModelTemplate(4, 4))
  
  # Convert to RxODE model
  rxodeMod <- object %>% export(dest="campsis") %>%
    export(dest="rxode2")
  
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
  expect_no_error(plot(sim, CONC) +
    ylab("Concentration"))
})

test_that("removePiecewiseStatements method works as expected", {
  model <- campsismod::model_suite$nonmem$advan1_trans2

  # This is an example of piecewise statement added by Pharmpy
  model_ <- model %>% replace(Ode("A_CENTRAL", "-CL*A_CENTRAL/V + Piecewise((RATE, t < AMT/RATE), (0, True))"))
  
  expect_equal(model, model_ %>% removePiecewiseStatements())
})

test_that("RATE is properly removed from control stream", {
  expect_equal(removeRateFromString("$INPUT ID ARM TIME EVID MDV AMT CMT RATE DOSENO DV DOSE TDOS TSLD"),
               "$INPUT ID ARM TIME EVID MDV AMT CMT DOSENO DV DOSE TDOS TSLD")
  
  # Dollar tag detected, no replacement
  expect_equal(removeRateFromString("$INPUT ID ARM TIME EVID MDV AMT CMT $DATA RATE"),
               "$INPUT ID ARM TIME EVID MDV AMT CMT $DATA RATE")
  
  # In filgrastim, RATE is at the end
  filgrastim <- "$INPUT ID AMT TIME CMT DV DRUG PERD ROUT BAS RATE\n$DATA Simulated_GCSF_dataset.csv IGNORE=I"
  expect_equal(removeRateFromString(filgrastim),
               "$INPUT ID AMT TIME CMT DV DRUG PERD ROUT BAS \n$DATA Simulated_GCSF_dataset.csv IGNORE=I")
  
  # RATE2 -> not removed
  expect_equal(removeRateFromString("$INPUT ID ARM TIME EVID MDV AMT CMT RATE2 DOSENO DV DOSE TDOS TSLD"),
               "$INPUT ID ARM TIME EVID MDV AMT CMT RATE2 DOSENO DV DOSE TDOS TSLD")
  
  expect_equal(removeRateFromString("$INPUT ID ARM TIME EVID MDV AMT CMT RATE2 RATE DOSENO DV DOSE TDOS TSLD"),
               "$INPUT ID ARM TIME EVID MDV AMT CMT RATE2 DOSENO DV DOSE TDOS TSLD")
})

