library(testthat)
library(ggplot2)

context("Test pmxtrans")
testFolder <<- ""
testFolder <<- "C:/prj/pmxtran/tests/testthat/"

test_that("ADVAN3 TRANS4 - simulation", {

  # Import your NONMEM model using pharmpy
  pmxtran <- importNONMEM(paste0(testFolder, "models/subroutine/advan3_trans4.mod"))
  
  # Convert to RxODE model
  rxodeMod <- pmxtran %>% toPmxModel() %>% pmxmod::export(dest="RxODE")

  # Loading model in RxODE
  mod <- RxODE::RxODE(paste0(rxodeMod@code, collapse="\n"))

  # Dosing regimen
  ev <- RxODE::et(amount.units="mg", time.units="hours") %>%
    RxODE::et(amt=1000, cmt="A_CENTRAL")
 
  # Sampling time
  ev <- ev %>% RxODE::et(0, 48, length.out=100)
 
  # Simulate
  sim  <- RxODE::rxSolve(mod, params=rxodeMod@theta, ev, omega=rxodeMod@omega, sigma=rxodeMod@sigma, nSub=100)
 
  # Plotting C2
  plot(sim, CP) +
    ylab("Concentration") 
})

test_that("ADVAN4 TRANS4 - simulation (F not correct)", {
  
  # Import your NONMEM model using pharmpy
  pmxtran <- importNONMEM(paste0(testFolder, "models/subroutine/advan4_trans4.mod"))
  
  # Convert to RxODE model
  rxodeMod <- pmxtran %>% toPmxModel() %>% pmxmod::export(dest="RxODE")
  
  # Loading model in RxODE
  mod <- RxODE::RxODE(paste0(rxodeMod@code, collapse="\n"))
  
  # Dosing regimen
  ev <- RxODE::et(amount.units="mg", time.units="hours") %>%
    RxODE::et(amt=1000, cmt="A_DEPOT")
  
  # Sampling time
  ev <- ev %>% RxODE::et(0, 48, length.out=100)
  
  # Simulate
  sim  <- RxODE::rxSolve(mod, params=rxodeMod@theta, ev, omega=rxodeMod@omega, sigma=rxodeMod@sigma, nSub=100)
  
  # Plotting C2
  plot(sim, CP) +
    ylab("Concentration")
})

test_that("Custom test with RxODE", {
  
  mapping <- mapping(theta=c("CL"=1, "V1"=2, "V2"=3, "Q"=4),
                     omega=c("CL"=1, "V1"=2, "V2"=3, "Q"=4),
                     sigma=c("PROP"=1))
  
  # Import your NONMEM model using pharmpy
  pmxtran <- importNONMEM(paste0(testFolder, "models/subroutine/advan3_trans4.mod"), mapping)
  
  # Convert to RxODE model
  rxodeMod <- pmxtran %>% toPmxModel() %>% pmxmod::export(dest="RxODE")
  
  mod <- RxODE::RxODE("
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
  ev <- RxODE::et(amount.units="mg", time.units="hours") %>%
    RxODE::et(amt=1000, cmt="A_CENTRAL")
  
  # Sampling time
  ev <- ev %>% RxODE::et(0, 48, length.out=100)
  
  # Simulate
  sim  <- RxODE::rxSolve(mod, params=rxodeMod@theta, ev, omega=rxodeMod@omega, sigma=rxodeMod@sigma, nSub=100)
  
  results <- as.data.frame(sim) 
  results <- results %>% dplyr::mutate(time=as.numeric(time)) %>% dplyr::filter(time==0)
  
  # Plotting C2
  plot(sim, CP) +
    ylab("Concentration") 
})