$PROBLEM EPS Model_No IIV_No split_for _drug Effect
$INPUT STUD	ID TIME TRT DOSR DOSE FLG DV EVID CMT AMT RACE SEX CTRY CLR FR

;TRT Code: Placebo=1; Olanzapine=2, Haloperidol=3; Paliperidone=4; Ziprasidone=5;  
;TRT JJ-681=6 

$DATA EPS_PALI_ZIPRA_HALO_OLA-681_D2RO_681.csv
IGNORE=@ IGNORE=(TRT.EQ.2)

$SUBROUTINES ADVAN13 TOL=7 

$MODEL
       COMP =(EPS_0)   ;1  NO EPS
       COMP =(EPS_1)   ;2  MILD EPS
       COMP =(EPS_23)   ;3  MODERATE EPS

$PK
; -----------------Rate Constants------------------------------------------------
K12=THETA(1)
K21=0
IF(TIME.LE.16)K21=THETA(2)
IF(TIME.GT.16)K21=THETA(3)

K23=0
IF(TIME.LE.16)K23=THETA(4)
IF(TIME.GT.16)K23=THETA(3)

K32=THETA(2)*EXP(ETA(1))

; ------------------Placebo Effect and time effect paremeters------------------
FOR1=THETA(5)
FOR2=0
IF(TIME.LE.16)FOR2=THETA(6)
IF(TIME.GT.16)FOR2=THETA(5)

BACK=THETA(7)


;; ----------------Drug Effect Parameters--------------------------------------

; ----------------------CSS-----------------------------------------------------
CSS=0
IF(TRT.EQ.3)CSS=(FR*DOSR/(CLR*24))*1000        ; Css for Halo
IF(TRT.EQ.4)CSS=(FR*DOSE/(CLR*24))*1000        ; Css for Pali
IF(TRT.EQ.5)CSS=(FR*DOSR/(CLR*12))*1000        ; Css for Zipra
IF(TRT.EQ.6)CSS=(FR*DOSE/(CLR*12))*1000        ; Css for JJ681

;; ------------------SLOPE---------------------------------------------------------

;EFFH=0
EFFH=THETA(8) ; Slope for HALO
   
;EFFP=0
EFFP=THETA(9) ; Slope for PALI

;EFFZ=0
EFFZ=THETA(10) ; Slope for ZIPRA

;EFFJ=0
EFFJ=THETA(11) ; Slope for JJ681

CTY1=0
IF(CTRY.EQ.1)CTY1=THETA(12)


;;;----------------- Linear Model for drug effect SLOPE*AUC--------------------------

HAL=EFFH*CSS
PAL=EFFP*CSS
ZIP=EFFZ*CSS
JJ6=EFFJ*CSS

FEFF=0

IF(TRT.EQ.3)FEFF=HAL
IF(TRT.EQ.4)FEFF=PAL
IF(TRT.EQ.5)FEFF=ZIP
IF(TRT.EQ.6)FEFF=JJ6


$DES

KF12=(K12*EXP(-FOR1*T))*(1+FEFF)
KB21=K21*EXP(-BACK*T)*(1+CTY1)
KF23=(K23*EXP(-FOR2*T))*(1+FEFF)
KB32=K32*EXP(-BACK*T)*(1+CTY1)
TIM=T
DADT(1)=KB21*A(2)-KF12*A(1)
DADT(2)=KF12*A(1)+KB32*A(3)-KB21*A(2)-KF23*A(2)
DADT(3)=KF23*A(2)-KB32*A(3)

$ERROR

PABS=A(1)
PMIL=A(2)
PMOD=A(3)

Y=1
IF(DV.EQ.1.AND.CMT.EQ.1) Y=A(1)
IF(DV.EQ.1.AND.CMT.EQ.2) Y=A(2)
IF(DV.EQ.1.AND.CMT.EQ.3) Y=A(3)

ABS=A(1)
MIL=A(2)
MOSE=A(3)

$THETA (0,0.178)        ;1 K12
$THETA (0,0.135)        ;2 K21 D<16 & K32 
$THETA (0,0.0703)       ;3 K21 & K23  D>16
$THETA (0,0.263)        ;4 K23 D<16
$THETA (0,0.0782)       ;5 PCB FOR D>16
$THETA (0,0.241)        ;6 PCB FOR D<16
$THETA (0,0.031)        ;7 PCB Back
$THETA  (0,2.03)        ;8 EFF Halo 
$THETA  (0,0.197)        ;9 EFF Pali 
$THETA  (0,0.0087)        ;10 EFF Zipra 
$THETA  (0,0.0192)        ;11 EFF JJ681 
$THETA  (-0.99,0.791)   ;12 Country Effect

$OMEGA 0 FIX

$EST METHOD=1 LAPLACE LIKE NUMERICAL SLOW MAX=9999 PRINT=1 NOTHETABOUNDTEST SIGL=6 NSIG=3 MSFO=msf3
$COV MATRIX=S PRINT=E
