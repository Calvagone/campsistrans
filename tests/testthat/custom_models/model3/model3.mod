
$PROB XXXXXXXXX
;----------------------------------
$INPUT
ID ;	Unique subject identifier across all studies from 1 to n
STUDY ;	 center number
AMT ;	Amount Dose Given 
TIME ;	Time since first dose
TSLD
CMT ;	Compartment data item
XDV ;	Dependent variable
LNDV=DV ;	Log of dependent variable
EVID ;	Event Identification data item
MDV ;	Missing Dependent Variable
DOSE ; dose
SS; Steady state dose item
II; interdose interval 24h
FOOD ;	0=Fasted,1=Fed ; -99=?
SEX ;	Sex
AGE ;	Age
BW0 ;	Body weight at baseline
BMI0 ;	Body mass index
OCC


;----------------------------------
$DATA ID

;----------------------------------
$SUBROUTINES ADVAN4

;IOV
$ABBREVIATED REPLACE ETA(OCC_CL)=ETA(,8 to 20 by 1)
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
$PK

;;; F1BMI0-DEFINITION START
   F1BMI0 = ((BMI0/23.85)**THETA(16))
;;; F1BMI0-DEFINITION END

;;; F1-RELATION START
F1COV=F1BMI0
;;; F1-RELATION END


;;; CLSEX-DEFINITION START
; Frequency of most common case is 171/188=0.909574
IF(SEX.EQ.1) CLSEX_COMMON=1; Most common case, indicator variable is 1
IF(SEX.EQ.2) CLSEX_COMMON=0
CLSEX = (1 + THETA(15)*(1-CLSEX_COMMON)) 
;;; CLSEX-DEFINITION END


;;; CLAGE-DEFINITION START
   CLAGE = ((AGE/38)**THETA(14))
;;; CLAGE-DEFINITION END

;;; CL-RELATION START
CLCOV=CLAGE*CLSEX
;;; CL-RELATION END

;----------------------------------
TVF1  = THETA(1)
TVF1 = F1COV*TVF1
TVKA  = THETA(2)
TVCL  = THETA(3) 
TVCL = CLCOV*TVCL
TVV1  = THETA(4)
TVQ   = THETA(5) 
TVV2  = THETA(6)

;--- residual error paramters -----
SDEARLY = 1
IF(TSLD.LT.0.75) SDEARLY=THETA(7)
addRUV101  = THETA(10)
propRUV101 = THETA(11)

SDSTUD = 1
IF(STUDY.EQ.201) SDSTUD = THETA(8)
IF(STUDY.EQ.203) SDSTUD = THETA(9)
IF(STUDY.EQ.202) SDSTUD = THETA(17)
IF(STUDY.EQ.103) SDSTUD = THETA(18)


;------- covariates ---------------

XFOOD =FOOD 
;IF(IDO.EQ.103003) XFOOD = 0
;IF(IDO.EQ.203001) XFOOD = 0
;IF(IDO.EQ.602001) XFOOD = 0
;IF(IDO.EQ.602007) XFOOD = 0

IF(STUDY.EQ.203) XFOOD = 1 ; all fed in 203

KAFOOD = 1
IF(XFOOD.EQ.1) KAFOOD =  1+ THETA(12)


F1FOOD = 1
IF(XFOOD.EQ.1) F1FOOD =  1+ THETA(13)



FORM = 2
KAFORM = 1
IF(FORM.EQ.1) KAFORM =  1+ THETA(19)  ; PH1 form in 101
IF(FORM.EQ.3) KAFORM =  1+ THETA(19)  ; PH1 form in 103

F1FORM= 1
IF(FORM.EQ.1) F1FORM=  1+ THETA(20)
IF(FORM.EQ.3) F1FORM=  1+ THETA(20)

KASTUDY = 1
IF(STUDY.EQ.103) KASTUDY =  1+ THETA(21)  ; study effect on KA

F1STUDY = 1
IF(STUDY.EQ.103) F1STUDY =  1+ THETA(22)  ; study effect on F1


; IOV ---------------------------
IOVCL = ETA(OCC_CL)

;---- individual parameters -------
F1    = TVF1 *  EXP(ETA(1)) * F1FOOD*F1FORM*F1STUDY
ALAG1 = 0
KA    = TVKA *  EXP(ETA(2)) * KAFOOD*KAFORM*KASTUDY
CL    = TVCL *  EXP(ETA(3)+IOVCL) * (BW0/75)**0.75
V1    = TVV1 *  EXP(ETA(4)) * (BW0/75)**1
Q     = TVQ  *  EXP(ETA(5)) * (BW0/75)**0.75
V2    = TVV2 *  EXP(ETA(6)) * (BW0/75)**1
K=CL/V1
K23=Q/V1
K32=Q/V2
S2 = V1	;cmp 2 = central compartment

AUCSS = F1*DOSE/CL

;-----------------------------------
ALAG1 = 0 

;IF(IDO.EQ.10) THEN 
;ALAG1 = 0.5
;ENDIF 
;IF(IDO.EQ.12) THEN 
;ALAG1 = 0.5
;ENDIF 

;----------------------------------
$ERROR
;----------------------------------
IPRED=LOG(F+0.0001) 
W = SQRT( propRUV101**2 + addRUV101**2/EXP(IPRED)**2 )*SDEARLY*SDSTUD
IF (W.EQ.0) W = 1
IRES = DV-IPRED
IWRES = IRES/W
Y= IPRED+W*ERR(1)*EXP(ETA(7))
;----------------------------------

;----------------------------------


$THETA
(1) FIX ; F1
(0, 5.33)  ; KA
(0, 0.81)  ; CL
(0, 20.0)  ; V1
(0, 0.901)  ; Q
(0, 4.68)  ; V2
(0, 6.77)  ; SDEARLY
(0, 1) FIX ; SDRUV201
(0, 1) FIX ; SDRUV203
(0) FIX ; addRUV101
(0, 0.0859)  ; propRUV101
(-1, -0.835,10)  ; KA_FOOD
(-1, -0.041,10)  ; F1_FOOD
(-100,0.0803,100000) ; CLAGE1
(-1.099,-0.188,11.059) ; CLSEX1
(-100,0.403,100000) ; F1BMI01
(0, 1)  ; SDRUV202
(0, 1)  ; SDRUV103
(-1, 0,10)FIX  ; KA_FORM
(-1, 0,10)FIX  ; F1_FORM
(-1, -0.0826,10)  ; KA_S103
(-1, 0 ,10) FIX  ; F1_S103
;#  F1 KA CL V1 Q V2 SDEARLY SDRUV201 SDRUV203 addRUV101 propRUV101 KA_FOOD F1_FOOD CLAGE1 CLSEX1 F1BMI01 SDRUV202 SDRUV103 KA_FORM F1_FORM KA_S103 F1_S103 KA_FOOD_PAT F1_FOOD_PAT
;-------------------------------------------------------------------------------
$OMEGA
 0.0168 ; IIV_F
 0.3506   ; IIV_KA
 0.0223 ; IIV_CL
 0 FIX  ; IIV_V1
 0 FIX   ; IIV_Q
 0.112   ; IIV_V2
 0 FIX ; ETA_EPS
$OMEGA BLOCK(1) 0.01845 ; IOV_CL
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
$OMEGA BLOCK(1) SAME
;# IIV_F IIV_KA IIV_CL IIV_V1 IIV_Q IIV_V2 ETA_EPS
;-------------------------------------------------------------------------------
$SIGMA
 1 FIX  ;residual variability (do not change!)
; Estimation settings -------------

$EST METHOD=1 INTERACTION MAXEVAL=0 SIG=3 PRINT=5 NOHABORT NOOBT NOTBT NONINFETA=1 MCETA=5 RANMETHOD=4P SADDLE_RESET=1



















