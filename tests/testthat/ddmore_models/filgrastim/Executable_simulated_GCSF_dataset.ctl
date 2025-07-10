$PROBLEM  PKPD NEUPOGEN BASELINE(BAS) at 0.0246 ng/ml

$INPUT ID AMT TIME CMT DV DRUG PERD ROUT BAS RATE

$DATA Simulated_GCSF_dataset.csv IGNORE=I
      IGNORE=(DRUG.EQ.0,PERD.EQ.2,ID.EQ.103052)
    

$SUBROUTINE ADVAN6 TOL=6
$MODEL
NCOMP = 14


COMP = (ABS)
COMP = (CENT)
COMP = (FR)
COMP = (DR)
COMP = (B1)
COMP = (B2)
COMP = (B3)
COMP = (B4)
COMP = (B5)
COMP = (B6)
COMP = (B7)
COMP = (B8)
COMP = (B9)
COMP = (NB)

$ABBREVIATED DERIV2=NO

$PK
 
   FF    = THETA(1)
   KA    = THETA(2)*EXP(ETA(4))
   FR    = THETA(3)
   D2    = THETA(4)
   KEL   = THETA(5)*EXP(ETA(2))
   VD    = THETA(6)*EXP(ETA(3))
   KD    = THETA(7)
   KINT  = THETA(8)
   KSI   = THETA(9)*EXP(ETA(5))
   KOFF  = THETA(10)
   KMT   = THETA(11)
   KBB1  = THETA(12)
   KTT   = THETA(13)    
   NB0    = THETA(14)*EXP(ETA(1))
   SC1    = THETA(15)*EXP(ETA(6))
   SM1    = THETA(16)*EXP(ETA(7))
   SM2    = THETA(17)*EXP(ETA(8))
   SM3    = THETA(18)
   CP0   = BAS
    F1=FF
    F2=0
    
    
   
   IF (ROUT.EQ.1) THEN
    F1=0
    F2=1
   ENDIF 

   KON   = KOFF / KD

; HILL FUNCTION AT BASELINE 
  
   H10     = 1 + SM1 * CP0 / (SC1 + CP0)
   H20     = 1 + SM2 * CP0 / (SC1 + CP0)
   H30     = 1 + SM3 * CP0 / (SC1 + CP0)

; SECONDARY RELATIONSHIPS
  
   KINB = KMT * NB0/ H10
   BM10 = KINB * H10/ (KBB1 * H30 + KTT * H20)
   BM20 = KTT * H20 * BM10/(KBB1 * H30 + KTT * H20)   
   BM30 = KTT * H20 * BM20/(KBB1 * H30 + KTT * H20) 
   BM40 = KTT * H20 * BM30/(KBB1 * H30 + KTT * H20) 
   BM50 = KTT * H20 * BM40/(KBB1 * H30 + KTT * H20)
   BM60 = KTT * H20 * BM50/(KBB1 * H30 + KTT * H20)
   BM70 = KTT * H20 * BM60/(KBB1 * H30 + KTT * H20)
   BM80 = KTT * H20 * BM70/(KBB1 * H30 + KTT * H20)
   BM90 = KTT * H20 * BM80/(KBB1 * H30 + KTT * H20)

   NT0      = BM10+BM20+BM30+BM40+BM50+BM60+BM70+BM80+BM90+NB0
   AC0      = CP0 * VD  
   RTOT0    = KSI*NT0
   ADR0     = RTOT0*AC0/(KD+CP0)

   KIN  = KEL * AC0 + KINT * ADR0
      
;----- INITIAL CONDITIONS  

  A_0(1)  = 0
  A_0(2)  = AC0+ADR0
  A_0(3)  = 0
  A_0(4)  = 0
  A_0(5)  = BM10
  A_0(6)  = BM20
  A_0(7)  = BM30
  A_0(8)  = BM40
  A_0(9)  = BM50
  A_0(10) = BM60
  A_0(11) = BM70
  A_0(12) = BM80
  A_0(13) = BM90
  A_0(14) = NB0

  
  
$DES    
    
   ABSO     = A(1)
   ATOT    = A(2)
   
   ;ADR     = A(4)
   BM1     = A(5)
   BM2     = A(6)
   BM3     = A(7)
   BM4     = A(8)
   BM5     = A(9)
   BM6     = A(10)
   BM7     = A(11)
   BM8     = A(12)
   BM9     = A(13)
   NB      = A(14)
   NT      = BM1+BM2+BM3+BM4+BM5+BM6+BM7+BM8+BM9+NB
   RTOT    = KSI*NT
   BB=RTOT-A(2)/VD+KD
   CP=0.5*(-BB+SQRT(BB**2+4*KD*A(2)/VD))  
   AC= CP*VD
   ADR=RTOT*AC/(KD+CP)

   H1     = 1 + SM1 * CP / (SC1 + CP)
   H2     = 1 + SM2 * CP / (SC1 + CP)
   H3     = 1 + SM3 * CP / (SC1 + CP)
   
   
;-------- SET OF DIFFERENTIAL EQUATIONS:
             
DADT(1) = - KA * ABSO
DADT(2) =   KA * ABSO  + KIN - KEL * AC - KINT * ADR
DADT(3) =   0
DADT(4) =   0
 
DADT(5) =   KINB* H1*(BM1/BM10) - KBB1 * H3 * BM1 - KTT * H2 * BM1
DADT(6) =   KTT * H2 * BM1 - KBB1 * H3 * BM2 - KTT * H2 * BM2
DADT(7) =   KTT * H2 * BM2 - KBB1 * H3 * BM3 - KTT * H2 * BM3
DADT(8) =   KTT * H2 * BM3 - KBB1 * H3 * BM4 - KTT * H2 * BM4
DADT(9) =   KTT * H2 * BM4 - KBB1 * H3 * BM5 - KTT * H2 * BM5
DADT(10) =  KTT * H2 * BM5 - KBB1 * H3 * BM6 - KTT * H2 * BM6
DADT(11) =  KTT * H2 * BM6 - KBB1 * H3 * BM7 - KTT * H2 * BM7
DADT(12) =  KTT * H2 * BM7 - KBB1 * H3 * BM8 - KTT * H2 * BM8
DADT(13) =  KTT * H2 * BM8 - KBB1 * H3 * BM9 - KTT * H2 * BM9
DADT(14) =  KTT * H2 * BM9 + KBB1 * H3 *(BM1+BM2+BM3+BM4+BM5+BM6+BM7+BM8+BM9)-KMT*NB
   
$ERROR   
;----------SOLUTIONS:

   
   ZNB   = A(14)
   ZNT=A(5)+A(6)+A(7)+A(8)+A(9)+A(10)+A(11)+A(12)+A(13)+A(14)
   RRTOT    = KSI*ZNT
   BBB=RRTOT-A(2)/VD+KD
   ZCP=0.5*(-BBB+SQRT(BBB**2+4*KD*A(2)/VD))  
   
;----- OUTPUT:


IF (CMT.EQ.2) THEN
   IPRED = ZCP
   IRES  = DV-IPRED
   Y     = IPRED*(1+ERR(2))+ERR(1)
ENDIF
IF (CMT.EQ.4) THEN
   IPRED = ZNB
   IRES  = DV-IPRED
   Y     = IPRED*(1+ERR(4))+ERR(3)
ENDIF
   
   
;------THETAS:

$THETA 

(0,0.666,1)          ;1 FF
(0,0.512,100)       ;2 KA1
;(0,0.3,1)          ;3 FR
1 FIX
(0,6.77,20) FIX     ;4 D2
(0,0.212,0.5)         ;5 KEL
(0,2.98,10)         ;6 VD
(0,0.104,50)         ;7 KD
(0,0.0903,1)         ;8 KINT 
(0,0.022,50)        ;9 KSI
;(0,0.00835,0.01)   ;10 KOFF
0 FIX
(0,0.075,1)         ;11 KMT
;(0,0.00167,0.1)    ;12 KBB1
0 FIX
(0,0.00421,1)        ;13 KTT
(0,2.07, 10)        ;14 NB0
(0,1.86,10)         ;15 SC1
(0,28.5,100 )        ;16 SM1
(0,19.8,50)          ;17 SM2
;(0,0.307,50)       ;18 SM3
0 FIX
;(0,0.02,0.1)        ; C0


;--------OMEGAS:
$OMEGA  
0.0869 
0.01
0.01
0 FIX
0.01
0.01
0.01
0 FIX

;--------SIGMAS
 
$SIGMA 
   0 FIX   
   0.0432      
   2.44 
   0.0523

$ESTIMATION MAXEVAL=9999 PRINT=1 METHOD=1 NOABORT SIGDIG=2 INTERACTION 
 MSFO=GCSF_FINAL_simulated_dataset.msf

$COVARIANCE PRINT=E

