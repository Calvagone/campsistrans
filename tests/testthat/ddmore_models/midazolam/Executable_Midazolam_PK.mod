$PROBLEM Mida PK in obese adolescents and adults
$INPUT ID   ;<=30 = adults, >30 = adolescent
       TIME ;in min
       AMT  ;in microgram
       ;RATE ;in microgram / min (NLUY: REMOVE RATE for Pharmpy)
       DV   ;in microgram / L
       MDV  
       CMT 
       TBW  ;total bodyweight in kg
       WTAL ;weight for age and length in kg
       WTAC ;access weight
$DATA SimulatedDatafileMidaObesity.csv IGNORE=@
$SUBROUTINES ADVAN6 TOL=5        

$MODEL
COMP=(PODOSE)
COMP=(CENTRAL)
COMP=(PERIP)
COMP=(TRANSIT1)
COMP=(TRANSIT2)
COMP=(TRANSIT3)
COMP=(TRANSIT4)
COMP=(TRANSIT5)

$PK					
IF(ID.LE.30) TVCL=THETA(1) 
IF(ID.GT.30) TVCL=THETA(7)*(TBW/104.7)**THETA(8) 
CL=TVCL*EXP(ETA(1))		;clearance L/min
F1=THETA(2)*EXP(ETA(2))		;bioavailability
V2=THETA(3)*EXP(ETA(5))        	;central volume L 
Q=THETA(4)*EXP(ETA(3))         	;intercompartmental CL L/min 
IF(ID.LE.30) TVV3=THETA(5)*(TBW/141.8)**THETA(9) 
IF(ID.GT.30) TVV3=THETA(5)
V3= TVV3*EXP(ETA(6))            ;peripheral volume L
KA= THETA(6)*EXP(ETA(4))	;absoprtion rate min-1
KTR= KA                         ;transit rate min-1

S2=V2                            
S3=V3

K14=KA
K45=KTR
K56=KTR
K67=KTR
K78=KTR
K82=KTR
K20=CL/V2
K23=Q/V2
K32=Q/V3

$DES
DADT(1)= -K14*A(1)
DADT(2)= KTR*A(8) -K23*A(2) +K32*A(3) -K20*A(2)
DADT(3)= K23*A(2) -K32*A(3)
DADT(4)= K14*A(1) -KTR*A(4)
DADT(5)= KTR*A(4) -KTR*A(5)
DADT(6)= KTR*A(5) -KTR*A(6)
DADT(7)= KTR*A(6) -KTR*A(7)
DADT(8)= KTR*A(7) -KTR*A(8)

$ERROR
IPRED=F                     
Y=F*(1+ERR(1))              ;proportional error model

IRES=DV-IPRED               
DEL=0                       
IF(IPRED.EQ.0)DEL=1
IWRES=(1-DEL)*IRES/(IPRED+DEL)   

$THETA
(0, 0.442) ;CL adults
(0, 0.562) ;F1 
(0, 55.2) ;V2 (L)
(0, 1.14) ;Q (L/min)
(0, 172) ;V3 (L)
(0, 0.115) ;KA 
(0, 0.71) ;CL adolsecents
(0, 1.2) ;tbw pow CL adolescents
(0, 3.28) ;tbw POW V3 adults

$OMEGA                            
 0.0433 ;CL
 0.143 ;F1
 0.165 ;Q 
 0.219 ;KA
 0.294 ;V2 
 0.164 ;V3 

$SIGMA       
 0.0888 ;proportional

$EST SIGDIG=3 MAXEVAL=9999 PRINT=5 NOABORT METHOD=1 INTERACTION POSTHOC
$COV 
$TABLE ID TIME IPRED IWRES CWRES MDV CMT NOPRINT ONEHEADER NOAPPEND FILE=sdtab00134