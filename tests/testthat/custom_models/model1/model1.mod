$PROB
$INPUT
$DATA ../dataset.csv
$SUBR       ADVAN13 TOL=6
$MODEL      NCOMP=7 COMP=(DEPOT) COMP=(CENTRAL) COMP=(PERI)
            COMP=(TRANSI1) COMP=(TRANSI2) COMP=(TRANSI3) COMP=(TRANSI4)
$PK	 			 
			 
         IF (NEWIND.NE.2) THEN
             NFOOD=FOOD   		 
         ENDIF

;;;; Covariates
		 
		 KAFD=1+NFOOD*THETA(8)		 
		 LAMB=THETA(9)
		 IMAX=THETA(10)
		 CLBW=(BBW/70)**THETA(11)
		 VDBW=(BBW/70)**THETA(12)
		 ST99=0
		 IF (STUD.EQ.99) ST99=1
		 CL99=1+ST99*THETA(13)		 
		 
;;;; IOV

		 VIS1=0
		 VIS2=0
		 VIS3=0
		 VIS4=0
		 VIS5=0
		 VIS6=0
		 VIS7=0
		 VIS8=0
		 VIS9=0
		 VIS10=0
		 
		 IF (OCC.EQ.1)  VIS1=1
		 IF (OCC.EQ.2)  VIS2=1
		 IF (OCC.EQ.3)  VIS3=1
		 IF (OCC.EQ.4)  VIS4=1
		 IF (OCC.EQ.5)  VIS5=1
		 IF (OCC.EQ.6)  VIS6=1		 
		 IF (OCC.EQ.7)  VIS7=1
		 IF (OCC.EQ.8)  VIS8=1
		 IF (OCC.GT.8.AND.OCC.LE.14) VIS9=1
		 IF (OCC.GT.14) VIS10=1
		 
		 IOVF1A=VIS1*ETA(5)+VIS2*ETA(6)+VIS3*ETA(7)+VIS4*ETA(8)+VIS5*ETA(9)
		 IOVF1B=VIS6*ETA(10)+VIS7*ETA(11)+VIS8*ETA(12)+VIS9*ETA(13)+VIS10*ETA(14)
		 IOVF1=IOVF1A+IOVF1B
		 
		 IOVKA1=VIS1*ETA(15)+VIS2*ETA(16)+VIS3*ETA(17)+VIS4*ETA(18)+VIS5*ETA(19)		 
		 IOVKA2=VIS6*ETA(20)+VIS7*ETA(21)+VIS8*ETA(22)+VIS9*ETA(23)+VIS10*ETA(24)
		 IOVKA=IOVKA1+IOVKA2
		 
;;;; PK parameters
		 
         CL=THETA(1)*CLBW*CL99*EXP(ETA(1))
         V2=THETA(2)*VDBW
         Q3=THETA(3)*CLBW
         V3=THETA(4)*VDBW
         KA=THETA(5)*KAFD*EXP(ETA(3)+IOVKA)
		 
		 F1TM=1-IMAX+IMAX*EXP(-LAMB*TIME)		 
         F1=THETA(6)*F1TM*EXP(ETA(2)+IOVF1)
		 
         K20=CL/V2
         K23=Q3/V2
         K32=Q3/V3
         KTR=KA

         S2=V2
		 
         NFOOD=FOOD		  

		 DONO1=0
		 IF (DONO.EQ.1) DONO1=1

		 		 
$DES  
         CP=A(2)/V2

         DADT(1)=  -KA*A(1)	
         DADT(2)=   KTR*A(7)-K20*A(2)-K23*A(2)+K32*A(3)
         DADT(3)=   K23*A(2)-K32*A(3)
         DADT(4)=   KA*A(1) -KTR*A(4)
         DADT(5)=   KTR*A(4) -KTR*A(5)
         DADT(6)=   KTR*A(5) -KTR*A(6)
         DADT(7)=   KTR*A(6) -KTR*A(7)
		 
$ERROR  
         CONC= A(2)/S2
         IPRED=LOG(0.0001)
         IF (CONC.GT.0) IPRED=LOG(CONC)
        
         W=THETA(7)*EXP(ETA(4))
         Y=IPRED+W*EPS(1)
         IRES=IPRED-DV
         IWRES=IRES/W
		 		 
$THETA  (0,26) (0,59) (0,2.3) (0,9) (0,2.66) 1 FIX 
        (0,0.444) (-5,-0.33,5) (0,0.0444) (0,0.8,1) 0.45 FIX 1 FIX
		(-5,-0.9,5)
$OMEGA   0.03 0.21
$OMEGA   BLOCK(2)
         0.14 
		 0.07 0.05		 
$OMEGA   BLOCK(1) 0.08
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) 0.14
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$OMEGA   BLOCK(1) SAME
$SIGMA   1  FIX
$EST     METHOD=COND INTER,NSIG 3 SIGL 9,PRINT 2,MAX 19999,NOABORT,POSTHOC,NONINFETA=1,
         NOOMEGABOUNDTEST
$COVA    PRINT=E

