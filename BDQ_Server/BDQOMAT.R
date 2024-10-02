code <- "
$PROB 
  - author: model specification
  - tittle: BDQ and M2 popPK in patients plus time varying albumin and weight
  - date:July 12, 2022
 
$PLUGIN nm-vars

$PARAM @covariates
  DOSE    = 400,     
  ALB     = 3.65,
  RACE    = 1,
  AGE     = 32,
  WT      = 56.6,
  FLAG    = 1,
  OCC     = 1,
  regimen = 1
   
$PARAM @annotated  
// Albumin 
  THETA1  :  3.64865      : X0 g/dl
  THETA2  :  4.04068      : Xss g/dl
  THETA3  :  0.03399109   : Rate constant return to normal 1/week 
  THETA4  :  -2.43936     : shape factor BSVX0 boxcox 
  THETA5  :  -5.37749     : shape factor BSVXSS boxcox
      
      // Body weight 
  THETA6  :  56.6371      : WT0 kg 
  THETA7  :  62.6425      : WT120 kg 
  THETA8  :  -0.416034    : Boxcox BSV WT120

// BDQ and M2 PK 
  THETA9  :  0.662045     : MAT
  THETA10 :  0.466443     : FR
  THETA11 :  2.61592      : CL
  THETA12 :  198.34       : V 
  THETA13 :  3.658        : Q1 
  THETA14 :  8549.06      : VP1 
  THETA15 :  7.33504      : Q2 
  THETA16 :  2690.91      : VP2 
  THETA17 :  10.0496      : CLM2 
  THETA18 :  2203.71      : VM2
  
// Covariates 
  THETA19 :  0.180912     : Allometric scaling baseline CL
  THETA20 :  1.0          : Allometric scaling V 
  THETA21 :  1.0          : Time varying FU on BDQ+M2 disp 
  THETA22 :  1.64021      : Individual time varying effect of ALB BDQ+M2 CL 
  THETA23 :  0.838743     : Effect of black race on CL/CM2 
  THETA24 :  0.00880756   : AGE effect on CL/CLM2
 
 // IE on BDQ CL
  THETA25   :    1       : IEBDQ
 
//IE on M2 CL 
  THETA26   :    1        : IEM2
  
// Albumin and body weight 
$OMEGA @annotated @block 
  ETA1  : 0.0253964                                                : BSVX0
  ETA2  : 0.00787165 0.00965573                                    : BSVXSS 
  ETA3  :-0.0831085  0.00887115  0.979314                          : BSVREP 
  ETA4  : 0.0117627  0.00499615 -0.0379866  0.0421083              : BSVWT0 
  ETA5  : 0.00559706 0.00649501 -0.00226264 0.0369625  0.0494914   : BSVRWT120

// BDQ and M2 PK
$OMEGA @annotated @block 
  ETA6  : 0.0382322 : BOV F 
$OMEGA @annotated @block 
  ETA7  : 0.0382322 : BOV F

$OMEGA @annotated @block  
  ETA8  : 1.16205             : BOV on MAT 
$OMEGA @annotated @block  
  ETA9  : 1.16205             : BOV on MAT
  
$OMEGA @annotated 
  ETA10 : 0.0803271           : BSV on F

$OMEGA @annotated @block  
  ETA11 : 0.152776            : 11 BSV on CL 
  ETA12 : 0.13486 0.212124    : 12 BSV on CLM2

$OMEGA @annotated  
  ETA13 : 0.171909            : BSV on V 
  ETA14 : 0.181182            : BSV on Q1
  ETA15 : 0.150223            : BSV on VM2

$OMEGA @annotated @block  
  ETA16 : 0.05392             : BSV on RUVBDQ
  ETA17 : 0.0295137 0.0522882 : BSV on RUVM2

$SIGMA @annotated @block  
  EPS1 : 0.00500974           : Prop error ALB 
$SIGMA @annotated @block  
  EPS2 : 0.00114573           : Prop error WT

$SIGMA @annotated @block   
  EPS3 : 0.0518161            : Prop error TMC
  EPS4 : 0.0189319 0.0366836  : Prop error M2

$MAIN                    
// Model for Albumin
  double TVX0    = THETA1                    ;
  double TVXSS   = THETA2                    ;
  double TVREP   = THETA3                    ;

  double BSVX0   = ETA(1)                    ; 
  double BSVXSS  = ETA(2)                    ; 
  double BSVREP  = ETA(3)                    ; 
  
  double SHPX0   = THETA4                    ;
  double PHIX0   = exp(BSVX0)                ;
  double PHI2X0  = (pow(PHIX0,SHPX0)-1)/SHPX0  ; // Box-cox transformation
  
  double SHPXSS  = THETA5                    ;
  double PHIXSS  = exp(BSVXSS)               ;
  double PHI2XSS = (pow(PHIXSS,SHPXSS)-1)/SHPXSS; // Box-cox transformation
  
  double X0      = TVX0*exp(PHI2X0)          ;
  double XSS     = TVXSS*exp(PHI2XSS)        ;
  double REP     = TVREP*exp(BSVREP)         ;
  double HL      = log(2)/REP                ;
  
  A_0(8)         = X0                        ;

// Model for WT
  double TVWT0    = THETA6                   ;
  double TVWT120  = THETA7                   ;

  double BSVWT0   = ETA(4)                   ;
  double BSVWT120 = ETA(5)                   ;


  double SHPWT120  = THETA8                  ;
  double PHIWT120  = exp(BSVWT120)           ;
  double PHI2WT120 = (pow(PHIWT120,SHPWT120)-1)/SHPWT120; // Box-cox transformation

  double WT0   = TVWT0*exp(BSVWT0)           ;
  double WT120 = TVWT120*exp(PHI2WT120)      ;
  double SLOPE = (WT120 - WT0)/(120*7*24)    ; // TIME in hours, 120 weeks

// BDQ and M2 PK
// Typical values fixed effects 
  double TVMAT  = THETA9                     ;
  double TVFR   = THETA10                    ;
  double TVCL   = THETA11                    ;
  double TVV    = THETA12                    ;
  double TVQ1   = THETA13                    ;
  double TVVP1  = THETA14                    ;
  double TVQ2   = THETA15                    ;
  double TVVP2  = THETA16                    ; 
  double TVCLM2 = THETA17                    ;
  double TVVM2  = THETA18                    ;
  
  // IE on BDQ CL
  double IEBDQ    =   THETA25                 ;
 
//IE on M2 CL 
  double IEM2      = THETA26                  ;



// Typical values variability
  double BOVF               =  ETA(6)        ;
  if(OCC==2) {
   BOVF =  ETA(7);
  } 
  double BOVMAT             =  ETA(8)        ;
  if(OCC==2) {
   BOVMAT = ETA(9);
  }       
  double BSVF               =  ETA(10)       ;
  double BSVCL              =  ETA(11)       ;
  double BSVCLM2            =  ETA(12)       ;
  double BSVV               =  ETA(13)       ;
  double BSVQ1              =  ETA(14)       ;
  double BSVVM2             =  ETA(15)       ;

// Covaraiate model
// Mechanistic
// Allomertic scaling and albumin effects coded in $DES 
// and $ERROR since they are time changing Empiric
// Effect of Black race on CL 
  double BLACK            =  0              ;                    
  if(RACE==2) {
    BLACK = 1.0;
  }                   
  double BLACKCL          = 1 + BLACK*THETA23;

// AGE on CL 
  double AGECL           = 1 + (32-AGE)*(THETA24);

//  Parameters
  double PHI  = log(TVMAT/(1-TVMAT)) + BOVMAT;
  double MAT  = 6*exp(PHI)/(exp(PHI) + 1)    ;
// Mean absorption time, overall time for both delay and 90% complete absorption,  
// logit transformed to retain constraines even with BOV in MAT 
  double FR   = TVFR                        ; //Fraction of MAT that is delay 
  double MTT  = MAT*FR                      ; 
  double KAHL = MAT*(1-FR)/3.3              ;
  double KA   = log(2)/KAHL                 ;
  double KTR  = 2/MTT                       ;

  F1   = exp(BOVF + BSVF)            ;
// AMT in mg in input file, MW TMC207 555.5 g/mol, DV as nmol/mL = μmol/L -->
// (AMT/1000)/(555.5)*1000000 = AMT*1.8002 

  double CLB   = TVCL*BLACKCL*AGECL*exp(BSVCL) *THETA25 ;
  double VB    = TVV*exp(BSVV)                       ;
  double Q1B   = TVQ1*exp(BSVQ1)                     ;
  double VP1B  = TVVP1                               ;
  double Q2B   = TVQ2                                ;
  double VP2B  = TVVP2                               ;
  double CLM2B = TVCLM2*BLACKCL*AGECL*exp(BSVCLM2)*THETA26 ; 
  double VM2B  = TVVM2*exp(BSVVM2)                   ;   
  
  
$CMT @annotated
DEPOT DEFDOSE : 1 Dosing compartment for a ref product
BDQC          : 2 BDQ Central compartment
BDQPERI1      : 3 BDQ Peripheral Compartment 1
BDQPERI2      : 4 BDQ Peripheral Compartment 2
M2            : 5 M2 Compartment 
TRANSI1       : 6 Transit Compartment 1
TRANSI2       : 7 Transit Compartment 2
ALBUMIN       : 8 Albumin Compartment
AUCBDQ        : 9 AUC BDQ Compartment
AUCM2         : 10 AUC M2 Compartment
  
$ODE // $DES
//Albumin 
  DADT(8)          = log(2)/(HL*7*24)*A(8)*(1- A(8)/XSS) ; //Time in hours, unit of half life: weeks

//Body weight
  double WTTIME    = WT0 + T*SLOPE                     ; //Predicted individual WT at time T

//Time varying covariates 
  double ALBRELI   = A(8)/XSS ;
// Time varying individual albumin relative individual value albumin at SS 
  double COVALBI   = pow(ALBRELI, THETA22)              ; //Albumin effect on hepatic function --> CL 
  double FM        = pow(ALBRELI,-THETA22)              ; //Albumin effect on hepatic function --> fm
  
  double ALLCL     = pow(WTTIME/70, THETA19)            ; //Allometric scaling CL/Q 
  double ALLV      = pow(WTTIME/70, THETA20)            ; //Allometric scaling V/VP
  
//--- BDQ and M2 PK 
  double CL   = CLB*COVALBI*ALLCL                      ;
  double V    = VB*ALLV                                ;
  double Q1   = Q1B*ALLCL                              ;
  double VP1  = VP1B*ALLV                              ;
  double Q2   = Q2B*ALLCL                              ;
  double VP2  = VP2B*ALLV                              ;
  double CLM2 = CLM2B/FM*COVALBI*ALLCL                 ;
  double VM2  = VM2B/FM*ALLV                           ;

  DADT(1) = -KTR*A(1)                           ;
  DADT(2) = A(7)*KA - A(2)*CL/V - A(2)*Q1/V + A(3)*Q1/VP1 - A(2)*Q2/V + A(4)*Q2/VP2; //BDQ 
  DADT(3) = A(2)*Q1/V - A(3)*Q1/VP1             ;
  DADT(4) = A(2)*Q2/V - A(4)*Q2/VP2             ;
  DADT(5) = A(2)*CL/V - A(5)*CLM2/VM2           ; //M2 
  DADT(6) = A(1)*KTR - A(6)*KTR                 ; //transit1 
  DADT(7) = A(6)*KTR - A(7)*KA                  ; //transit2
  DADT(9) = A(2)/(V*COVFUIP)                    ;
  DADT(10) = A(5)/(VM2*COVFUIP)                 ;

$TABLE // $ERROR
//Body weight 
  double WTTIMEE            = WT0 + TIME*SLOPE        ;
  
// Time varying covariates 
  double FURELIP            = THETA2/A(8)             ;
  
//Time varying individual fraction unbound relative typical population value at SS (inversely 
//proportional to albumin)

  double COVFUIP            = pow(FURELIP,THETA21)    ; //Fu effect on V
   
  double ALBRELIE           = A(8)/XSS                ;        
// Time varying individual albumin relative individual value albumin at SS

  double FME                = pow(ALBRELIE,-THETA22)  ; // Albumin effect on hepatic function --> fm
  double ALLCLE             = pow(WTTIMEE/70,THETA19) ; // Allometric scaling CL/Q 
  double ALLVE              = pow(WTTIMEE/70,THETA20) ; // Allometric scaling V/VP
  
// BDQ and M2 PK
  double VE                  =   VB*ALLVE*COVFUIP     ;
  double VM2E                =   VM2B/FME*ALLVE*COVFUIP;    
  
// FLAG 1 = BDQ PK, 2 = M2 PK, 3= Albumin, 4 = Body weight
 double DEL                  =   1E-12                ;
 double IPRED                =   log(A(2)/VE + DEL)   ;
 
 double IPREDM2              =   log(A(5) / VM2E + DEL);
  
 double IPREDALB             =   A(8);

 double IPREDWT              =   WTTIMEE;
        
 double AAUCBDQ              =   A(9) ;
 double AAUCM2               =   A(10) ;
 
//Error additive on log scale for PK, proportional for albumin and weight 
  double BSVRUV1            =   ETA(16)               ;
  double BSVRUV2            =   ETA(17)               ;
  
  double W                  =   1*exp(BSVRUV1)            ;
  if (W == 0) W             =   1.0                       ;
  double WM2                =   1*exp(BSVRUV2)            ;
  if (WM2 == 0) WM2         =   1.0                       ;
  double WALB               =   IPREDALB                  ;
  if (WALB == 0) WALB       =   1.0                       ;
  double WWT                =   IPREDWT                   ;
  if (WWT == 0) WWT         =   1.0                       ;

  double IPREDD             =   IPRED    + W*EPS(3)       ;
  double IPREDDM2           =   IPREDM2  + WM2*EPS(4)     ;
  double IPREDDALB          =   IPREDALB + WALB*EPS(1)    ;
  double IPREDDWT           =   IPREDWT  + WWT*EPS(2)     ;


// ORIGINAL CODE  
//  double W                  =   1*exp(BSVRUV1)        ;
//  if(FLAG==2) {
//    W = 1*exp(BSVRUV2);
//  }       
//  if(FLAG==3) {
//    W = IPRED;
//  }                
//  if(FLAG==4) {
//    W = IPRED;
//  }               
//  if(W==0){
//    W = 1.0;
//  }                 
//  double Y                  =  IPRED + W*EPS(3)       ;
//  if(FLAG==2) {
//    Y  = IPRED + W*EPS(4);
//  }    
//  if(FLAG==3) {
//     Y = IPRED  + W*EPS(1);
//  }     
//  if(FLAG==4) {
//     Y = IPRED  + W*EPS(2);
//  }    
  
$CAPTURE ALB WT RACE AGE AMT IPRED IPREDM2 IPREDALB IPREDWT AAUCBDQ AAUCM2 regimen
"
