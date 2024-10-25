BDQMSM <- "
$PROB 
  - author: Yu-Jou Lin
  - date: Sep 6, 2024
  - NONMEM run: 10060, final model with actual observed date for death, dataset with correct definition of dropout
  - Multistate model for bedaquiline
  - 1. Simulation without dropout, calculate hypothetical estimands being in 
       active TB, converted, recurrent TB and death states
    2. 

$PLUGIN nm-vars Rcpp

$CMT @annotated
S1      : Active TB state
S2      : Converted state
S3      : Recurrent TB state
S4      : Dropout state
S5      : Death state
DUMMY   : Dummy compartment for DEFOBS

$PARAM @annotated // Theta values
MTT23           : 295.375*24*7     : From Converted to Recurrent TB
MTT32           : 61.8711*24*7     : From Recurrent TB to Converted
MTT14           : 0                : From Active TB to Dropout
MTT24           : 0                : From Converted to Dropout
MTT15           : 192.364*24*7     : From Active TB to Death
MTT25           : 3803.44*24*7     : From Converted to Death
SA              : 11.2899          : SA/10000 in hours-1
PT              : 11.3799          : PT in week
SW              : 5.58883          : SW in week
HZA15           : 1.96131          : HZA15
BetaMTTP12      : 0.442668         : MTTP on trasition 12
BetaHL2_12      : -0.686145        : HL2 on trasition 12
BetaXDR12       : -0.622792        : XDR on trasition 12
BetaC208_14_24  : 0                : C208 study on trasition 14/24
BetaAge14_24    : 0                : Age on trasition 14/24
BetaWT15_25     : -0.0838138       : Baseline weight on trasition 15/25
BetaSEX23       : -0.812999        : Sex on transition 23
BetaMBLend_23   : 0.0371081        : MBLend on transition 23

$PARAM @annotated // reference values for covariate
HL2          : 0.69443             : Half-life (derived PD predictor)
MTTP         : 217.6667            : Mean TTP (hour)
XDR          : 0                   : XDR status (0: MDR/pre-XDR, 1: XDR)
C208         : 0                   : Enrolled in the C208 study (0: CC09 study, 1: C208 study)
AGE          : 33                  : Age (year)
WT           : 55                  : Baseline weight (kg)
SEX          : 0                   : Sex (0: male, 1: female)
MBLend       : 0.000055726         : MBL at week 24 [the end of treatment] (derived PD predictor)
regimen      : 1                : regimen

$PARAM @annotated // initial variables (changing over time in the model)
MTT34        : 0                : From Recurrent TB to Dropout = MTT24
MTT35        : 192.364*24*7     : From Recurrent TB to Death   = MTT15
REP          : 1                : Simulation replicates number
WEEK         : 1                : Time after start of treatment

$OMEGA  0  

$SIGMA  0  

$MAIN
//------------------- Define surge function parameters --------------
  double SAi = SA/10000*exp(BetaHL2_12*(HL2-0.69443)+BetaXDR12*XDR)                     ;    // Surge amplitude in hours-1, scaling for easily computation
  double PTi = PT*24*7*exp(-BetaHL2_12*(HL2-0.69443)-BetaMTTP12*((MTTP-217.6667)/24/7)) ;    // Peak time in weeks
  double SWi = SW*24*7*exp(BetaMTTP12*((MTTP-217.6667)/24/7))                           ;    // Surge width in weeks

//------------------- Define Weibull/Gompertz parameters --------------
// Active infection to Death
  double HZL15 = 1/MTT15 ;

//--------- Initialize DV and probabilities -----------
if (NEWIND != 2) double XDV = 1     ; // initial DV = 1
if (NEWIND != 2) double PREVDV = 1  ; // previous DV

if (NEWIND != 2) {
   double BIO1   = 1 ;	
   double BIO2   = 0 ;
   double BIO3   = 0 ;
   double BIO4   = 0 ;
   double BIO5   = 0 ;
}

//--------- Bioavailability factors -------------------
F1    = BIO1    ;
F2    = BIO2    ;
F3    = BIO3    ;
F4    = BIO4    ;
F5    = BIO5    ;
//----------------------------------------------------------

//--- MBL at the end of treatment ---
double FLAG_MBL = 0              ;
if (TIME > 26*168) FLAG_MBL = 1  ;

$ODE
  double HZ12 = SAi / (pow(((T - PTi) / SWi), 2) + 1)                      ;
  double DEL  = 1e-16                                                      ;
  double WB15 = HZL15 * HZA15 * pow((HZL15 * (T + DEL)), (HZA15 - 1))      ;

//----------- Probability transfer constants -----------

  double K12 = HZ12                                                        ;     
  double K23 = 1/MTT23*exp(BetaSEX23*SEX+BetaMBLend_23*FLAG_MBL*(log(MBLend)-log(0.000055726))) ;
  double K32 = 1/MTT32                                                     ;
  double K14 = 0                                                           ;
  double K24 = 0                                                           ;
  double K34 = 0                                                           ;
  double K15 = WB15*exp(BetaWT15_25*(WT-55))                               ;
  double K25 = 1/MTT25*exp(BetaWT15_25*(WT-55))                            ;
  double K35 = K15                                                         ;

//--Differential equations for the probability of each score
DADT(1) = - A(1)*(K12 + K14 + K15)                                         ; // ACTIVE TB
DADT(2) =   A(1)*K12 +A(3)*K32 -A(2)*K23 -A(2)*K24 -A(2)*K25               ; // CONVERTED
DADT(3) =   A(2)*K23 -A(3)*K32 -A(3)*K34 -A(3)*K35                         ; // RECURRENT TB
DADT(4) =   A(1)*K14 +A(2)*K24 +A(3)*K34                                   ; // DROPOUT
DADT(5) =   A(1)*K15 +A(2)*K25 +A(3)*K35                                   ; // DEATH
DADT(6) =   0                                                              ; // DUMMY PK Compartment

//-------------------------------------------------------
$TABLE

  capture P_1 = A(1)                                                       ; // Probability of observing Active infection state
  capture P_2 = A(2)                                                       ; // Probability of observing Conversion state
  capture P_3 = A(3)                                                       ; // Probability of observing Recurrence state
  capture P_4 = A(4)                                                       ; // Probability of observing Dropout
  capture P_5 = A(5)                                                       ; // Probability of observing Death

//-------------------------------------------------------

// ---------Cumulative probabilities -----
  double CUP1 = A(1)                              ;   // 1
  double CUP2 = A(1) + A(2)                       ;   // 1 + 2
  double CUP3 = A(1) + A(2) + A(3)                ;   // 1 + 2 + 3
  double CUP4 = A(1) + A(2) + A(3) + A(4)         ;   // 1 + 2 + 3 + 4
  double CUP5 = A(1) + A(2) + A(3) + A(4) + A(5)  ;   // 1 + 2 + 3 + 4 + 5
  
   // Loop over each record in the input data
   if (EVID == 0) {
   // Generate a random number
     double Random = R::runif(0, 1);
   
     // Assign USUR based on random number
     double USUR = Random;
     if (USUR <= CUP1) {
       XDV = 1;
     } else if (USUR > CUP1 && USUR <= CUP2) {
       XDV = 2;
     } else if (USUR > CUP2 && USUR <= CUP3) {
       XDV = 3;
     } else if (USUR > CUP3 && USUR <= CUP4) {
       XDV = 4;
     } else {
       XDV = 5;
     }
     
     // Check if PREVDV = 4
     if (PREVDV == 4) XDV = 4;
     
     // Check if PREVDV = 5
     if (PREVDV == 5) XDV = 5;
     
     // Update PREVDV
     PREVDV = XDV;
 }
    

//--------- Update Bioavailability factors -------------------
BIO1=0                 ;
BIO2=0                 ;
BIO3=0                 ;
BIO4=0                 ;
BIO5=0                 ;

if (XDV == 1) BIO1 = 1 ;
if (XDV == 2) BIO2 = 1 ;
if (XDV == 3) BIO3 = 1 ;
if (XDV == 4) BIO4 = 1 ;
if (XDV == 5) BIO5 = 1 ;

// $CAPTURE EVID XDV K12 K23 K32 K14 K24 K15 K25 P_1 P_2 P_3 P_4 P_5 
// $CAPTURE HL2 MTTP XDR C208 AGE WT SEX MBLend FLAG_MBL
$CAPTURE EVID XDV HL2 MTTP XDR WT SEX MBLend P_1 P_2 P_3 P_5 USUR CUP1 CUP2 CUP3 CUP4 CUP5 regimen
"
