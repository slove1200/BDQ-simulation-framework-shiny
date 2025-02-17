
#$INPUT      ID WEEKP TAST REP TIME AMT EVID DV FLAG TYPE LASTR TREATMENT AUCW TBTYPE MTTP
#;ID - Subject ID number
#;WEEKP - Week of sampling 			
#;TAST - Time after start of treatment when the sample were taken [hours]
#;REP - 1/2/3 Numbering of triplicate sputum sample collected at the same TAST 
#;TIME - Time in MGIT tube for each sample [days]
#;AMT - 1 at record initializing time-to-event model, 0 otherwise
#;EVID - 4 at record initializing time-to-event model, 0 otherwise
#;DV - Logistic model: 0/1 negative/positive sample, Time-to-event model: 0/1 censored/event
#;FLAG - 1/2 Flagging records for logistic model (2) and time-to-event model (1)
#;TYPE - 0/1 Flagging records for estimation (1) and simulation (0)
#;LASTR - 0/1 Flag for simulations marking last possible time for MGIT positive signal (42days)
#;TREATMENT - 0/1 placebo or bedaquiline treatment arm
#;CAVG - Individual weekly average concentration [µg/mL]
#;TBTYPE  - 1/2/3/4 Drug sensitivity classification DS/MDR/pre-XDR/XDR 
#;MTTP - Individual mean time to positivity at baseline [hours] 

# TASTW (in weeks) = TAST/24/7 
# MTTP = indiv mean time to positivity at baseline

codeTTP_sim <- "

$PROB 
  - author: Yu-Jou Lin
  - date: Jul 14, 2022
  - NONMEM run: 1
  - TTP model for bedaquiline
  
$PLUGIN nm-vars Rcpp autodec

$PARAM @covariates
TAST = 0, 
TASTW = 0,
MTTP = 163.7,
WEEKP = 0, 
LASTR = 0,
FLAG = 1, 
REP = 1, 
TTPD = 1, 
HLEFF = 0.0
// value = 0.1


$PARAM @annotated
// THETA
  THETA1    : 1.37552   : kG
  THETA2    : 0.476046  : Nmax
  THETA3    : 0.213941  : MBL0
  THETA4    : 0.811166  : HL
  THETA5    : 0.659174  : Box-Cox
  THETA6    : 0.5       : N50 FIX
  THETA7    : 1         : SHAPE FIX
  THETA8    : 0.969     : MAX FIX
  THETA9    : -1        : BDQ effect FIX
  THETA10   : 1.4215    : EC50
  // THETA10   : 2.64064   : EC50 AUCW
  THETA11   : 0.280558  : preAndXDR effect
  THETA12   : -3.68983  : MTTP effect
  THETA13   : 0.952229  : Scaling of hazard

$OMEGA @annotated
ETA1     : 0.331734     : HL
ETA2     : 3.70792      : IOV in MBL week 1
ETA3     : 3.70792      : IOV in MBL week 2
ETA4     : 3.70792      : IOV in MBL week 3
ETA5     : 3.70792      : IOV in MBL week 4
ETA6     : 3.70792      : IOV in MBL week 5
ETA7     : 3.70792      : IOV in MBL week 6
ETA8     : 3.70792      : IOV in MBL week 7
ETA9     : 3.70792      : IOV in MBL week 8
ETA10    : 3.70792      : IOV in MBL week 10
ETA11    : 3.70792      : IOV in MBL week 12
ETA12    : 3.70792      : IOV in MBL week 14
ETA13    : 3.70792      : IOV in MBL week 16
ETA14    : 3.70792      : IOV in MBL week 18
ETA15    : 3.70792      : IOV in MBL week 20

$MAIN //The same as $PK in NONMEM
//=========== DISEASE PROGRESSION MODEL IN PATIENTS ===========
//--- Definition of covariates
  double MTTPEFF = THETA12 ;


//--- Mycobacterial load over time on treatment (TAST)
  double BXPAR   = THETA5 ; 
  double PHI     = exp(ETA(1)) ; 
  double ETATR   = (pow(PHI, BXPAR) - 1)/BXPAR ;  // Box-Cox transformation of the IIV in half-life (HL)
  double N0MBL   = THETA3 * 10000 * pow((MTTP/163.7), MTTPEFF) ; // Number of mycobacterial at start of treatment 
  // double N0MBL   = value ; // For validate TTP model
  double HL      = THETA4 * exp(ETATR) * (1.0+HLEFF/100.0) ; // HL of mycobacterial load modifier
  double KD      = log(2)/HL ;


//--- Calculate delta TAST between records (needed since KD change with TAST due to PK relation)
  if (NEWIND != 2) {                   // First record of new individual
    double DTAST = TASTW ;
    double OMBL = N0MBL ;
    double OTAST = 0 ;
    }
  else {                               // Any record after the first in each individual
    DTAST = TASTW - OTAST ;
    }
  double MBL = OMBL * exp(-KD*DTAST) ; // Underlying mycobacterial load at this TASTW
  OTAST = TASTW ; 
  OMBL = MBL ;
  
  double N0 = MBL ;


//=========== PROBABILITY OF BACTERIAL PRESENCE =========== (Emax model)
  double N50 = THETA6 ; 
  double SHP = THETA7 ;
  double MAX = THETA8 ;
  double P1  =  (MAX*pow(N0, SHP)) / (pow(N50, SHP) + pow(N0, SHP)) ; // Probability of bacterial presence, i.e. positive sample
  double P2  = 1 - P1 ;


//=========== GROWTH MODEL OF MYCOBACTERIA IN MGIT TUBE ===========
  double KGROWTH = THETA1/1000000 ; // Scaling to avoid parameter estimation over several orders of magnitude 
  double NMAX    = THETA2*1000000	;					
  
  if(N0 > NMAX) N0 = NMAX ; // Safety to avoid inoculation > Nmax
  F1 = N0 ; // Amount of bacteria inoculated in MGIT tube 
  
//--- Scaling of hazard
  double SCALEHAZ = THETA13/1000000 ;

$CMT @annotated
  BUGS   : 1 logistic growth model of mycobacteria in MGIT
  HAZARD : 2 hazard function describing obs TTP (proportional to the amount of bacteria in MGIT)

$ODE //$DES
  DADT(1) = A(1)*KGROWTH*(NMAX-A(1)) ;
  DADT(2) = A(1)*SCALEHAZ            ;

  
$TABLE //$ERROR                                                                                       
//--- Logistic model positive/negative sample
if(FLAG == 2 && double DV == 1) double Y = P1      ; // Probability of positive culture
if(FLAG == 2 && DV == 0) Y = 1 - P1          ; // Probability of negative culture

//--- Time-to-event model for TTP
double HAZ = A(1) * SCALEHAZ                 ; // Instantaneous hazard of positive culture
double CHZ = A(2)                            ; // Cumulative hazard
double SURV = exp(-CHZ)                      ; // Survival

// ---------- SIMULATION MODEL ------------------------------;
// For new sample
if (EVID == 4) {
  DV           = 0 ;
  double RTTE  = 0 ;
  double ORTTE = 0 ;
  double USUR1 = R::runif(0,1) ;  // 2nd distribution (uniform)
  double USUR2 = R::runif(0,1) ;  // 3nd distribution (uniform)
}

// Simulate positive or negative sample
if (FLAG == 2) {
  double NEG = 0 ;
  if (USUR1 > P1) { 
  NEG = 1 ;
  }
  RTTE = 0 ;
}

// If there was a previous event
if (ORTTE == 1) RTTE = 0 ;

// If there was no previous event AND the random variable is greater than probability of survival -> event
if (FLAG == 1 && NEG == 0 && ORTTE == 0 && USUR2 > SURV) {
  DV    = 1 ;
  RTTE  = 1 ;
  ORTTE = 1 ;
}

// If there was no previous event AND it is the last record -> censoring 
if (ORTTE == 0 && LASTR == 1) {
  DV    = 0 ;
  RTTE  = 1 ;
  ORTTE = 1 ;
}


// $CAPTURE TAST TASTW WEEKP REP TTPD FLAG DV ETATR NEWIND N0MBL N0 OMBL MBL OTAST DTAST SURV HAZ CHZ ORTTE RTTE USUR1 USUR2 P1 NEG LASTR
$CAPTURE TAST MTTP REP TTPD FLAG DV USUR1 USUR2 HL N0MBL MBL SURV ORTTE RTTE P1 NEG LASTR
"
