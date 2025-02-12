
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
#;AUCW - Individual bedaquiline weekly AUC  [ng*h/mL]
#;TBTYPE  - 2/3/4 Drug sensitivity classification DS or MDR/pre-XDR/XDR 
#;MTTP - Individual mean time to positivity at baseline [hours] 

# TASTW (in weeks) = TAST/24/7 
# MTTP = indiv mean time to positivity at baseline

codeTTP_TrtExperienced <- "

$PROB 
  - author: Yu-Jou Lin
  - date: Nov 18, 2024
  - NONMEM run: 1
  - TTP model for bedaquiline using C209 data, treatment experienced
  
$PLUGIN nm-vars

$PARAM @covariates
TAST = 0, 
TASTW = 0,
preXDR = 0, 
XDR  = 0,
MTTP = 163.7,
AUCW = 150, 
WEEKP = 0, 
TYPE = 2, 
LASTR = 0,
FLAG = 1, 
subjectID = 1, 
REP = 1, 
TTPD = 1, 
regimen = 1,
HLEFF = 0.0
// value = 0.1


$PARAM @annotated
// THETA
  THETA1    : 3.37      : kG
  THETA2    : 0.566    : Nmax*kG
  THETA3    : 0.0522    : MBL0
  THETA4    : 0.897     : HL
  THETA5    : 0.565     : Box-Cox
  THETA6    : 2.64      : EC50
  THETA7    : 0.306     : Joint preXDR+XDR effect
  THETA8    : -3.65     : MTTP effect
  THETA9    : 0.441     : Scaling of hazard*Nmax
  THETA10   : 0.216     : pXDR effect

$OMEGA @annotated
ETA1     : 0.574        : HL
ETA2     : 3.70792      : IOV in MBL

$MAIN //The same as $PK in NONMEM
//=========== DISEASE PROGRESSION MODEL IN PATIENTS ===========
//--- Definition of covariates
  double BDQEFF        = -1 * AUCW/(AUCW + THETA6 * 100) ;
  double MIXEFF        = THETA7                          ; // Joint effect of p-XDR & XDR (because PRIOR from C208)
  double PPX           = 0.8596491                       ; // 49/(49+8), Proportion of PreXDR in C208 NPRE/(NPRE+NXDR)=0.859
  double XDREFF        = (MIXEFF - THETA10*PPX)/(1-PPX)	 ; // XDReff equivalent of joint effect minus effect of pXDR
  double preXDREFF     = THETA10                         ;
  double MTTPEFF       = THETA8                          ;

//--- Mycobacterial load over time on treatment (TAST)
  double BXPAR   = THETA5 ; 
  double PHI     = exp(ETA(1)) ; 
  double ETATR   = (pow(PHI, BXPAR) - 1)/BXPAR ;  // Box-Cox transformation of the IIV in half-life (HL)
  double N0MBL   = THETA3 * 10000 * pow((MTTP/163.7), MTTPEFF) ; // Number of mycobacterial at start of treatment 
  // HL mycobacterial clearance, background effect = 0.28
  double HL      = THETA4 * (1 + BDQEFF) * (1.0 + 0.28) * exp(ETATR) * (1.0/(1.0+HLEFF/100.0)) ; 
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
  
// --- Inter-occasion variability in sputum sampling between weeks
// --- Possible to estimate due to triplicate sputum samples from each sampling occasion
  double IOV = 0 ;
  // if(WEEKP == 1) IOV = ETA(2) ;
  
//=========== PROBABILITY OF BACTERIAL PRESENCE =========== (Emax model)
  double N50 = 0.5                  ; 
  double MAX = 0.969                ;
  double P1  = (MAX*N0 / N50 + N0)  ; // Probability of bacterial presence, i.e. positive sample
  double P2  = 1 - P1               ;


//=========== GROWTH MODEL OF MYCOBACTERIA IN MGIT TUBE ===========
  double KGROWTH = THETA1/1000000   ; // Scaling to avoid parameter estimation over several orders of magnitude 
  double KGNMAX  = THETA2           ; // kg*Nmax
  double NMAX    = KGNMAX/KGROWTH	  ;					
  double N0      = MBL * exp(IOV)   ;
  
  if(N0 > NMAX) N0 = NMAX           ; // Safety to avoid inoculation > Nmax
  F1 = N0                           ; // Amount of bacteria inoculated in MGIT tube 
  
//--- Scaling of hazard
  double SHNMAX   = THETA9          ;
  double SCALEHAZ = SHNMAX/NMAX     ;

$CMT @annotated
  BUGS   : 1 logistic growth model of mycobacteria in MGIT
  HAZARD : 2 hazard function describing obs TTP (proportional to the amount of bacteria in MGIT)

$ODE //$DES
  DADT(1) = A(1)*KGROWTH*(NMAX-A(1)) ;
  DADT(2) = A(1)*SCALEHAZ            ;

  
$TABLE //$ERROR                                                                                       
//--- Logistic model positive/negative sample
if(FLAG == 2 && double DV == 1) capture Y = P1      ; // Probability of positive culture
if(FLAG == 2 && DV == 0) Y = 1 - P1          ; // Probability of negative culture

//--- Time-to-event model for TTP
double HAZ = A(1) * SCALEHAZ                 ; // Instantaneous hazard of positive culture
double CHZ = A(2)                            ; // Cumulative hazard
double SURV = exp(-CHZ)                      ; // Survival
if(FLAG == 1 && DV == 1) Y = SURV*HAZ        ; // Probability density for MGIT positivity signal
if(FLAG == 1 && DV == 0) Y = SURV            ; // Survival in MGIT

// ---------- SIMULATION MODEL ------------------------------;
// For new sample
if (EVID == 4) {
  DV           = 0 ;
  double RTTE  = 0 ;
  double ORTTE = 0 ;
  double USUR1 = (double)rand() / (double)RAND_MAX;  // 2nd distribution (uniform)
  double USUR2 = (double)rand() / (double)RAND_MAX;  // 3nd distribution (uniform)
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
$CAPTURE TASTW WEEKP MTTP REP TTPD FLAG DV HL N0MBL MBL SURV HAZ CHZ ORTTE RTTE P1 NEG LASTR regimen
"
