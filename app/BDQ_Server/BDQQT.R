# In this version, no REPLICATE is included (as this only affects Y, but we use IPRED)
# Also CTIME is not included (fixed as 0, time since midnight), as we are interested in long-term QT


codeQT <- "

$PROB 
  - author: Yu-Jou Lin
  - date: Aug 19, 2023
  - NONMEM run: 1
  - PKQT model for bedaquiline

$PLUGIN nm-vars

$PARAM @covariates // What you input into the model from the dataset
  ARM      = 1, 
  TIMW     = 0, 
  // REPL     = 1, 
  CTIME    = 0,   // clock time, circadian rhythm
  CONCM2   = 0,
  CLOFA    = 0,
  MOXI     = 0,
  CACOR    = 2.440,
  K        = 4.300,
  SEX      = 0,
  RACE     = 0,
  AGE      = 33,
  regimen  = 1


$PARAM @annotated
// THETA
  THETA1   : 400.337   : QT baseline
  THETA2   : 28.5654   : Emax M2
  THETA3   : 855.306   : EC50 M2
  THETA4   : 6.50411   : QTmax
  THETA5   : 6.44053   : Half_Life Time (wks)
  THETA6   : 2.75472   : AMPLI24
  THETA7   : 4.91289   : PEAK24
  THETA8   : 1.45861   : AMPLI12
  THETA9   : 4.49629   : PEAK12
  THETA10  : 11.833    : CLOFAZAMINE
  THETA11  : 2.47224   : MOXIFLOXACIN
  THETA12  : -8.74365  : CALCIUM
  THETA13  : -1.25313  : POTASSIUM
  THETA14  : 7.75303   : SEX
  THETA15  : -6.85734  : BLACK
  THETA16  : 0.349297  : AGE
  THETA17  : 4.10873   : boxcox IIV RUVMISP
  THETA18  : 0.824882  : boxcox IIV RUVREPL

$OMEGA @annotated @block
ETA1     : 0.00140223                 : IIV QT BASEL
ETA2     : -0.0192379 2.77451         : IIV QTmax
ETA3     : 0.00996292 0.774742 2.1854 : IIV EC50 M2
$OMEGA @annotated
ETA4     : 0.0450904 : IIV RUVMISP
ETA5     : 0.0572299 : IIV 

$SIGMA @annotated
SIGMA1   :  67.1173  : RUV ADD
$SIGMA @annotated @block
SIGMA2   :  47.1976  : RUV_REPL1
$SIGMA @annotated @block
SIGMA3   :  47.1976  : RUV_REPL2      // BLOCK(1) SAME
$SIGMA @annotated @block
SIGMA4   :  47.1976  : RUV_REPL3      // BLOCK(1) SAME

$MAIN
//=====  Covariates  =========
//------ Drug effect
  double TVEMM2   = THETA2 ;
  double TVEC50M2 = THETA3 ;
  
  double EMM2     = TVEMM2 ;
  double EC50M2   = TVEC50M2 * exp(ETA(3)) ;
  double EE       = EMM2*CONCM2/(EC50M2 + CONCM2) ;

//------ Time effect
  double TVQTMAX  = THETA4 ; 
  double HLT      = THETA5 ;
  
  double QTMAX    = TVQTMAX*(1 + ETA(2)) ; 
  double SLOPT    = log(2)/HLT ; 
  double TIMEFF   = QTMAX*(1-exp(-SLOPT*TIMW)) ; 

//------ Circadian rhythm
  double PI       = 3.1415926535 ; 
  double AMPLI24  = THETA6 ; 
  double PEAK24   = THETA7 ;
  double AMPLI12  = THETA8 ;
  double PEAK12   = THETA9 ;
  
  double DIURN24  = AMPLI24 * cos(2*PI*(CTIME - PEAK24)/24) ;
  double DIURN12  = AMPLI12 * cos(2*PI*(CTIME - PEAK12)/12) ;
  double CIRC     = DIURN24 + DIURN12 ;

//------ Effect of background TB drugs with QT liability
  double CLOFA2   = CLOFA ;
  double MOXI2    = MOXI ;
  
  double CLOFACOV = THETA10*CLOFA2 ;
  double MOXICOV  = THETA11*MOXI2 ;

//------ Effect of electrolyte levels
  double CACOR2   = CACOR ;
  double K2       = K ;

  double CACOV    = THETA12*(CACOR2 - 2.440) ;
  double KCOV     = THETA13*(K2-4.200) ;

//------ SEX
  double SEX2     = SEX ; // 0=male , 1=female

  double SEXCOV   = THETA14*SEX2 ; 
  
//------ RACE
  double BLACK    = 0 ;
  if(RACE == 2) BLACK = 1 ;

  double BLACKCOV = THETA15*BLACK ;
  
//------ AGE
  double AGE2     = AGE ;

  double AGECOV   = THETA16*(AGE2-33) ;


//======   Baseline QT  =========
  double TVBASEL  = THETA1 + CLOFACOV + MOXICOV + CACOV + KCOV + SEXCOV + BLACKCOV + AGECOV ;
  double BASEL    = TVBASEL *exp(ETA(1)) ;

$TABLE 
  capture IPRED    = BASEL + EE*ARM + TIMEFF + CIRC ; 

//------ Box-cox transformation of ηs on RUV
  // double ETAB3    = (pow(exp(ETA(4)), THETA17) - 1) / (THETA17) ;
  // double ETAB4    = (pow(exp(ETA(5)), THETA18) - 1) / (THETA18) ;
  
  // capture Y = IPRED + EPS(1)*exp(ETAB3)+ EPS(2)*exp(ETAB4) ;
  // if(REPL == 2) {Y = IPRED + EPS(1)*exp(ETAB3)+ EPS(3)*exp(ETAB4) ;}
  // if(REPL == 3) {Y = IPRED + EPS(1)*exp(ETAB3)+ EPS(4)*exp(ETAB4) ;}


  
$CAPTURE CLOFA MOXI CACOR K SEX RACE AGE IPRED regimen 
  "
