#### Function ####
convertTimeUnit <- function(val) {
  if (val == "1") {                          # if input unit is "day"
    return(24)
  } else {                                   # else: "week"
    return(168)
  }
}

defineEventVariable <- function(dur, unit, val) {
  if (val == "Twice daily") {
    ii <<- 12
    dosingtime <<- 0
    addl <<- (dur * unit / 24) * 2 - 1
  } else if (val == "Once daily") {
    ii <<- 24
    dosingtime <<- 0
    addl <<- (dur * unit / 24) * 1 - 1
  } else if (val == "Three times weekly") {
    ii <<- 168
    dosingtime <<- c(0, 48, 96)
    addl <<- dur * unit / 168 - 1
  } else {
    ii <<- 168
    dosingtime <<- 0
    addl <<- dur * unit / 168 - 1
  }
}

createEventDataset <- function(nsamples, dose, timeModifier) {
  return(as.data.frame(ev(
    ID = 1:nsamples,
    ii = ii,
    cmt = 1,
    amt = dose,
    addl = addl,
    time = dosingtime + timeModifier)))
}

# Function to handle dosing details (both loading and maintenance)
processDosing <- function(load_dose, ldose, ldur, lunit, lfreq, mdose, mdur, munit, mfreq, nsamples) {
  # Handle Loading Dose if enabled
  if (load_dose) {
    lunit <- convertTimeUnit(lunit)
    loading_interval <- defineEventVariable(ldur, lunit, lfreq)
    dfLoad <- createEventDataset(nsamples, ldose, 0)
  }
  
  # Handle Maintenance Dose
  munit <- convertTimeUnit(munit)
  maintenance_interval <- defineEventVariable(mdur, munit, mfreq)
  
  if (load_dose) {
    dfMaintenance <- createEventDataset(nsamples, mdose, ldur * lunit)
    dfPK <- rbind(dfLoad, dfMaintenance)
  } else {
    dfMaintenance <- createEventDataset(nsamples, mdose, 0)
    dfPK <- dfMaintenance
  }
  
  dfPK$THETA25 <- 1    # IE BDQ
  dfPK$THETA26 <- 1    # IE M2
  
  return(dfPK)
}

PKDDIProcessing <- function(IEval, df) {
  # Define base values for Efavirenz, Lopinavir/r, Nevirapine
  base_values <- list(
    "Efavirenz" = c(2.1, 2.1),
    "Lopinavir/r" = c(0.25, 0.59),
    "Nevirapine" = c(0.82, 1.19)
  )
  
  # Define override values for Rifampicin and Rifapentine
  override_values <- list(
    "Rifampicin" = c(4.8, 4.8),
    "Rifapentine" = c(4.0, 4.0)
  )
  
  # Check if IEval is one of the base drugs
  if (IEval %in% names(base_values)) {
    df$THETA25 <- base_values[[IEval]][1]
    df$THETA26 <- base_values[[IEval]][2]
  }
  
  # Check if IEval is one of the override drugs
  if (IEval %in% names(override_values)) {
    df$THETA25 <- override_values[[IEval]][1]
    df$THETA26 <- override_values[[IEval]][2]
  }
  
  return(df)
}

PKSimulation <- function(IIVval, mod, df, sim_time, sunit) {
  
  ## IIV "ON"/"OFF"
  if(IIVval == "OFF") {
    mod <- zero_re(mod)        # zero omega and sigma
  }
  else {
    mod <- zero_re(mod, sigma) # zero sigma, keep omega
  }
  
  ## Return simulated PK dataset
  return(
    mod %>%
      data_set(df) %>%
      mrgsim(end = sim_time * sunit, delta = 1) %>%
      as.data.frame() %>% filter(AMT == 0)
  )
}

#### functions for virtual pop generation using MICE, conditional distribution ####
simCovMICE <- function(m = 5, 
                       orgCovs, 
                       catCovs = c("SEX","RACE","TBTYPE"), 
                       seedCovs = NULL,
                       targetRangeSeedCovs = NULL,
                       seedCovsValues = NULL,
                       nsubj = nrow(orgCovs),
                       contMeth = "pmm",
                       sampleFromReal = T) {
  
  # names of continuous covariates
  contCovs <- setdiff(names(orgCovs),catCovs)
  
  # create copy of the original data set with factor version of categorical covariates
  orgCovsF <- orgCovs %>% dplyr::mutate_at(catCovs,function(x) as.factor(x))
  
  ## find covariates with missing values
  missVars <- names(orgCovs)[colSums(is.na(orgCovs)) > 0]
  
  # impute missing data once with mice  
  if(length(missVars)>0)
  {
    imp1 <- mice::mice(orgCovsF, m=1, printFlag=FALSE, maxit = 15)
    orgCovs <- mice::complete(imp1)
  }
  
  miCovs <- orgCovs[1:nsubj,] %>% mutate_all(function(x) NA)
  
  
  if(!is.null(seedCovs)) 
  {
    if(sampleFromReal)
    {
      ## create vector of seedCov values contained in original data set
      poolSeed <-  orgCovs[orgCovs[,seedCovs]>=min(targetRangeSeedCovs) & orgCovs[,seedCovs]<=max(targetRangeSeedCovs),seedCovs]
      ## do the actual sampling
      miCovs[seedCovs] <- sample(poolSeed,nsubj,replace = T)
    }
    else
      miCovs[seedCovs] <-  seedCovsValues 
  }
  
  combCovs <- orgCovs %>% mutate(Type="Original") %>% 
    bind_rows(miCovs %>% mutate(Type="Simulated")) %>%
    mutate_at(catCovs,function(x) as.factor(x))
  
  myPredMat <- mice::make.predictorMatrix(combCovs)
  myPredMat[,c("Type")] <- 0
  
  myMethods <- mice::make.method(combCovs)
  myMethods[contCovs] <- contMeth
  
  imp2 <-mice::mice(combCovs, m=m,printFlag=FALSE,predictorMatrix = myPredMat, method = myMethods)
  
  impCovs <- mice::complete(imp2,action="long") %>% 
    filter(Type=="Simulated") %>% 
    mutate(NSIM=.imp) %>% 
    select(everything(),-.id,-Type,-.imp)
  
  return(impCovs)
  
}


Pop_generation <- function(input) {
  nsubjects <- input$nsim
  num_regimens <- sum(c(TRUE, input$RG2, input$RG3, input$RG4))  # Regimen 1 is compulsory
  
  # Read in dataset for conditional distribution modeling for covariates distribution
  orgCovsEx <- read.csv("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/Virtual_population/Simulated_population_for_BDQ_virtural_population_WTALB.csv", 
                        header = T)
  # Differentiate categorical and continuous variables
  categorical_vars <- c("SEX", "RACE", "TBTYPE")  # Replace with your actual categorical variables
  continuous_vars <- c("AGE", "MTTP", "CACOR", "K", "WT", "ALB")  # Replace with your actual continuous variables
  
  # Conditional distribution modeling for covariates distribution
  # Numbers of subjects depending on input (numbers of simulated individuals)
  set.seed(3468)
  myCovSimMICE <- simCovMICE(m = 1, 
                             orgCovs = orgCovsEx,
                             catCovs = c("SEX", "RACE", "TBTYPE"),
                             nsubj = nsubjects*num_regimens)
  
  myCovSimMICE <- myCovSimMICE %>% ungroup() %>% 
    mutate(regimen = rep(1:num_regimens, each = nsubjects)) %>%
    # correspondent to unique ID defined in Server_PK
    group_by(regimen) %>%
    mutate(ID = row_number()) %>%
    mutate(ID = ifelse(regimen == 1, ID, ID+nsubjects*(regimen-1))) %>% 
    select(ID, everything(), -NSIM) %>%
    mutate_if(is.factor, ~ as.numeric(as.character(.)))
  
  return(myCovSimMICE)
}

#### input ####
input <- c()
input$nsim    <- 10  # Number of simulated individuals
input$simtime <- 24   # Time of simulation imputed (transformed in hours during simulation)
input$sunit   <- 168  ## Simulation unit: "1" day, "2" week


#### PK ####
input$LD1         <- TRUE
input$ldose_1     <- 400 # Loading dose amount (mg)
input$ldur_1      <- 2   # Loading dose duration (transformed in hours during simulation)
input$lunit_1     <- 2 # Loading dose unit: "1" day, "2" week
input$lfreq_1     <- "Once daily" # Loading dose unit: "1" day, "2" week
input$mdose_1     <- 200 # Maintenance dose amount (mg)
input$mdur_1      <- 22 # Maintenance dose duration (transformed in hours during simulation)
input$munit_1     <- 2 # Maintenance dose unit: "1" day, "2" week
input$mfreq_1     <- "Three times weekly" # Maintenance dose unit: "1" day, "2" week
input$IE_1_HIV    <- "None"
input$IE_1_TB     <- "None"

input$LD2         <- FALSE
input$ldose_2     <- 400 # Loading dose amount (mg)
input$ldur_2      <- 2   # Loading dose duration (transformed in hours during simulation)
input$lunit_2     <- 2 # Loading dose unit: "1" day, "2" week
input$lfreq_2     <- "Once daily" # Loading dose unit: "1" day, "2" week
input$mdose_2     <- 200 # Maintenance dose amount (mg)
input$mdur_2      <- 22 # Maintenance dose duration (transformed in hours during simulation)
input$munit_2     <- 2 # Maintenance dose unit: "1" day, "2" week
input$mfreq_2     <- "Three times weekly" # Maintenance dose unit: "1" day, "2" week
input$IE_2_HIV    <- "None"
input$IE_2_TB     <- "None"

input$LD3         <- FALSE
input$ldose_3     <- 400 # Loading dose amount (mg)
input$ldur_3      <- 2   # Loading dose duration (transformed in hours during simulation)
input$lunit_3     <- 2 # Loading dose unit: "1" day, "2" week
input$lfreq_3     <- "Once daily" # Loading dose unit: "1" day, "2" week
input$mdose_3     <- 200 # Maintenance dose amount (mg)
input$mdur_3      <- 22 # Maintenance dose duration (transformed in hours during simulation)
input$munit_3     <- 2 # Maintenance dose unit: "1" day, "2" week
input$mfreq_3     <- "Three times weekly" # Maintenance dose unit: "1" day, "2" week
input$IE_3_HIV    <- "None"
input$IE_3_TB     <- "None"

input$IIV         <- "ON" # ON or OFF

input$RG1       <- T
input$RG2       <- F
input$RG3       <- F
input$RG4       <- F

## Common model covariates
input$population_radio <- "Individual"
input$RACE             <- "Non-Black"
input$WT               <- 60
input$ALB              <- 3.1
input$AGE              <- 45
input$SEX              <- "Male"

nsamples <- input$nsim    
sim_time <- input$simtime 
sunit <- convertTimeUnit(input$sunit) 

## Dosing details
# Common inputs shared across all regimens
regimens <- NULL

# Create a list to hold the selected regimens
num_regimens <- sum(c(TRUE, input$RG2, input$RG3, input$RG4))  # Regimen 1 is compulsory

# Loop over the selected regimens
for (i in 1:num_regimens) {
  
  # Dynamically access input fields for each regimen
  common_inputs <- list(
    LD    = input[[paste0("LD", i)]],
    ldose = input[[paste0("ldose_", i)]],
    ldur  = input[[paste0("ldur_", i)]],
    lunit = input[[paste0("lunit_", i)]],
    lfreq = input[[paste0("lfreq_", i)]],
    mdose = input[[paste0("mdose_", i)]],
    mdur  = input[[paste0("mdur_", i)]],
    munit = input[[paste0("munit_", i)]],
    mfreq = input[[paste0("mfreq_", i)]]
  )
  
  # Now you can use 'common_inputs' for processing each regimen
  # For example:
  regimens[[i]] <- c(list(selected = input[[paste0("RG", i)]]), 
                     common_inputs, 
                     IE_HIV = input[[paste0("IE_", i, "_HIV")]], 
                     IE_TB = input[[paste0("IE_", i, "_TB")]])
  
  # Process the rest of the regimen as needed...
}

# Initialize an empty list to hold datasets from each regimen
all_regimens_df <- list()

# Loop over each regimen and process if selected
for (i in 1:num_regimens) {
  # 1. Dosing details
  if (i == 1 || regimens[[i]]$selected) {
    dfPK <- processDosing(
      regimens[[i]]$LD, 
      regimens[[i]]$ldose, 
      regimens[[i]]$ldur, 
      regimens[[i]]$lunit, 
      regimens[[i]]$lfreq,
      regimens[[i]]$mdose, 
      regimens[[i]]$mdur, 
      regimens[[i]]$munit, 
      regimens[[i]]$mfreq, 
      nsamples
    )
    dfPK$regimen <- i
    dfPK$ID <- dfPK$ID+nsamples*(i-1)  # Unique ID for each regimen
    
    # 2. PK DDI details
    if (!is.null(regimens[[i]]$IE_HIV) ) {
      dfPK <- PKDDIProcessing(regimens[[i]]$IE_HIV, dfPK)
    }
    
    if (!is.null(regimens[[i]]$IE_TB) ) {
      dfPK <- PKDDIProcessing(regimens[[i]]$IE_TB, dfPK)
    }
    
    all_regimens_df[[i]] <- dfPK
    
  } else {
    # If the regimen is not selected, ensure that it does not exist in the final dataset
    all_regimens_df[[i]] <- NULL
  }
}

# Merge all regimens into a single dataframe
dfPK_combined <- do.call(rbind, all_regimens_df)
dfPK_combined <- dfPK_combined[order(dfPK_combined$ID, dfPK_combined$time), ]

####################
#### covariate value input ####

# if UI input is to simulate in an individual-level
if (input$population_radio == "Individual") {
  
  # 1 "RACE"
  if (input$RACE == "Non-Black") {
    RACE <- 0
  } else {
    RACE <- 1   # RACE 0: "Non-Black", 1: "Black"
  }
  
  # 2 "WT"
  WT <- input$WT
  
  # 3 "ALB"
  ALB <- input$ALB
  
  # 4 "AGE"
  AGE <- input$AGE
  
  # 5 "SEX"
  if (input$SEX == "Male") {
    SEX <- 0
  } else {
    SEX <- 1   # SEX 0: "Male", 1: "Female"
  }
  
  ## Set parameters in dataset
  dfPK_combined$AGE    <- AGE
  dfPK_combined$RACE   <- RACE
  dfPK_combined$WT     <- WT
  dfPK_combined$ALB    <- ALB
  dfPK_combined$SEX    <- SEX
  
} else { # UI input is to simulate in a population-level
  dfPK_combined <- full_join(dfPK_combined, Pop_generation(input), by = c("ID", "regimen"))
}

# ##############################################
### PK simulation
# Load mrgsolve model
mod <- mcode("BDQOMAT", code)
mod <- update(mod, outvars = outvars(mod)$capture)

# Run simulation
set.seed(3468)
out <- PKSimulation(input$IIV, mod, dfPK_combined, sim_time, sunit)


#### PK output - BDQ weekly avg concentration ####
d2 <- out %>% filter(AMT == 0) %>% filter(time %% 168 == 0)
d2$WEEK <- d2$time / 168
d2 <- subset(d2, select = c(ID, time, WEEK, ALB, WT, RACE, AGE, REGIMEN, AAUCBDQ))

d2$AUCWBDQ <- 0
i <- 2
while (i <= length(d2$ID)) {
  d2$AUCWBDQ[i] <- d2$AAUCBDQ[i] - d2$AAUCBDQ[i - 1]
  i <- i + 1
}

d2 <- d2 %>% mutate(AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ))

dd2 <- d2 %>%
  group_by(ID, time, REGIMEN, WEEK) %>%
  summarise(weekly_BDQ = mean(AUCWBDQ, na.rm = T)) %>%
  mutate(CAVG = weekly_BDQ/168) # unit µg/mL

#### PK output - M2 concentration ####
dd3 <- out %>% filter(AMT == 0) %>% 
  mutate(CONCM2 = ifelse(time == 0, 0, exp(IPREDM2)*1000)) %>%
  select(ID, time, ALB, WT, RACE, AGE, CONCM2, REGIMEN)# unit ng/mL 

#### TTP ####
TTPdf   <- tidyr::crossing(
  ID    = c(1:input$nsim), 
  WEEKP = c(1:24), 
  REP   = c(1:1), 
  EVID  = 0, 
  AMT   = 0,
  FLAG  = 1, 
  TTPD  = c(0, 1:42), 
  LASTR = 0) %>%
  mutate(TIME = seq_along(TTPD) - 1) # dummy time column

TTPdf2 <- TTPdf %>% filter(TTPD == 0) %>% mutate(FLAG = 2, TIME = NA)

TTPdf_fin <- rbind(TTPdf, TTPdf2) %>%
  mutate(EVID  = ifelse(TTPD == 0 & FLAG == 1,  4, EVID), 
         AMT   = ifelse(TTPD == 0 & FLAG == 1,  1, AMT), 
         LASTR = ifelse(TTPD == 42, 1, LASTR), 
         TASTW = WEEKP) %>%
  mutate(CMT = ifelse(AMT == 1, 1, NA)) %>%
  arrange(ID, WEEKP, REP, TTPD) %>%
  zoo::na.locf()

# covariate distribution sampling
###### Sampling of pre-XDR+XDR
unique.TTPdf_fin <- unique(TTPdf_fin$ID)
sample.TTPdf_fin <- sample(unique.TTPdf_fin, 0.3 * length(unique.TTPdf_fin))
TTPdf_fin <- TTPdf_fin %>%
  dplyr::mutate(XDR = ifelse(ID %in% sample.TTPdf_fin, 1, 0))

# MTTP SAMPLING
set.seed(100)
TTPdf_fin <- TTPdf_fin %>%
  group_by(ID) %>%
  dplyr::mutate(MTTP = runif(1, 55.2, 1008))

dfCAVG <- dd2 %>% rename("WEEKP" ="WEEK") %>% 
  filter(WEEKP != 0) %>% 
  ungroup() %>% select(ID, WEEKP, CAVG)    

dfTTP <- TTPdf_fin %>% full_join(dfCAVG)

modTTP <- mcode("BDQTTP", codeTTP)
modTTP <- update(modTTP, outvars = outvars(modTTP)$capture)

##### Simulation with interindividual variability ON/OFF
if (input$IIV == "OFF") {
  set.seed(3468)
  outTTP <- modTTP %>%
    zero_re() %>%
    data_set(dfTTP) %>%
    mrgsim(end = sim_time * sunit, delta = 1) %>%
    as.data.frame()
  outTTP$REGIMEN <- "1"
} else { #### no sigma in TTP
  set.seed(3468)
  outTTP <- modTTP %>%
    data_set(dfTTP) %>%
    mrgsim(end = sim_time * sunit, delta = 1) %>%
    as.data.frame()
  outTTP$REGIMEN <- "1"
}

#### TTP Plot
# --- Get the proportion of samples without positive signal ---
outTTE <- outTTP %>% filter(RTTE == 1) ## positive signal at specific time
TTEcount <- outTTE %>% group_by(WEEKP) %>% count(TTPD)
ttpd <- crossing(TTPD = c(1:42), WEEKP = c(1:24))
TTEcount <- merge(ttpd, TTEcount, all.x = TRUE) %>% mutate(n = ifelse(is.na(n), 0, n))
TTEcount <- TTEcount[order(TTEcount$WEEKP, TTEcount$TTPD), ]

TTEcount <- TTEcount %>% group_by(WEEKP) %>% mutate(cumn = cumsum(n), proportion = NA)
TTEcount <- TTEcount %>% group_by(WEEKP) %>% mutate(proportion = ifelse(TTPD != 42, (1 - (cumn/sum(n))), lag(proportion)))
TTEcount <- TTEcount %>% filter(TTPD != 42)

# --- plot for WEEKP specified in the article ---
TTEcountw <- TTEcount %>% 
  filter(WEEKP %in% c(1:8, 10, 12, 14, 16, 18, 20))

# Custom labeller function to change facet titles
week_labels <- as_labeller(function(week) {
  paste("Week", week)
})

ggplot(TTEcountw, aes(TTPD, proportion*100)) +
  facet_wrap(~WEEKP, labeller = week_labels, ncol = 3, scales = "free_x") +
  geom_line(size = 1) +
  theme_bw() +
  xlab("Time in MGIT after inoculation (days)") +
  ylab("Proportion of samples without positive signal (%)") +
  ggtitle("Simulated TTP in MGIT per week after start of treatment") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20))

# --- plot for logit model (prob of bacterial presence) ---
PROBcount <- outTTP %>% filter(FLAG == 2 & NEG == 1) %>% group_by(WEEKP) %>% 
  summarise(prop = 1-(n()/nrow(outTTP %>% filter(FLAG == 2 & WEEKP == 1))))
## proportion = num of positive sample/total samples in each WEEKP

# Custom labeller function to change facet titles
TRT_labels <- as_labeller(function(TREATMENT) {
  "Bedaquiline arm"
})

ggplot(PROBcount, aes(WEEKP, prop*100)) +
  geom_line(size = 0.7) +
  geom_point(size = 2, shape = 1) +
  xlab("Time after start of treatment (weeks)") +
  ylab("Proportion of positive samples (%)") +
  ggtitle("Logistic model describing the probability of positive sample") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20.5))



######## MSM ####
#### create simulation dataset
HLMBL <- outTTP %>% filter(REP == 1 & WEEKP %in% c(1,2,sim_time) & FLAG == 2)

# Create a copy of the rows where WEEKP = 1
new_rows <- HLMBL %>% group_by(ID) %>%
  filter(WEEKP == 1) %>% slice(1L) %>%
  mutate(WEEKP = 0)  # Change WEEKP to 0

new_rows2 <- HLMBL %>% group_by(ID) %>%
  filter(WEEKP == 1) %>% slice(1L) %>%
  mutate(WEEKP = 3)  # Change WEEKP to 3


# Bind the new rows to the original dataframe
HLMBL2 <- bind_rows(HLMBL, new_rows, new_rows2) %>% arrange(ID, WEEKP)

TTPcov <- HLMBL2 %>% group_by(ID) %>% 
  mutate(HL2 = ifelse(WEEKP == 0, 0.69443, lag(HL)), 
         MBLend = MBL[WEEKP == sim_time]) %>% 
  mutate(HL2 = ifelse(WEEKP == 1, 0.69443, HL2), # median of HL 
         time = WEEKP*168)  %>% # hours
  select(ID, MTTP, XDR, time, HL2, MBLend)


####
# Set up event
ev0 <- ev(time = 0, amt = 1, cmt = 1, ID = seq(input$nsim))
ev1 <- ev(time = 0, amt = c(0,1,1,1,1,1), cmt = c(0,1,2,3,4,5), evid = c(2,4,1,1,1,1), ID = seq(input$nsim),
          addl = 120, ii = 168, realize = T)
data.dose <- seq(ev0, ev1)
data.dose <- data.table::setDT(as.data.frame(data.dose)) %>% arrange(ID, time) %>%
  mutate(evid = ifelse(time != 0 & cmt == 1, 4, ifelse(evid == 2, 0, evid))) %>%
  select(-ii, -addl)


# Covariates
# MTTP (same with TTP), XDR (in TTP is MDR and pre-XDR/XDR, in MSM is non-XDR and XDR), 
# HL2, MBLend, SEX (same with QT), baseWT (from PK)
PKcov <- out %>% group_by(ID) %>% slice(1L) %>% select(ID, WT, AGE, RACE)

idata <- data.table::data.table(ID=1:input$nsim) %>% left_join(PKcov)
data.all <- merge(data.dose, idata, by = "ID") %>% left_join(TTPcov) %>%
  group_by(ID) %>% zoo::na.locf()

dftentimes <- map_df(1:10, ~ {
  data.all %>%
    mutate(ID = ID + input$nsim * (.x - 1))  # Adjust ID for each copy
})

# MSM simulation
modMSM <- mcode("BDQMSM", BDQMSM)
modMSM <- update(modMSM, outvars = outvars(modMSM)$capture)

set.seed(3468)
outMSM <- modMSM %>%
  data_set(dftentimes) %>%
  mrgsim(end = 672, delta = 168) %>%
  as.data.frame %>%
  filter(EVID == 0) %>% 
  mutate(time = time/24/7) %>%
  rename("STATE" = "XDV") %>%
  select(-EVID, -P_4)

outMSM2 <- outMSM2 %>% mutate(ID = ID+10)

#### Plot
outMSM <- rbind(outMSM, outMSM2)

summary_MSM <- outMSM %>% group_by(regimen, time, STATE) %>% summarise(prop = n()/input$nsim, .groups = "drop") %>%
  # Ensure all combinations of time and STATE are included, even if they have no observations
  complete(regimen, time, STATE, fill = list(prop = 0))


# Custom labeller function to change facet titles
state_labels <- as_labeller(function(STATE) {
  case_when(STATE == 1 ~ "Active TB", 
            STATE == 2 ~ "Converted", 
            STATE == 3 ~ "Recurrent TB", 
            STATE == 5 ~ "Death")
})

ggplot(summary_MSM %>% filter(time %in% c(0,4,8,16,24,48,72,96,120)), aes(x = time, y = prop*100)) +
  geom_line(size = 0.8) +
  geom_point(size = 2, shape = 1) +
  facet_wrap(~STATE, labeller = state_labels, scales = "free_x") +
  theme_bw() +
  xlab("Time after start of treatment (weeks)") +
  ylab("Proportion of patients (%)") +
  ggtitle("Proportions of patients being in each state") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(breaks = seq(0, 120, by = 12))



## different numbers of replicates
test <- rbind(summary_MSM5 %>% mutate(REP = 1), 
              summary_MSM6 %>% mutate(REP = 10), 
              summary_MSM7 %>% mutate(REP = 50), 
              summary_MSM8 %>% mutate(REP = 100))


ggplot(test %>% filter(time %in% c(0,4,8,16,24,48,72,96,120)), aes(x = time, y = prop*100, 
                                                                   color = as.factor(REP), group = as.factor(REP))) +
  geom_line(size = 1) +
  geom_point(size = 2, shape = 1) +
  facet_wrap(~STATE, labeller = state_labels, scales = "free_y") +
  theme_bw() +
  xlab("Time after start of treatment (weeks)") +
  ylab("Proportion of patients (%)") +
  ggtitle("Proportions of patients being in each state") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 120, by = 12)) +
  scale_color_manual(values = c("#D73027", "#4575B4", "#F46D43", "#4DAF4A")) +
  labs(color = "Replicates") +
  theme(
    plot.title = element_text(size = 14),       # Main title
    axis.title = element_text(size = 14),       # Axis titles
    axis.text = element_text(size = 12),        # Axis text
    legend.title = element_text(size = 12),     # Legend title
    legend.text = element_text(size = 11),      # Legend text
    strip.text = element_text(size = 14)
  )

