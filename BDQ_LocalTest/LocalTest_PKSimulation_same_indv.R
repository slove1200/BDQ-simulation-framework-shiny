##### library #######
library(mrgsolve)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)
library(ggplot2)
library(shiny)
library(grid)
library(ggpubr)
library(DT)
library(bslib)
library(dipsaus)
library(stringr)
library(bsicons)
library(ggh4x)
library(languageserver)
library(httpgd)

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
  DDIvalues <- list(
    "Efavirenz" = c(2.1, 2.1),
    "Lopinavir/r" = c(0.25, 0.59),
    "Nevirapine" = c(0.82, 1.19),
    "Rifampicin" = c(4.8, 4.8),
    "Rifapentine" = c(4.0, 4.0)
  )
  
  # Check if IEval is one of the base drugs
  if (IEval %in% names(DDIvalues)) {
    df$THETA25 <- DDIvalues[[IEval]][1]
    df$THETA26 <- DDIvalues[[IEval]][2]
  }
  
  return(df)
}

PKSimulation <- function(IIVval, mod, df, sim_time) {
  
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
      mrgsim(end = sim_time * 168, delta = 1) %>%
      as.data.frame() %>% filter(AMT == 0)
  )
}

Pop_generation <- function(input) {
  nsubjects <- input$nsim
  
  # Read in dataset
  myCovSimMICE <- read.csv("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/Virtual_population/TBPACTS/TBPACTS_Big_Virtual_Population_SimulatedforUse.csv", 
                           header = T)
  
  # Calculate exact numbers needed for each category
  n_rows_needed <- nsubjects 
  n_females <- round(n_rows_needed * (input$SEX_female/100))
  n_males <- n_rows_needed - n_females
  
  # Calculate exact numbers for race
  n_black <- round(n_rows_needed * (input$popRACE/100))
  n_nonblack <- n_rows_needed - n_black
  
  # Filter and sample females
  females_data <- myCovSimMICE %>%
    filter(SEX == 1) %>%
    slice_head(n = n_females)
  
  # Filter and sample males
  males_data <- myCovSimMICE %>%
    filter(SEX == 0) %>%
    slice_head(n = n_males)
  
  set.seed(3468)
  
  # Combine the datasets
  df_virtualPop <- bind_rows(females_data, males_data) %>%
    # Create sequential ID
    mutate(ID = row_number()) %>%
    # Randomly assign RACE
    mutate(
      RACE = sample(c(rep(1, n_black), rep(0, n_nonblack)))
    )
  
  return(df_virtualPop)
}

#### input ####
input <- c()
input$nsim    <- 500  # Number of simulated individuals
input$simtime <- 24   # Time of simulation imputed (transformed in hours during simulation)


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


input$LD2         <- TRUE
input$ldose_2     <- 200 # Loading dose amount (mg)
input$ldur_2      <- 8   # Loading dose duration (transformed in hours during simulation)
input$lunit_2     <- 2 # Loading dose unit: "1" day, "2" week
input$lfreq_2     <- "Once daily" # Loading dose unit: "1" day, "2" week
input$mdose_2     <- 100 # Maintenance dose amount (mg)
input$mdur_2      <- 16 # Maintenance dose duration (transformed in hours during simulation)
input$munit_2     <- 2 # Maintenance dose unit: "1" day, "2" week
input$mfreq_2     <- "Once daily" # Maintenance dose unit: "1" day, "2" week
input$IE_2_HIV    <- "None"
input$IE_2_TB     <- "None"

input$LD3         <- TRUE
input$ldose_3     <- 400 # Loading dose amount (mg)
input$ldur_3      <- 2   # Loading dose duration (transformed in hours during simulation)
input$lunit_3     <- 2 # Loading dose unit: "1" day, "2" week
input$lfreq_3     <- "Once daily" # Loading dose unit: "1" day, "2" week
input$mdose_3     <- 100 # Maintenance dose amount (mg)
input$mdur_3      <- 22 # Maintenance dose duration (transformed in hours during simulation)
input$munit_3     <- 2 # Maintenance dose unit: "1" day, "2" week
input$mfreq_3     <- "Once daily" # Maintenance dose unit: "1" day, "2" week
input$IE_3_HIV    <- "None"
input$IE_3_TB     <- "None"

input$IIV         <- "ON" # ON or OFF

input$RG1       <- T
input$RG2       <- T
input$RG3       <- T

## Common model covariates
input$population_radio <- "Population"
input$RACE             <- "Non-Black"
input$WT               <- 53
input$ALB              <- 3.5
input$AGE              <- 32
input$SEX              <- "Male"

input$SEX_female       <- 50
input$popRACE          <- 40


nsamples <- input$nsim    
sim_time <- input$simtime 

## Dosing details
# Common inputs shared across all regimens
regimens <- NULL

# Create a list to hold the selected regimens
num_regimens <- sum(c(TRUE, input$RG2, input$RG3))  # Regimen 1 is compulsory

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
                     IE_PK = input[[paste0("IE_", i, "_PK")]])
  
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
    if (!is.null(regimens[[i]]$IE_PK) ) {
      dfPK <- PKDDIProcessing(regimens[[i]]$IE_PK, dfPK)
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

#################### covariate value input ####

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
  
} else {  # UI input is to simulate in a population-level
  # Dynamically bind rows based on `num_regimens`
  virtual_population_df <- map_dfr(1:num_regimens, ~ {
    Pop_generation(input) %>%
      filter(ID %in% 1:nsamples) %>%
      mutate(regimen  = .x) # Optional: Add a column to indicate duplication
  }) %>% ungroup() %>%
    mutate(ID = row_number())
  
  dfPK_combined <- full_join(dfPK_combined, virtual_population_df, by = c("ID", "regimen"))
}

############# PK simulation  #####
# Load mrgsolve model
source("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_Server/BDQOMAT.R")
mod <- mcode("BDQOMAT", code)
mod <- update(mod, outvars = outvars(mod)$capture)

# Run simulation
# Filter and run simulation separately
out <- map_dfr(1:num_regimens, ~ {
  start_idx <- (.x - 1) * nsamples + 1
  end_idx <- .x * nsamples
  
  # Filter the dataset for the current regimen
  filtered_df <- dfPK_combined %>%
    filter(ID %in% start_idx:end_idx)
  
  # Set random seed and run the simulation for the current dataset
  set.seed(3468)
  PKSimulation(input$IIV, mod, filtered_df, sim_time)
  })

check <- out %>% group_by(ID) %>% slice(1L)

##### calculate cumulative exposures #####
d2 <- out %>% filter(AMT == 0)

dd2res <- d2 %>% filter(time %% 24 == 0)

dd2res$DAY <- dd2res$time / 24
dd2res$WEEK <- dd2res$time / 168

dd2day <- subset(dd2res, select = c(ID, time, DAY, WEEK, AAUCBDQ)) %>% filter(DAY == 13 | DAY == 14)
dd2day <- dd2day %>% group_by(ID) %>% mutate(AUCDBDQ = AAUCBDQ - lag(AAUCBDQ)) 

dd2wk8 <- subset(dd2res, select = c(ID, time, DAY, WEEK, AAUCBDQ)) %>% filter(WEEK == 7 | WEEK == 8)
dd2wk8 <- dd2wk8 %>% group_by(ID) %>% mutate(AUCWBDQ = AAUCBDQ - lag(AAUCBDQ)) 

dd2wk24 <- subset(dd2res, select = c(ID, time, DAY, WEEK, AAUCBDQ)) %>% filter(WEEK == 23 | WEEK == 24)
dd2wk24 <- dd2wk24 %>% group_by(ID) %>% mutate(AUCWBDQ = AAUCBDQ - lag(AAUCBDQ)) 

dd2res2 <- rbind(dd2day, dd2wk8, dd2wk24) %>% mutate(cpd = "Bedaquiline") %>% filter(is.na(AUCDBDQ) == F |
                                                                                       is.na(AUCWBDQ) == F)

dd2cum <- dd2res %>% filter(time == 168*2 | time == 168*8 | time == 168*24) %>% select(ID, time, DAY, AAUCBDQ, regimen)


dd2cum %>% group_by(regimen, DAY) %>% summarise(median = median(AAUCBDQ)) %>% arrange(DAY, regimen)

