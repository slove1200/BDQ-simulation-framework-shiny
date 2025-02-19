##### library #######
library(mrgsolve)
library(dplyr)
library(tidyr)
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

#### functions for virtual pop generation using MICE, conditional distribution ####
simCovMICE <- function(m = 5, 
                       orgCovs, 
                       catCovs = c("SEX","RACE"), 
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
  num_regimens <- sum(c(TRUE, input$RG2, input$RG3))  # Regimen 1 is compulsory
  
  # Read in dataset for conditional distribution modeling for covariates distribution
  orgCovsEx <- read.csv("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/Virtual_population/TBPACTS/TBPACTS_Virtual_Population.csv", 
                        header = T)
  # Differentiate categorical and continuous variables
  categorical_vars <- c("SEX", "RACE")  # Replace with your actual categorical variables
  continuous_vars <- c("AGE", "MTTP", "CACOR", "K", "WT", "ALB")  # Replace with your actual continuous variables
  
  # Conditional distribution modeling for covariates distribution
  # Numbers of subjects depending on input (numbers of simulated individuals)
  set.seed(3468)
  myCovSimMICE <- simCovMICE(m = 1, 
                             orgCovs = orgCovsEx,
                             catCovs = c("SEX", "RACE"),
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
input$RG2       <- F
input$RG3       <- F

## Common model covariates
input$population_radio <- "Population"
input$RACE             <- "Non-Black"
input$WT               <- 53
input$ALB              <- 3.5
input$AGE              <- 32
input$SEX              <- "Male"

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
  
} else { # UI input is to simulate in a population-level
  dfPK_combined <- full_join(dfPK_combined, Pop_generation(input), by = c("ID", "regimen"))
}

############# PK simulation  #####
# Load mrgsolve model
mod <- mcode("BDQOMAT", code)
mod <- update(mod, outvars = outvars(mod)$capture)

# Run simulation
set.seed(3468)
out <- PKSimulation(input$IIV, mod, dfPK_combined, sim_time)


# #### PK plots ####
# dfForPlotBDQ <- out %>%
#   ungroup() %>%
#   group_by(time, regimen) %>%
#   summarize(
#     lower = quantile(exp(IPRED)*1000, probs = 0.05),
#     median = quantile(exp(IPRED)*1000, probs = 0.5),
#     upper = quantile(exp(IPRED)*1000, probs = 0.95)
#   )
# 
# # Set dynamic ylim BDQ
# if (input$nsim == 1 || input$IIV == "OFF") { # individual
#   maxBDQ <- exp(max(out$IPRED))*1000
#   ylimitsBDQ <- maxBDQ
# } else {
#   q97BDQ <- quantile(exp(out$IPRED)*1000, probs = 0.97)
#   max15BDQ <- exp(max(out$IPRED))*1000*0.15
#   ylimitsBDQ <- q97BDQ + max15BDQ
# }
# 
# a1 <- ggplot(dfForPlotBDQ, aes(x = time / 168, y = median, 
#                                color = as.factor(regimen), 
#                                fill = as.factor(regimen))) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, colour = NA) +
#   geom_line(size = 1) +
#   theme_bw() +
#   labs(x = "Time (weeks)", y = c("BDQ concentration (ng/mL)")) +
#   ggtitle("BDQ Concentration (ng/mL) vs Time") +
#   coord_cartesian(ylim = c(0, ylimitsBDQ)) +
#   scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
#   scale_fill_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
#   theme(
#     plot.title = element_text(size = 18),       # Main title
#     axis.title = element_text(size = 16),       # Axis titles
#     axis.text = element_text(size = 14),        # Axis text
#     legend.title = element_text(size = 16),     # Legend title
#     legend.text = element_text(size = 14),      # Legend text
#     strip.text = element_text(size = 16)
#   ) + 
#   guides(color = guide_legend("Regimen"),
#          fill  = guide_legend("Regimen"))
# 
# dfForPlotM2 <- out %>%
#   ungroup() %>%
#   group_by(time, regimen) %>%
#   summarize(
#     lower = quantile(exp(IPREDM2)*1000, probs = 0.05),
#     median = quantile(exp(IPREDM2)*1000, probs = 0.5),
#     upper = quantile(exp(IPREDM2)*1000, probs = 0.95)
#   )
# 
# # Set dynamic ylim M2
# if (input$nsim == 1 || input$IIV == "OFF") { # individual
#   maxM2 <- exp(max(out$IPREDM2))*1000
#   ylimitsM2 <- maxM2
# } else {
#   q97M2 <- quantile(exp(out$IPREDM2)*1000, probs = 0.97)
#   max15M2 <- exp(max(out$IPREDM2))*1000*0.15
#   ylimitsM2 <- q97M2 + max15M2
# }
# 
# a2 <- ggplot(dfForPlotM2, aes(x = time / 168, y = median, 
#                               color = as.factor(regimen), 
#                               fill = as.factor(regimen))) +
#   geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.factor(regimen)), alpha = 0.3, colour = NA) +
#   geom_line(size = 1) +
#   theme_bw() +
#   labs(x = "Time (weeks)", y = c("M2 concentration (ng/mL)")) +
#   ggtitle("M2 Concentration (ng/mL) vs Time") +
#   coord_cartesian(ylim = c(0, ylimitsM2)) +
#   scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
#   scale_fill_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
#   theme(
#     plot.title = element_text(size = 18),       # Main title
#     axis.title = element_text(size = 16),       # Axis titles
#     axis.text = element_text(size = 14),        # Axis text
#     legend.title = element_text(size = 16),     # Legend title
#     legend.text = element_text(size = 14),      # Legend text
#     strip.text = element_text(size = 16)
#   ) + 
#   guides(color = guide_legend("Regimen"),
#          fill  = guide_legend("Regimen"))
# 
# plot <- ggarrange(a1, a2, ncol = 2, common.legend = TRUE, legend = "bottom")
# plot
# 
# 
# # daily average ####
# # Filter and add the DAY column
# Cavg_daily <- out %>% 
#   filter(time %% 24 == 0) %>%
#   mutate(DAY = time / 24)
# 
# # Subset relevant columns
# Cavg_daily <- Cavg_daily %>% select(ID, time, regimen, DAY, AAUCBDQ, AAUCM2)
# 
# # Calculate daily AUC using the `lag` function for vectorized operations
# Cavg_daily <- Cavg_daily %>%
#   mutate(
#     AUCDBDQ = AAUCBDQ - lag(AAUCBDQ, default = 0),
#     AUCDM2  = AAUCM2 - lag(AAUCM2, default = 0)
#   )
# 
# # Ensure AUC values are zero for `time == 0` as required
# Cavg_daily <- Cavg_daily %>%
#   mutate(
#     AUCDBDQ = ifelse(time == 0, 0, AUCDBDQ),
#     AUCDM2  = ifelse(time == 0, 0, AUCDM2)
#   )
# 
# ###### summarise by
# dfForPlot_CavgD <- Cavg_daily %>%
#   group_by(time, regimen, DAY) %>%
#   dplyr::summarise(
#     lower_CavgDBDQ = quantile(AUCDBDQ/24*1000, probs = 0.05),
#     median_CavgDBDQ = quantile(AUCDBDQ/24*1000, probs = 0.5),
#     upper_CavgDBDQ = quantile(AUCDBDQ/24*1000, probs = 0.95),
#     lower_CavgDM2 = quantile(AUCDM2/24*1000, probs = 0.05),
#     median_CavgDM2 = quantile(AUCDM2/24*1000, probs = 0.5),
#     upper_CavgDM2 = quantile(AUCDM2/24*1000, probs = 0.95),
#   )
# 
# # Set dynamic ylim BDQ
# if (input$nsim == 1 || input$IIV == "OFF") { # individual
#   maxBDQ <- max(Cavg_daily$AUCDBDQ)/24*1000
#   ylimitsBDQ <- maxBDQ
# } else {
#   q97BDQ <- quantile(Cavg_daily$AUCDBDQ/24*1000, probs = 0.97)
#   max15BDQ <- max(Cavg_daily$AUCDBDQ)/24*1000*0.15
#   ylimitsBDQ <- q97BDQ + max15BDQ
# }
# 
# a3 <- ggplot(dfForPlot_CavgD, aes(x = time / 168, y = median_CavgDBDQ, 
#                                   color = as.factor(regimen), 
#                                   fill = as.factor(regimen))) +
#   geom_ribbon(aes(ymin = lower_CavgDBDQ, ymax = upper_CavgDBDQ), alpha = 0.3, colour = NA) +
#   geom_line(size = 1) +
#   theme_bw() +
#   labs(x = "Time (weeks)", y = c("Daily Average BDQ concentration (ng/mL)")) +
#   ggtitle("Daily Average BDQ Concentration (ng/mL) vs Time") +
#   coord_cartesian(ylim = c(0, ylimitsBDQ)) +
#   scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
#   scale_fill_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
#   theme(
#     plot.title = element_text(size = 18),       # Main title
#     axis.title = element_text(size = 16),       # Axis titles
#     axis.text = element_text(size = 14),        # Axis text
#     legend.title = element_text(size = 16),     # Legend title
#     legend.text = element_text(size = 14),      # Legend text
#     strip.text = element_text(size = 16)
#   ) + 
#   guides(color = guide_legend("Regimen"),
#          fill  = guide_legend("Regimen"))
# 
# # Set dynamic ylim M2
# if (input$nsim == 1 || input$IIV == "OFF") { # individual
#   maxM2 <- max(Cavg_daily$AUCDM2)/24*1000
#   ylimitsM2 <- maxM2
# } else {
#   q97M2 <- quantile(Cavg_daily$AUCDM2/24*1000, probs = 0.97)
#   max15M2 <- max(Cavg_daily$AUCDM2)/24*1000*0.15
#   ylimitsM2 <- q97M2 + max15M2
# }
# 
# a4 <- ggplot(dfForPlot_CavgD, aes(x = time / 168, y = median_CavgDM2, 
#                                   color = as.factor(regimen), 
#                                   fill = as.factor(regimen))) +
#   geom_ribbon(aes(ymin = lower_CavgDM2, ymax = upper_CavgDM2, fill = as.factor(regimen)), alpha = 0.3, colour = NA) +
#   geom_line(size = 1) +
#   theme_bw() +
#   labs(x = "Time (weeks)", y = c("Daily Average M2 concentration (ng/mL)")) +
#   ggtitle("Daily Average M2 Concentration (ng/mL) vs Time") +
#   coord_cartesian(ylim = c(0, ylimitsM2)) +
#   scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
#   scale_fill_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
#   theme(
#     plot.title = element_text(size = 18),       # Main title
#     axis.title = element_text(size = 16),       # Axis titles
#     axis.text = element_text(size = 14),        # Axis text
#     legend.title = element_text(size = 16),     # Legend title
#     legend.text = element_text(size = 14),      # Legend text
#     strip.text = element_text(size = 16)
#   ) + 
#   guides(color = guide_legend("Regimen"),
#          fill  = guide_legend("Regimen"))
# 
# plot2 <- ggarrange(a3, a4, ncol = 2, common.legend = TRUE, legend = "bottom")
# plot2
# 
# 
# # Weekly average concentration
# # Filter and add the WEEK column
# Cavg_weekly <- out %>%
#   filter(time %% 168 == 0) %>%
#   mutate(WEEK = time / 168)
# 
# # Subset relevant columns
# Cavg_weekly <- Cavg_weekly %>% select(ID, time, regimen, WEEK, AAUCBDQ, AAUCM2)
# 
# # Calculate weekly AUC
# Cavg_weekly$AUCWBDQ <- 0
# Cavg_weekly$AUCWM2 <- 0
# 
# # Calculate daily AUC using the `lag` function for vectorized operations
# Cavg_weekly <- Cavg_weekly %>%
#   mutate(
#     AUCWBDQ = AAUCBDQ - lag(AAUCBDQ, default = 0),
#     AUCWM2  = AAUCM2 - lag(AAUCM2, default = 0)
#   )
# 
# # Ensure AUC values are zero for `time == 0` as required
# Cavg_weekly <- Cavg_weekly %>%
#   mutate(
#     AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ),
#     AUCWM2  = ifelse(time == 0, 0, AUCWM2)
#   )
# 
# 
# ###### summarise by
# dfForPlot_CavgW <- Cavg_weekly %>%
#   group_by(time, regimen, WEEK) %>%
#   dplyr::summarise(
#     lower_CavgWBDQ = quantile(AUCWBDQ/168*1000, probs = 0.05),
#     median_CavgWBDQ = quantile(AUCWBDQ/168*1000, probs = 0.5),
#     upper_CavgWBDQ = quantile(AUCWBDQ/168*1000, probs = 0.95),
#     lower_CavgWM2 = quantile(AUCWM2/168*1000, probs = 0.05),
#     median_CavgWM2 = quantile(AUCWM2/168*1000, probs = 0.5),
#     upper_CavgWM2 = quantile(AUCWM2/168*1000, probs = 0.95),
#   )
# 
# # Set dynamic ylim BDQ
# if (input$nsim == 1 || input$IIV == "OFF") { # individual
#   maxBDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000
#   ylimitsBDQ <- maxBDQ
# } else {
#   q97BDQ <- quantile(Cavg_weekly$AUCWBDQ/168*1000, probs = 0.97)
#   max15BDQ <- max(Cavg_weekly$AUCWBDQ)/168*1000*0.15
#   ylimitsBDQ <- q97BDQ + max15BDQ
# }
# 
# a5 <- ggplot(dfForPlot_CavgW, aes(x = time / 168, y = median_CavgWBDQ, 
#                                   color = as.factor(regimen), 
#                                   fill = as.factor(regimen))) +
#   geom_ribbon(aes(ymin = lower_CavgWBDQ, ymax = upper_CavgWBDQ), alpha = 0.3, colour = NA) +
#   geom_line(size = 1) +
#   theme_bw() +
#   labs(x = "Time (weeks)", y = c("Weekly Average BDQ concentration (ng/mL)")) +
#   ggtitle("Weekly Average BDQ Concentration (ng/mL) vs Time") +
#   coord_cartesian(ylim = c(0, ylimitsBDQ)) +
#   scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
#   scale_fill_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
#   theme(
#     plot.title = element_text(size = 18),       # Main title
#     axis.title = element_text(size = 16),       # Axis titles
#     axis.text = element_text(size = 14),        # Axis text
#     legend.title = element_text(size = 16),     # Legend title
#     legend.text = element_text(size = 14),      # Legend text
#     strip.text = element_text(size = 16)
#   ) + 
#   guides(color = guide_legend("Regimen"),
#          fill  = guide_legend("Regimen"))
# 
# # Set dynamic ylim M2
# if (input$nsim == 1 || input$IIV == "OFF") { # individual
#   maxM2 <- max(Cavg_weekly$AUCWM2)/168*1000
#   ylimitsM2 <- maxM2
# } else {
#   q97M2 <- quantile(Cavg_weekly$AUCWM2/168*1000, probs = 0.97)
#   max15M2 <- max(Cavg_weekly$AUCWM2)/168*1000*0.15
#   ylimitsM2 <- q97M2 + max15M2
# }
# 
# a6 <- ggplot(dfForPlot_CavgW, aes(x = time / 168, y = median_CavgWM2, 
#                                   color = as.factor(regimen), 
#                                   fill = as.factor(regimen))) +
#   geom_ribbon(aes(ymin = lower_CavgWM2, ymax = upper_CavgWM2, fill = as.factor(regimen)), alpha = 0.3, colour = NA) +
#   geom_line(size = 1) +
#   theme_bw() +
#   labs(x = "Time (weeks)", y = c("Weekly Average M2 concentration (ng/mL)")) +
#   ggtitle("Weekly Average M2 Concentration (ng/mL) vs Time") +
#   coord_cartesian(ylim = c(0, ylimitsM2)) +
#   scale_color_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
#   scale_fill_manual(values = c("#A084B5", "#D65D61", "#44BE5F")) +
#   theme(
#     plot.title = element_text(size = 18),       # Main title
#     axis.title = element_text(size = 16),       # Axis titles
#     axis.text = element_text(size = 14),        # Axis text
#     legend.title = element_text(size = 16),     # Legend title
#     legend.text = element_text(size = 14),      # Legend text
#     strip.text = element_text(size = 16)
#   ) + 
#   guides(color = guide_legend("Regimen"),
#          fill  = guide_legend("Regimen"))
# 
# plot3 <- ggarrange(a5, a6, ncol = 2, common.legend = TRUE, legend = "bottom")
# plot3
