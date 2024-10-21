#### Define function used in server ####
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
  if (IEval == "Efavirenz") {
    df$THETA25 <- 2.1
    df$THETA26 <- 2.1
  }
  else if (IEval %in% c("Lopinavir/r", "Rifapentine")) {
    df$THETA25 <- 4.0
    df$THETA26 <- 4.0
  }
  else if (IEval == "Nevirapine") {
    df$THETA25 <- 0.95
    df$THETA26 <- 1.58
  }
  else if (IEval == "Rifampicin") {
    df$THETA25 <- 4.8
    df$THETA26 <- 4.8
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

## define sim_PK() ####
sim_PK <- function(input) {
  ## Simulation settings
  # 1. "nsim"
  nsamples <- input$nsim      # Number of simulated individuals
  
  # 2. "simtime" and "simunit"
  sim_time <- input$simtime   # Time of simulation imputed (transformed in hours during simulation)
  sunit <- convertTimeUnit(input$sunit)   # Simulation unit: "1" day, "2" week
  
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
                       IE = input[[paste0("IE_", i)]])
    
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
      if (!is.null(regimens[[i]]$IE)) {
        dfPK <- PKDDIProcessing(regimens[[i]]$IE, dfPK)
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
  ## Common model covariates
  
  # if UI input is to simulate in an individual-level
  if (input$population_radio == "Individual") {
    
    # 1 "RACE"
    if (input$RACE == "Non-Black") {
      RACE <- 1
    } else {
      RACE <- 2   # in the PK model RACE = 2 means BLACK race
    }
    
    # 2 "WT"
    WT <- input$WT
    
    # 3 "ALB"
    ALB <- input$ALB
    
    # 4 AGE
    AGE <- input$AGE
    
    ## Set parameters in dataset
    dfPK_combined$AGE <- AGE
    dfPK_combined$RACE <- RACE
    dfPK_combined$THETA6 <- WT
    dfPK_combined$THETA1 <- ALB
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
  
  return(out)
}