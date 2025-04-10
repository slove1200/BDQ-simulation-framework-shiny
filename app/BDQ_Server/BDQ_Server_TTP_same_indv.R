sim_TTP <- function(input, sim_PKtable, virtual_population_df) {
  
  reg1_dur <- NULL
  reg2_dur <- NULL
  reg3_dur <- NULL
  
  # calculate duration
  reg1_dur <- (if(input$LD1) input$ldur_1 * ifelse(input$lunit_1 == "2", 1, 1/7) else 0) + 
    (input$mdur_1 * ifelse(input$munit_1 == "2", 1, 1/7)) +
    (if(input$MD2_1) input$m2dur_1 * ifelse(input$m2unit_1 == "2", 1, 1/7) else 0)
  
  reg2_dur <- (if(input$LD2) input$ldur_2 * ifelse(input$lunit_2 == "2", 1, 1/7) else 0) + 
    (input$mdur_2 * ifelse(input$munit_2 == "2", 1, 1/7)) +
    (if(input$MD2_2) input$m2dur_2 * ifelse(input$m2unit_2 == "2", 1, 1/7) else 0)
  
  reg3_dur <- (if(input$LD3) input$ldur_3 * ifelse(input$lunit_3 == "2", 1, 1/7) else 0) + 
    (input$mdur_3 * ifelse(input$munit_3 == "2", 1, 1/7)) +
    (if(input$MD2_3) input$m2dur_3 * ifelse(input$m2unit_3 == "2", 1, 1/7) else 0)
  
  durations <- c(reg1_dur, reg2_dur, reg3_dur)
  max_dur <- max(durations, na.rm = TRUE)
  
  # User input
  num_REPs <- input$REP
  
  # Retrieve information from simulated PK profiles
  Cavg_weekly <- sim_PKtable %>%
    filter(AMT == 0 & time %% 168 == 0) %>%
    mutate(WEEK = time / 168)  %>%
    filter(
      (regimen == 1 & WEEK <= reg1_dur) |
      (regimen == 2 & WEEK <= reg2_dur) |
      (regimen == 3 & WEEK <= reg3_dur)
    )
  
  # Subset relevant columns
  Cavg_weekly <- Cavg_weekly %>% select(ID, time, regimen, WEEK, AAUCBDQ, AAUCM2)
  
  # Calculate weekly AUC
  Cavg_weekly$AUCWBDQ <- 0
  Cavg_weekly$AUCWM2 <- 0
  
  # Calculate daily AUC using the `lag` function for vectorized operations
  Cavg_weekly <- Cavg_weekly %>%
    mutate(
      AUCWBDQ = AAUCBDQ - lag(AAUCBDQ, default = 0),
      AUCWM2  = AAUCM2 - lag(AAUCM2, default = 0)
    )
  
  # Ensure AUC values are zero for `time == 0` as required
  Cavg_weekly <- Cavg_weekly %>%
    mutate(
      AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ),
      AUCWM2  = ifelse(time == 0, 0, AUCWM2)
    )

  ###### summarise by
  Cavg_weekly <- Cavg_weekly %>%
    group_by(ID, time, regimen, WEEK) %>%
    dplyr::summarise(
      weekly_BDQ = mean(AUCWBDQ, na.rm = T)
    )
  
  # Create dataset for simulation
  nsamples    <- input$nsim
  num_regimens <- sum(c(TRUE, input$RG2, input$RG3))  # Regimen 1 is compulsory
  
  TTPdf   <- tidyr::crossing(
    ID    = seq(nsamples*num_regimens),
    WEEKP = c(1:max_dur),
    REP   = c(1:num_REPs),
    EVID  = 0,
    AMT   = 0,
    FLAG  = 1,
    TTPD  = c(0, 1:42),
    LASTR = 0) 
  
  TTPdf   <- TTPdf %>%
    mutate(regimen = (ID - 1) %/% nsamples + 1) %>%
    filter(WEEKP <= ifelse(regimen == 1, reg1_dur, ifelse(regimen == 2, reg2_dur, reg3_dur))) %>%
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
  
  #### Covariates
  # if UI input is to simulate in an individual-level
  if (input$population_radio == "Individual") {
    
    # 1. Mean time-to-posistivity (MTTP)
    # input$MTTP unit in days, PK-efficacy model unit in hours
    TTPdf_fin$MTTP <- input$MTTP*24
  } else {
    # Dynamically bind rows based on `num_regimens`
    virtual_population_df <- map_dfr(1:num_regimens, ~ {
        virtual_population_df %>%
          filter(ID %in% 1:nsamples) %>%
          mutate(regimen  = .x) # Optional: Add a column to indicate duplication
      }) %>% ungroup() %>%
      mutate(ID = row_number())
    
    TTPdf_fin <- left_join(TTPdf_fin, virtual_population_df, by = c("ID", "regimen"))
  }
  
  # mean HL in BDQ-TTP simulation = 0.54
  TTPdf_fin$HLEFF <- (input$HLEFF/0.54-1)*100

  dfCAVG <- Cavg_weekly %>% rename("WEEKP" ="WEEK") %>%
    filter(WEEKP != 0) %>%
    mutate(
      AUCW  =  weekly_BDQ,         # unit µg*h/mL
      CAVG  =  weekly_BDQ/168) %>% # unit µg/mL
    ungroup() %>% select(ID, WEEKP, AUCW, CAVG)
  
  dfTTP <- TTPdf_fin %>% full_join(dfCAVG)
  
  # TTP simulation ########
  ## Simulation settings
  # 2. "simtime" and "simunit"
  modTTP <- mcode("BDQTTP", codeTTP)
  modTTP <- update(modTTP, outvars = outvars(modTTP)$capture)

  
# Determine seed
if (input$SEED_TTP) {
  seed_val <- sample(1e6, 1)  # Random seed between 1 and 1,000,000
} else {
  seed_val <- 3468  # Default fixed seed
}

## Interindividual variability ON/OFF
if (input$IIV == "OFF") {
  outTTP <- map_dfr(1:num_regimens, ~ {
    this_dur <- durations[.x]
    start_idx <- (.x - 1) * nsamples + 1
    end_idx <- .x * nsamples
    
    filtered_dfTTP <- dfTTP %>%
      filter(ID %in% start_idx:end_idx)
    
    set.seed(seed_val)
    
    modTTP %>%
      zero_re() %>%
      data_set(filtered_dfTTP) %>%
      mrgsim(end = this_dur * 168, delta = 1) %>%
      as.data.frame()
  })
} else { #### no sigma in TTP
  outTTP <- map_dfr(1:num_regimens, ~ {
    this_dur <- durations[.x]
    start_idx <- (.x - 1) * nsamples + 1
    end_idx <- .x * nsamples
    
    filtered_dfTTP <- dfTTP %>%
      filter(ID %in% start_idx:end_idx)
    
    set.seed(seed_val)
    
    modTTP %>%
      data_set(filtered_dfTTP) %>%
      mrgsim(end = this_dur * 168, delta = 1) %>%
      as.data.frame()
  })
}
  
  return(outTTP)
}