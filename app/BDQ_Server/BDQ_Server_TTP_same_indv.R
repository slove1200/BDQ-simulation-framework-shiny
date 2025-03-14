sim_TTP <- function(input, sim_PKtable, virtual_population_df) {
  # User input
  num_REPs <- input$REP
  
  # Retrieve information from simulated PK profiles
  Cavg_weekly <- sim_PKtable %>%
    filter(AMT == 0 & time %% 168 == 0) %>%
    mutate(WEEK = time / 168)
  
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
  
  sim_time <- input$simtime   # Time of simulation imputed (transformed in hours during simulation)
  
  TTPdf   <- tidyr::crossing(
    ID    = seq(nsamples*num_regimens),
    WEEKP = c(1:sim_time),
    REP   = c(1:num_REPs),
    EVID  = 0,
    AMT   = 0,
    FLAG  = 1,
    TTPD  = c(0, 1:42),
    LASTR = 0) 
  
  TTPdf   <- TTPdf %>%
    mutate(regimen = (ID - 1) %/% nsamples + 1) %>%
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
  
  if (input$STUDY == "Treatment-naïve") {
    modTTP <- mcode("BDQTTP", codeTTP)
    modTTP <- update(modTTP, outvars = outvars(modTTP)$capture)
  } else {
    modTTP <- mcode("BDQTTP_TrtExperienced", codeTTP_TrtExperienced)
    modTTP <- update(modTTP, outvars = outvars(modTTP)$capture)
  }
  
  ## Interindividual variability ON/OFF
  if (input$IIV == "OFF") {
    set.seed(3468)
    
    outTTP <- modTTP %>%
      zero_re() %>%
      data_set(dfTTP) %>%
      mrgsim(end = sim_time * 168, delta = 1) %>%
      as.data.frame()
  } else { #### no sigma in TTP
    # Filter and run simulation separately
    outTTP <- map_dfr(1:num_regimens, ~ {
      start_idx <- (.x - 1) * nsamples + 1
      end_idx <- .x * nsamples
      
      # Filter the dataset for the current regimen
      filtered_dfTTP <- dfTTP %>%
        filter(ID %in% start_idx:end_idx)
      
      # Set random seed and run the simulation for the current dataset
      set.seed(3468)
      modTTP %>%
        data_set(filtered_dfTTP) %>%
        mrgsim(end = sim_time * 168, delta = 1) %>%
        as.data.frame()
    })
  }
  
  return(outTTP)
}