sim_TTP <- function(input, sim_PKtable) {
  # User input
  num_REPs <- input$REP
  
  # Retrieve information from simulated PK profiles
  Cavg_weekly <- sim_PKtable() %>% filter(AMT == 0)
    
  ####### Use the substituted new data frame d1
  Cavg_weekly <- Cavg_weekly %>% filter(time %% 168 == 0)
  Cavg_weekly$WEEK <- Cavg_weekly$time / 168
  Cavg_weekly <- subset(Cavg_weekly, select = c(ID, time, regimen, WEEK, AAUCBDQ))
  
  # Calculate weekly AUC
  Cavg_weekly$AUCWBDQ <- 0
  
  i <- 2
  while (i <= length(Cavg_weekly$ID)) {
    Cavg_weekly$AUCWBDQ[i] <- Cavg_weekly$AAUCBDQ[i] - Cavg_weekly$AAUCBDQ[i - 1]
    
    i <- i + 1
  }

  Cavg_weekly <- Cavg_weekly %>% mutate(
    AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ)
  )

  ###### summarise by
  Cavg_weekly <- Cavg_weekly %>%
    group_by(ID, time, regimen, WEEK) %>%
    dplyr::summarise(
      weekly_BDQ = mean(AUCWBDQ, na.rm = T)
    )
  
  # Create dataset for simulation
  nsubjects    <- input$nsim
  num_regimens <- sum(c(TRUE, input$RG2, input$RG3, input$RG4))  # Regimen 1 is compulsory
  
  TTPdf   <- tidyr::crossing(
    ID    = seq(nsubjects*num_regimens),
    WEEKP = c(1:24),
    REP   = c(1:num_REPs),
    EVID  = 0,
    AMT   = 0,
    FLAG  = 1,
    TTPD  = c(0, 1:42),
    LASTR = 0) 
  
  TTPdf   <- TTPdf %>%
    mutate(regimen = (ID - 1) %/% nsubjects + 1) %>%
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
    
    # 1. Drug Resistance
    if (input$XDR == "MDR-TB") {
      TTPdf_fin$preAndXDR <- 0
      TTPdf_fin$XDR <- 0
    } else if (input$XDR == "pre-XDR-TB") {
      TTPdf_fin$preAndXDR <- 1
      TTPdf_fin$XDR <- 0
    } else {
      TTPdf_fin$preAndXDR <- 1
      TTPdf_fin$XDR <- 1
    }
    
    
    # 2. Mean time-to-posistivity (MTTP)
    # input$MTTP unit in days, PK-efficacy model unit in hours
    TTPdf_fin$MTTP <- input$MTTP*24
  } else {
    TTPdf_fin <- full_join(TTPdf_fin, Pop_generation(input), by = c("ID", "regimen"))
    TTPdf_fin <- TTPdf_fin %>%
      mutate(preAndXDR = ifelse(TBTYPE == 3 | TBTYPE == 4, 1, 0),  # pre-XDR + XDR
             XDR       = ifelse(TBTYPE == 4, 1, 0)) 
  }

  dfCAVG <- Cavg_weekly %>% rename("WEEKP" ="WEEK") %>%
    filter(WEEKP != 0) %>%
    mutate(CAVG  =  weekly_BDQ/168) %>% # unit µg/mL
    ungroup() %>% select(ID, WEEKP, CAVG)
  
  dfTTP <- TTPdf_fin %>% full_join(dfCAVG)
  
  # TTP simulation ########
  ## Simulation settings
  # 2. "simtime" and "simunit"
  sim_time <- input$simtime   # Time of simulation imputed (transformed in hours during simulation)
  sunit <- convertTimeUnit(input$sunit)   # Simulation unit: "1" day, "2" week
  
  modTTP <- mcode("BDQTTP", codeTTP)
  set.seed(3468)
  
  ## Interindividual variability ON/OFF
  if (input$IIV == "OFF") {
    outTTP <- modTTP %>%
      zero_re() %>%
      data_set(dfTTP) %>%
      mrgsim(end = sim_time * sunit, delta = 1) %>%
      as.data.frame()
  } else { #### no sigma in TTP
    outTTP <- modTTP %>%
      data_set(dfTTP) %>%
      mrgsim(end = sim_time * sunit, delta = 1) %>%
      as.data.frame()
  }
  
  return(outTTP)
}