#### MSM ####
######## Create MSM dataframe for simulation
sim_MSM <- function(input, sim_TTPtable, sim_PKtable) {
  
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

  # Create dataset for simulation
  nsubjects    <- input$nsim
  num_regimens <- sum(c(TRUE, input$RG2, input$RG3))  # Regimen 1 is compulsory
  # "simtimeMSM" and "simunitMSM" for MSM
  sim_timeMSM <- input$simtimeMSM   # Time of simulation imputed (transformed in hours during simulation)
  
  # Get MBLend and HL2 from TTP output
  HLMBL <- sim_TTPtable %>%
    filter(REP == 1 & FLAG == 2) %>%
    mutate(
      dur = case_when(
        regimen == 1 ~ reg1_dur, 
        regimen == 2 ~ reg2_dur, 
        regimen == 3 ~ reg3_dur
      )
    ) %>%
    filter(
      (regimen == 1 & WEEKP %in% c(1, 2, floor(reg1_dur))) |
      (regimen == 2 & WEEKP %in% c(1, 2, floor(reg2_dur))) |
      (regimen == 3 & WEEKP %in% c(1, 2, floor(reg3_dur)))
      ) %>%
    group_by(ID) %>%
    mutate(MBLend = first(MBL[WEEKP == floor(dur)]))
  
  # Create a copy of the rows where WEEKP = 1
  new_rows <- HLMBL %>% group_by(ID) %>%
    filter(WEEKP == 1) %>% slice(1L) %>%
    mutate(WEEKP = 0)  # Change WEEKP to 0
  
  new_rows2 <- HLMBL %>% group_by(ID) %>%
    filter(WEEKP == 1) %>% slice(1L) %>%
    mutate(WEEKP = 3)  # Change WEEKP to 3
  
  
  # Bind the new rows to the original dataframe
  HLMBL2 <- bind_rows(HLMBL, new_rows, new_rows2) %>% arrange(ID, WEEKP)
  
  # mean HL in BDQ-TTP simulation = 0.54
  HLEFF2 <- (input$HLEFF/0.54-1)*100

  TTPcov <- HLMBL2 %>% group_by(ID) %>%
    mutate(HL2 = ifelse(WEEKP == 0, 0.69443*(1+(HLEFF2/100)), lag(HL))) %>%
    mutate(HL2 = ifelse(WEEKP == 1, 0.69443*(1+(HLEFF2/100)), HL2), # median of HL
           time = WEEKP*168)  %>% # hours
    filter(WEEKP %in% c(0, 1, 2, 3)) %>%
    mutate(MBLend = ifelse(MBLend == 0, 1e-300, MBLend)) %>% # set up a lower limit of MBLend to prevent log(0)
    select(ID, MTTP, time, HL2, MBLend, dur)


  ####
  # Set up event
  ev0 <- ev(time = 0, amt = 1, cmt = 1, ID = seq(nsubjects*num_regimens))
  ev1 <- ev(time = 0, amt = c(0,1,1,1,1,1), cmt = c(0,1,2,3,4,5), evid = c(2,4,1,1,1,1),
            ID = seq(nsubjects*num_regimens), addl = sim_timeMSM, ii = 168, rate = 0, realize = T)
  data.dose <- seq(ev0, ev1)
  data.dose <- data.table::setDT(as.data.frame(data.dose)) %>% arrange(ID, time) %>%
    mutate(evid = ifelse(time != 0 & cmt == 1, 4, ifelse(evid == 2, 0, evid)))


  #### Covariates used in MSM
  # MTTP (retrieved from TTP dataset), XDR (from TTP model),
  # HL2 (derived from TTP model), MBLend [the end of treatment, depends on the duration of the regimen] (derived from TTP model)
  # SEX, WT (same with QT, retrieved from PK dataset)
  dfCov <- sim_PKtable %>% filter(time == 0)
  dfCov <- dfCov %>% select(ID, regimen, WT, SEX)

  idata <- data.table::data.table(ID=seq(nsubjects*num_regimens)) %>% left_join(dfCov, by = "ID")
  data.all <- merge(data.dose, idata, by = "ID") %>% left_join(TTPcov, by = c("ID", "time")) %>%
    group_by(ID) %>% zoo::na.locf()

  # MSM simulation ########
  # Simulation settings

  # Determine seed
  if (input$SEED_MSM) {
    seed_val <- sample(1e6, 1)  # Random seed between 1 and 1,000,000
  } else {
    seed_val <- 3468  # Default fixed seed
  }

  modMSM <- mcode("BDQMSM", BDQMSM)
  modMSM <- update(modMSM, outvars = outvars(modMSM)$capture)

  set.seed(seed_val)

  outMSM <- modMSM %>%
    data_set(data.all) %>%
    mrgsim(end = sim_timeMSM*168, delta = 168) %>%
    as.data.frame %>%
    filter(EVID == 0) %>%
    mutate(time = time/24/7) %>%
    rename("STATE" = "XDV") %>%
    select(-EVID, -P_4)

  return(outMSM)
}