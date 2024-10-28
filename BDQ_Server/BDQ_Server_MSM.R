#### MSM ####
######## Create MSM dataframe for simulation
sim_MSM <- function(input, sim_TTPtable, sim_PKtable) {
  
  # Create dataset for simulation
  sim_time     <- input$simtime
  nsubjects    <- input$nsim
  num_regimens <- sum(c(TRUE, input$RG2, input$RG3, input$RG4))  # Regimen 1 is compulsory
  # "simtimeMSM" and "simunitMSM" for MSM
  sim_timeMSM <- input$simtimeMSM   # Time of simulation imputed (transformed in hours during simulation)
  
  # Get MBLend and HL2 from TTP output
  HLMBL <- sim_TTPtable() %>%
    filter(REP == 1 & FLAG == 2) %>%
    mutate(
      dur = case_when(
        regimen == 1 & input$LD1 == TRUE  ~ input$ldur_1 + input$mdur_1,
        regimen == 2 & input$LD2 == TRUE  ~ input$ldur_2 + input$mdur_2,
        regimen == 3 & input$LD3 == TRUE  ~ input$ldur_3 + input$mdur_3,
        regimen == 4 & input$LD4 == TRUE  ~ input$ldur_4 + input$mdur_4, 
        regimen == 1 & input$LD1 == FALSE ~ input$mdur_1,
        regimen == 2 & input$LD2 == FALSE ~ input$mdur_2,
        regimen == 3 & input$LD3 == FALSE ~ input$mdur_3,
        regimen == 4 & input$LD4 == FALSE ~ input$mdur_4
      )
    ) %>%
    filter(
        (regimen == 1 & WEEKP %in% c(1, 2, if (input$LD1) input$ldur_1 + input$mdur_1 else input$mdur_1)) |
        (regimen == 2 & WEEKP %in% c(1, 2, if (input$LD2) input$ldur_2 + input$mdur_2 else input$mdur_2)) |
        (regimen == 3 & WEEKP %in% c(1, 2, if (input$LD3) input$ldur_3 + input$mdur_3 else input$mdur_3)) |
        (regimen == 4 & WEEKP %in% c(1, 2, if (input$LD4) input$ldur_4 + input$mdur_4 else input$mdur_4))
    ) %>%
    mutate(MBLend = first(MBL[WEEKP == dur]))
  
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
    mutate(HL2 = ifelse(WEEKP == 0, 0.69443, lag(HL))) %>%
    mutate(HL2 = ifelse(WEEKP == 1, 0.69443, HL2), # median of HL
           time = WEEKP*168)  %>% # hours
    filter(WEEKP %in% c(0, 1, 2, 3)) %>%
    select(ID, MTTP, XDR, time, HL2, MBLend, dur, MBL) %>%
    mutate(MBLendLog10 = log10(MBLend), 
            MBLLog10    = log10(MBL))


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
  # SEX (same with QT, retrieved from PK dataset), baseWT (retrieved from PK dataset)
  dfCov <- sim_PKtable() %>% filter(time == 0)
  dfCov <- dfCov %>% select(ID, regimen, WT = IPREDWT, SEX)

  idata <- data.table::data.table(ID=seq(nsubjects*num_regimens)) %>% left_join(dfCov, by = "ID")
  data.all <- merge(data.dose, idata, by = "ID") %>% left_join(TTPcov, by = c("ID", "time")) %>%
    group_by(ID) %>% zoo::na.locf()

  # dftentimes <- map_df(1:10, ~ {
  #   data.all %>%
  #     mutate(ID = ID + input$nsim * (.x - 1))  # Adjust ID for each copy
  # })

  # MSM simulation ########
  # Simulation settings

  modMSM <- mcode("BDQMSM", BDQMSM)
  modMSM <- update(modMSM, outvars = outvars(modMSM)$capture)

  set.seed(3468)

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