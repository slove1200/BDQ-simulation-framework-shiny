# Title : PK Shiny app
######## .
# Load libraries ####
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

UI.directory <- "//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_UI/"
Server.directory <- "//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_Server/"

# Source MainTab: About, Dosing, Population, Simulation, Results ... details
source(paste0(UI.directory, "BDQ_Shiny_UI_About.R"))
source(paste0(UI.directory, "BDQ_Shiny_UI_Dosing.R"))
source(paste0(UI.directory, "BDQ_Shiny_UI_Population.R"))
source(paste0(UI.directory, "BDQ_Shiny_UI_Simulation.R"))
source(paste0(UI.directory, "BDQ_Shiny_UI_Results.R"))

# ui ####
ui <-
  fluidPage(
    tags$style(HTML("
    .shiny-options-group {
      margin-top: 0 !important;
    }
    div.comed-checkbox div.checkbox:first-of-type label {
      font-weight: bold;
    }
                    ")),
    
    navbarPage(
      header = tagList(""),  # This will be displayed above the tabs
      footer = tagList(""),        # This will be displayed below the tabs
      "BEDAQUILINE DOSE REGIMEN",
      
      tabsetPanel(
        id = "mainTab",
        selected = "Dosing", # Default shown when opening the app

        # Call UI - Dosing
        mainTabDosing,

        # Call UI - Population
        mainTabPopulation,

        # Call UI - Simulation
        mainTabSim,

        # Call UI - Results
        mainTabResults, 
        
        # Call UI - About   
        mainTabAbout


      ) # end of tabsetPanel
    ), # end of navbarPage
  ) # end of fluidPage

#### Define server logic ####
source(paste0(Server.directory, "BDQOMAT.R"))

server <- function(input, output, session) {
  
  # PK simulation ####
  source(paste0(Server.directory, "BDQ_Server_PK.R"))

  # Use the function defined in the sourced file
  sim_PKtable <- eventReactive(input$goButton, {
    sim_dataframePK(input)  # Call the function and pass input
  })
  
  # Render table
  output$sim_PKtable <- renderTable({
    # Call the reactive expression to get the data frame
    sim_PKtable()
  })
  
  # Render combined plot
  source(paste0(Server.directory, "BDQ_Server_PKplots.R"))

  output$plot <- renderPlot({
    create_plots(sim_PKtable)  # Call the function from the sourced file
  })


  dfReadyForQT <- eventReactive(input$goButton, {
    dfQT <- sim_dataframePK() %>% filter(AMT == 0)
    dfQT <- subset(dfQT, select = c(ID, REGIMEN, time, IPREDM2, RACE, AGE))
    # dfQT <- dfQT %>% mutate(CONCM2_weekly = dd2$weekly_M2/168*1000)
    dfQT <- dfQT %>% mutate(CONCM2 = exp(IPREDM2) * 1000) ## mg/L to ng/mL (concentration unit used in QT model)
    dfQT$TIMW <- 0#dfQT$time / 24 / 7 ## TIMW = TAST/24/7 for time effect in QT model
    ## Covariates
    # 1. "SEX"
    if (input$SEX == "Male") {
      dfQT$SEX <- 0
    } else {
      dfQT$SEX <- 1
    }

    # 2. Electrolytes level (Corrected Ca and Potassium level)
    dfQT$CACOR <- input$CACOR # 2.440
    dfQT$K <- input$K # 4.200

    # 3. DDI (Clofazimine or Moxifloxacin)
    if (input$IE == "CFZ") {
      dfQT$CLOFA <- 1
    }
    if (input$IE == "MFX") {
      dfQT$MOXI <- 1
    }

    return(dfQT)
  })

  #### Simulation: QT
  sim_dataframeQT <- eventReactive(input$goButton, {
    ## Simulation settings
    # 1. "nsim"
    nsamples <- as.numeric(input$nsim)      # Number of simulated individuals

    # 2. "simtime" and "simunit"
    sim_time <- as.numeric(input$simtime)   # Time of simulation imputed (transformed in hours during simulation)
    sunit <- convertTimeUnit(input$sunit)   # Simulation unit: "1" day, "2" week

    ###############################################
    ### PK simulation
    modQT <- mcode("BDQQT", codeQT)

    ## Interindividual variability ON/OFF
    if (input$IIV == "OFF") {
      set.seed(3468)
      outQT <- modQT %>%
        zero_re() %>%
        data_set(dfReadyForQT()) %>%
        mrgsim(end = sim_time * sunit, delta = 1) %>%
        as.data.frame()
      outQT$REGIMEN <- "1"
      return(outQT)
    } else {
      set.seed(3468)
      outQT <- modQT %>%
        zero_re(sigma) %>%
        data_set(dfReadyForQT()) %>%
        mrgsim(end = sim_time * sunit, delta = 1) %>%
        as.data.frame()
      outQT$REGIMEN <- "1"
      return(outQT)
    }
  })


  #### DATAFRAME FOR TROUGH CONC AT 24 HRS
  sim_dataframe24 <- eventReactive(input$goButton, {
    df <- sim_dataframePK() %>% filter(AMT == 0)
    df <- df %>% filter(time %% 24 == 0)
    df$DAY <- df$time / 24
    return(df)
  })

  ####### Reactive MEAN DAILY CONCENTRATION
  Cavg_daily <- eventReactive(input$goButton, {
    d1 <- sim_dataframePK() %>% filter(AMT == 0)

    d1 <- d1 %>% filter(time %% 24 == 0)

    d1$DAY <- d1$time / 24

    d1 <- subset(d1, select = c(ID, REGIMEN, time, DAY, RACE, AUCBDQ, AUCM2))

    ###### BDQ
    d1$AUCWBDQ <- "0"
    i <- 2
    while (i <= length(d1$ID)) {
      d1$AUCWBDQ[i] <- d1$AUCBDQ[i] - d1$AUCBDQ[i - 1]

      i <- i + 1
    }

    ########## M2
    d1$AUCWM2 <- "0"

    i <- 2
    while (i <= length(d1$ID)) {
      d1$AUCWM2[i] <- d1$AUCM2[i] - d1$AUCM2[i - 1]

      i <- i + 1
    }

    d1 <- d1 %>% mutate(
      AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ),
      AUCWM2 = ifelse(time == 0, 0, AUCWM2)
    )

    ####### as numeric
    d1$AUCWBDQ <- as.numeric(as.character(d1$AUCWBDQ))
    d1$AUCWM2 <- as.numeric(as.character(d1$AUCWM2))

    ###### summarise by
    dd1 <- d1 %>%
      group_by(ID, time, REGIMEN, DAY) %>%
      dplyr::summarise(
        daily_BDQ = mean(AUCWBDQ, na.rm = T),
        daily_M2 = mean(AUCWM2, na.rm = T)
      )

    return(dd1)
  })


  #### WEEKLY Cavg
  Cavg_weekly <- eventReactive(input$goButton, {
    d2 <- sim_dataframePK() %>% filter(AMT == 0)

    ####### Use the substituted new data frame d1
    d2 <- d2 %>% filter(time %% 168 == 0)

    d2$WEEK <- d2$time / 168

    d2 <- subset(d2, select = c(ID, time, REGIMEN, WEEK, AUCBDQ, AUCM2))

    ###### BDQ
    d2$AUCWBDQ <- "0"

    i <- 2
    while (i <= length(d2$ID)) {
      d2$AUCWBDQ[i] <- d2$AUCBDQ[i] - d2$AUCBDQ[i - 1]

      i <- i + 1
    }

    ########## M2
    d2$AUCWM2 <- "0"

    i <- 2
    while (i <= length(d2$ID)) {
      d2$AUCWM2[i] <- d2$AUCM2[i] - d2$AUCM2[i - 1]

      i <- i + 1
    }


    d2 <- d2 %>% mutate(
      AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ),
      AUCWM2 = ifelse(time == 0, 0, AUCWM2)
    )

    ####### as numeric
    d2$AUCWBDQ <- as.numeric(as.character(d2$AUCWBDQ))
    d2$AUCWM2 <- as.numeric(as.character(d2$AUCWM2))

    ###### summarise by
    dd2 <- d2 %>%
      group_by(ID, time, REGIMEN, WEEK) %>%
      dplyr::summarise(
        weekly_BDQ = mean(AUCWBDQ, na.rm = T),
        weekly_M2 = mean(AUCWM2, na.rm = T)
      )

    return(dd2)
  })

  #### TTP ####
  ######## Create TTP dataframe for simulation
  dfReadyForTTP <- eventReactive(input$goButton, {
    TTPdf   <- tidyr::crossing(
      ID    = c(1:input$nsim),
      WEEKP = c(1:24),
      REP   = c(1:3),
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

    # MTTP2 SAMPLING
    set.seed(100)
    TTPdf_fin <- TTPdf_fin %>%
      group_by(ID) %>%
      dplyr::mutate(MTTP2 = runif(1, 55.2, 1008))

    dfCAVG <- Cavg_weekly() %>% rename("WEEKP" ="WEEK") %>%
      filter(WEEKP != 0) %>%
      mutate(CAVG  =  weekly_BDQ/168) %>% # unit µg/mL
      ungroup() %>% select(ID, WEEKP, CAVG)

    dfTTP <- TTPdf_fin %>% full_join(dfCAVG)

    return(dfTTP)
  })

  #### Simulation: TTP
  sim_dataframeTTP <- eventReactive(input$goButton, {
    ## Simulation settings
    # 1. "nsim"
    nsamples <- as.numeric(input$nsim)      # Number of simulated individuals

    # 2. "simtime" and "simunit"
    sim_time <- as.numeric(input$simtime)   # Time of simulation imputed (transformed in hours during simulation)
    sunit <- convertTimeUnit(input$sunit)   # Simulation unit: "1" day, "2" week


    modTTP <- mcode("BDQTTP", codeTTP)

    ## Interindividual variability ON/OFF
    if (input$IIV == "OFF") {
      set.seed(3468)
      outTTP <- modTTP %>%
        zero_re() %>%
        data_set(dfReadyForTTP()) %>%
        mrgsim(end = sim_time * sunit, delta = 1) %>%
        as.data.frame()
      outTTP$REGIMEN <- "1"
      return(outTTP)
    } else { #### no sigma in TTP
      set.seed(3468)
      outTTP <- modTTP %>%
        data_set(dfReadyForTTP()) %>%
        mrgsim(end = sim_time * sunit, delta = 1) %>%
        as.data.frame()
      outTTP$REGIMEN <- "1"
      return(outTTP)
    }
  })

  #### MSM ####
  ######## Create MSM dataframe for simulation
  dfReadyForMSM <- eventReactive(input$goButton, {

    # Get MBLend and HL2 from TTP output
    HLMBL <- sim_dataframeTTP() %>% filter(REP == 1 & WEEKP %in% c(1,2,24) & FLAG == 2)

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
             MBLend = MBL[WEEKP == 24]) %>%
      mutate(HL2 = ifelse(WEEKP == 1, 0.69443, HL2), # median of HL
             time = WEEKP*168)  %>% # hours
      select(ID, MTTP, XDR, time, HL2, MBLend)


    ####
    # Set up event
    ev0 <- ev(time = 0, amt = 1, cmt = 1, ID = seq(input$nsim))
    ev1 <- ev(time = 0, amt = c(0,1,1,1,1,1), cmt = c(0,1,2,3,4,5), evid = c(2,4,1,1,1,1), ID = seq(input$nsim),
              addl = 120, ii = 168, rate = 0, realize = T)
    data.dose <- seq(ev0, ev1)
    data.dose <- data.table::setDT(as.data.frame(data.dose)) %>% arrange(ID, time) %>%
      mutate(evid = ifelse(time != 0 & cmt == 1, 4, ifelse(evid == 2, 0, evid)))


    #### covariate distribution sampling/combination
    # MTTP (same with TTP), XDR (in TTP is MDR and pre-XDR/XDR, in MSM is non-XDR and XDR),
    # HL2 (derived from TTP), MBLend (derived from TTP), SEX (same with QT), baseWT (from PK)
    PKcov <- sim_dataframePK() %>% group_by(ID) %>% slice(1L) %>% select(ID, WT, AGE, RACE)
    QTcov <- sim_dataframeQT() %>% group_by(ID) %>% slice(1L) %>% select(ID, SEX)

    idata <- data.table::data.table(ID=1:input$nsim) %>% left_join(PKcov)
    data.all <- merge(data.dose, idata, by = "ID") %>% left_join(TTPcov) %>%
      group_by(ID) %>% zoo::na.locf()

    dftentimes <- map_df(1:10, ~ {
      data.all %>%
        mutate(ID = ID + input$nsim * (.x - 1))  # Adjust ID for each copy
    })

    return(dftentimes)
  })

  #### Simulation: MSM
  sim_dataframeMSM <- eventReactive(input$goButton, {
    modMSM <- mcode("CodeMSM", codeMSM)
    modMSM <- update(modMSM, outvars = outvars(modMSM)$capture)

    set.seed(3468)
    outMSM <- modMSM %>%
      data_set(dfReadyForMSM()) %>%
      mrgsim(end = 20160, delta = 168) %>%
      as.data.frame %>%
      filter(EVID == 0) %>%
      mutate(time = time/24/7) %>%
      rename("STATE" = "XDV") %>%
      select(-EVID, -P_4)
    outMSM$REGIMEN <- "1"
    return(outMSM)
  })



  ########################################################
  ##### RENDER IMAGE ####
  #### render image
  output$img1 <- renderImage({
    filename <- "//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/Materials/BDQ_PK.png"
    list(src = filename,
         width = "600",
         height = "100%")
  },
  deleteFile = FALSE)

  ######## OUTPUTS reactive plot
  ipred <- eventReactive(input$goButton, {
    input$ipred
  })
  plot <- eventReactive(input$goButton, {
    input$plot
  })
  sim_time <- eventReactive(input$goButton, {
    input$sim_time
  })

  output$plot2 <- renderPlot({
    input$goButton
    RACE <- isolate(input$RACE)
    IE <- isolate(input$IE)

    if (input$ipred == 1) {
      # if(input$IIV == "ON") {
      dfForPlotBDQ <- sim_dataframePK() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(exp(IPRED), probs = 0.05),
          median = quantile(exp(IPRED), probs = 0.5),
          upper = quantile(exp(IPRED), probs = 0.95)
        )

      a1 <- ggplot(dfForPlotBDQ, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        # scale_y_continuous(breaks = seq(0, 4, 0.5),limits = c(0,2.5)) +
        labs(x = "Time (weeks)", y = c("BDQ concentration (mg/L)")) +
        ggtitle("BDQ Concentration (mg/L) vs Time")

      dfForPlotM2 <- sim_dataframePK() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(exp(IPREDM2), probs = 0.05),
          median = quantile(exp(IPREDM2), probs = 0.5),
          upper = quantile(exp(IPREDM2), probs = 0.95)
        )

      a2 <- ggplot(dfForPlotM2, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("M2 concentration (mg/L)")) +
        ggtitle("M2 Concentration (mg/L) vs Time")

      plot <- grid.arrange(a1, a2, nrow = 2)
    } else if (input$ipred == 2) {
      dfForPlotCtBDQ <- sim_dataframe24() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(exp(IPRED), probs = 0.05),
          median = quantile(exp(IPRED), probs = 0.5),
          upper = quantile(exp(IPRED), probs = 0.95)
        )

      b1 <- ggplot(dfForPlotCtBDQ, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("BDQ concentration (mg/L)")) +
        ggtitle("BDQ Concentration (mg/L) vs Time")

      dfForPlotCtM2 <- sim_dataframe24() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(exp(IPREDM2), probs = 0.05),
          median = quantile(exp(IPREDM2), probs = 0.5),
          upper = quantile(exp(IPREDM2), probs = 0.95)
        )

      b2 <- ggplot(dfForPlotCtM2, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("M2 concentration (mg/L)")) +
        ggtitle("M2 Concentration (mg/L) vs Time")

      plot <- grid.arrange(b1, b2, nrow = 2)
    } else if (input$ipred == 3) {
      dfForPlotDBDQ <- Cavg_daily() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(daily_BDQ, probs = 0.05),
          median = quantile(daily_BDQ, probs = 0.5),
          upper = quantile(daily_BDQ, probs = 0.95)
        )

      c1 <- ggplot(dfForPlotDBDQ, aes(x = time / 168, y = median / 24)) +
        geom_ribbon(aes(ymin = lower / 24, ymax = upper / 24), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("BDQ concentration (mg/L)")) +
        ggtitle("Daily Average BDQ Concentration (mg/L) vs Time (weeks)")

      dfForPlotDM2 <- Cavg_daily() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(daily_M2, probs = 0.05),
          median = quantile(daily_M2, probs = 0.5),
          upper = quantile(daily_M2, probs = 0.95)
        )

      c2 <- ggplot(dfForPlotDM2, aes(x = time / 168, y = median / 24)) +
        geom_ribbon(aes(ymin = lower / 24, ymax = upper / 24), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("M2 concentration (mg/L)")) +
        ggtitle("Daily Average M2 Concentration (mg/L) vs Time (weeks)")

      plot <- grid.arrange(c1, c2, nrow = 2)
    } else if (input$ipred == 4) {
      dfForPlotWBDQ <- Cavg_weekly() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(weekly_BDQ, probs = 0.05),
          median = quantile(weekly_BDQ, probs = 0.5),
          upper = quantile(weekly_BDQ, probs = 0.95)
        )

      d1 <- ggplot(dfForPlotWBDQ, aes(x = time / 168, y = median / 168)) +
        geom_ribbon(aes(ymin = lower / 168, ymax = upper / 168), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("BDQ concentration (mg/L)")) +
        ggtitle("Weekly Average BDQ Concentration (mg/L) vs Time (weeks)")

      dfForPlotWM2 <- Cavg_weekly() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(weekly_M2, probs = 0.05),
          median = quantile(weekly_M2, probs = 0.5),
          upper = quantile(weekly_M2, probs = 0.95)
        )

      d2 <- ggplot(dfForPlotWM2, aes(x = time / 168, y = median / 168)) +
        geom_ribbon(aes(ymin = lower / 168, ymax = upper / 168), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("M2 concentration (mg/L)")) +
        ggtitle("Weekly Average M2 Concentration (mg/L) vs Time (weeks)")

      plot <- grid.arrange(d1, d2, nrow = 2)
    } else if (input$ipred == 5) {
      dfForPlotWT <- sim_dataframePK() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(IPREDWT, probs = 0.05),
          median = quantile(IPREDWT, probs = 0.5),
          upper = quantile(IPREDWT, probs = 0.95)
        )

      e1 <- ggplot(dfForPlotWT, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 25)) +
        # scale_y_continuous(breaks = seq(0, 4, 0.5),limits = c(0,2.5)) +
        labs(x = "Time (weeks)", y = ("Body weight (kg)")) +
        ggtitle("Body Weight (kg) vs Time (weeks)")

      dfForPlotALB <- sim_dataframePK() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(IPREDALB, probs = 0.05),
          median = quantile(IPREDALB, probs = 0.5),
          upper = quantile(IPREDALB, probs = 0.95)
        )

      e2 <- ggplot(dfForPlotALB, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 25)) +
        labs(x = "Time (weeks)", y = ("Albumin concentration (g/dL)")) +
        ggtitle("Albumin Concentration (g/dL) vs Time(weeks)")

      plot <- grid.arrange(e1, e2, nrow = 2)
    } else if (input$ipred==6) {
      dfForPlotQT <- sim_dataframeQT() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(IPRED, probs = 0.05),
          median = quantile(IPRED, probs = 0.5),
          upper = quantile(IPRED, probs = 0.95)
        )

      plot <- ggplot(dfForPlotQT, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        scale_y_continuous(breaks = seq(380, 450, 5)) +
        labs(x = "Time (weeks)", y = ("QTc (ms)")) +
        ggtitle("QTc (ms) vs Time (weeks)")
    } else if (input$ipred==7) {
      # --- Get the proportion of samples without positive signal ---
      outTTE <- sim_dataframeTTP() %>% filter(RTTE == 1) ## positive signal at specific time
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

      plot <- ggplot(TTEcountw, aes(TTPD, proportion*100)) +
        facet_wrap(~WEEKP, labeller = week_labels, ncol = 3, scales = "free_x") +
        geom_line(size = 1) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        xlab("Time in MGIT after inoculation (days)") +
        ylab("Proportion of samples without positive signal (%)") +
        ggtitle("Simulated TTP in MGIT per week after start of treatment") +
        theme(
          plot.title = element_text(size = 18, hjust = 0.5),       # Main title
          axis.title = element_text(size = 15),                    # Axis titles
          axis.text = element_text(size = 15),                     # Axis text
          legend.position = "none",
          strip.text = element_text(size = 15)
        ) +
        scale_y_continuous(breaks = seq(0, 100, by = 20))
    } else {
      # --- proportions of patients being in each state ---
      total <- dplyr::n_distinct(sim_dataframeMSM()$ID)
      summary_MSM <- sim_dataframeMSM() %>% group_by(time, STATE) %>% reframe(prop = n()/total) %>%
        complete(time, STATE, fill = list(prop = 0))

      # Custom labeller function to change facet titles
      state_labels <- as_labeller(function(STATE) {
        case_when(STATE == 1 ~ "Active TB",
                  STATE == 2 ~ "Converted",
                  STATE == 3 ~ "Recurrent TB",
                  STATE == 5 ~ "Death")
      })

      plot <- ggplot(summary_MSM %>% filter(time %in% c(0,4,8,16,24,48,72,96,120)), aes(x = time, y = prop*100)) +
        geom_line(size = 0.8) +
        geom_point(size = 2, shape = 1) +
        facet_wrap(~STATE, labeller = state_labels, scales = "free_y") +
        theme_bw() +
        xlab("Time after start of treatment (weeks)") +
        ylab("Proportion of patients (%)") +
        ggtitle("Proportions of patients being in each state") +
        theme(
          plot.title = element_text(size = 18, hjust = 0.5),       # Main title
          axis.title = element_text(size = 15),                    # Axis titles
          axis.text = element_text(size = 15),                     # Axis text
          legend.title = element_text(size = 14),                  # Legend title
          legend.text = element_text(size = 12),                   # Legend text
          strip.text = element_text(size = 15)
        ) +
        scale_x_continuous(breaks = seq(0, 120, by = 12))
    }
    return(plot)
  })





  ## DOWNLOAD SIMULATED DATAFRAME ####
  # Dataset of Simulated dataframe
  output$sim_dataQTDT <- DT::renderDataTable({
    sim_dataframeQT()
  })

  output$sim_dataDT <- DT::renderDataTable({
    sim_dataframePK()
  })


  # Downloadable csv of Sim dataframe
  output$download_simdata <- downloadHandler(
    filename = function() {
      "simdataframe.csv"
    },
    content = function(file) {
      write.csv(sim_dataframePK(), file, row.names = FALSE)
    }
  )

  # Dataset of Trough dataframe
  output$trough_dataDT <- DT::renderDataTable({
    sim_dataframe24()
  })

  # Downloadable csv of Trough dataframe
  output$download_troughdata <- downloadHandler(
    filename = function() {
      "simdataframe24.csv"
    },
    content = function(file) {
      write.csv(sim_dataframe24(), file, row.names = FALSE)
    }
  )


  # Dataset of Cavg-daily  dataframe
  output$cavgdaily_dataDT <- DT::renderDataTable({
    Cavg_daily()
  })

  # Downloadable csv of Sim dataframe
  output$download_cavgdaily <- downloadHandler(
    filename = function() {
      "Cavg_daily.csv"
    },
    content = function(file) {
      write.csv(Cavg_daily(), file, row.names = FALSE)
    }
  )

  # Dataset of Cavg weekly dataframe
  output$cavgweekly_dataDT <- DT::renderDataTable({
    Cavg_weekly()
  })

  # Downloadable csv of Sim dataframe
  output$download_cavgweekly <- downloadHandler(
    filename = function() {
      "Cavgweekly.csv"
    },
    content = function(file) {
      write.csv(Cavg_weekly(), file, row.names = FALSE)
    }
  )

  output$ui_script <- renderText({
    text <- readLines(rstudioapi::getSourceEditorContext()$path)

    # split the text into a list of character vectors
    #   Each element in the list contains one line
    splitText <- paste(str = text, collapse = "\t\n")

    # wrap a paragraph tag around each element in the list
    # replacedText <- lapply(splitText, p)

    return(splitText)
  })
}

shinyApp(ui = ui, server = server)
