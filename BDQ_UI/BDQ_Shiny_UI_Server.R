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
library(dipsaus)


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
    
    titlePanel("BEDAQUILINE DOSE REGIMEN"),

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
  ) # end of fluidPage

#### Define server logic ####
source(paste0(Server.directory, "BDQOMAT.R"))
source(paste0(Server.directory, "BDQQT.R"))
source(paste0(Server.directory, "BDQTTP.R"))
source(paste0(Server.directory, "BDQMSM.R"))
source(paste0(Server.directory, "BDQ_Server_Virtual_population.R"))

server <- function(input, output, session) {
  # PK simulation ####
  source(paste0(Server.directory, "BDQ_Server_PK.R"))

  # Use the function defined in the sourced file
  sim_PKtable <- eventReactive(input$goButton, {
    sim_PK(input)  # Call the function and pass input
  })
  
  # # Render table
  # output$sim_PKtable <- renderTable({
  #   # Call the reactive expression to get the data frame
  #   sim_PKtable()
  # })
  
  # Render combined PK plot
  source(paste0(Server.directory, "BDQ_Server_PKplots.R"))

  output$plot <- renderPlot({
    TypPK_plots(input, sim_PKtable)  # Call the function from the sourced file
  })


  # QT simulation ####
  source(paste0(Server.directory, "BDQ_Server_QT.R"))
  
  # Use the function defined in the sourced file
  sim_QTtable <- eventReactive(input$goButton, {
    sim_QT(input, sim_PKtable)  # Call the function and pass input
  })
  
  # # Render table
  # output$sim_QTtable <- renderTable({
  #   # Call the reactive expression to get the data frame
  #   head(sim_QTtable())
  # })
  
  # Render QT plot
  source(paste0(Server.directory, "BDQ_Server_QTplots.R"))
  
  output$plotQT <- renderPlot({
    QT_plots(input, sim_QTtable)  # Call the function from the sourced file
  })
  
  # TTP simulation ####
  source(paste0(Server.directory, "BDQ_Server_TTP.R"))
  
  # Use the function defined in the sourced file
  sim_TTPtable <- eventReactive(input$goButton, {
    sim_TTP(input, sim_PKtable)  # Call the function and pass input
  })
  
  # # Render table
  # output$sim_TTPtable <- renderTable({
  #   # Call the reactive expression to get the data frame
  #   head(sim_TTPtable() %>% group_by(ID) %>% slice(1L), 30)
  # })

  # Render TTP plot
  source(paste0(Server.directory, "BDQ_Server_TTPplots.R"))

  output$plotTTP <- renderPlot({
    TTP_plots(input, sim_TTPtable)  # Call the function from the sourced file
  })
  
  
  # MSM simulation ####
  source(paste0(Server.directory, "BDQ_Server_MSM.R"))
  
  # Use the function defined in the sourced file
  sim_MSMtable <- eventReactive(input$goButton, {
    sim_MSM(input, sim_TTPtable, sim_PKtable)  # Call the function and pass input
  })
  
  # # Render table
  # output$sim_MSMtable <- renderTable({
  #   # Call the reactive expression to get the data frame
  #   head(sim_MSMtable() %>% filter(evid == 0) %>% mutate(MBLendLog10 = log10(MBLend)), 100)
  # })
  #

  # Render MSM plot
  source(paste0(Server.directory, "BDQ_Server_MSMplots.R"))

  output$plotMSM <- renderPlot({
    MSM_plots(input, sim_MSMtable)  # Call the function from the sourced file
  })

  
  
  #### DATAFRAME FOR TROUGH CONC AT 24 HRS
  sim_dataframe24 <- eventReactive(input$goButton, {
    df <- sim_PKtable() %>% filter(AMT == 0)
    df <- df %>% filter(time %% 24 == 0)
    df$DAY <- df$time / 24
    return(df)
  })

  ####### Reactive MEAN DAILY CONCENTRATION
  Cavg_daily <- eventReactive(input$goButton, {
    d1 <- sim_PKtable() %>% filter(AMT == 0)

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
    d2 <- sim_PKtable() %>% filter(AMT == 0)

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

    if (input$ipred == 2) {
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
      dfForPlotWT <- sim_PKtable() %>%
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

      dfForPlotALB <- sim_PKtable() %>%
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
    } 
    return(plot)
  })





  ## DOWNLOAD SIMULATED DATAFRAME ####
  # Dataset of Simulated dataframe
  output$sim_dataQTDT <- DT::renderDataTable({
    sim_dataframeQT()
  })

  output$sim_dataDT <- DT::renderDataTable({
    sim_PKtable()
  })


  # Downloadable csv of Sim dataframe
  output$download_simdata <- downloadHandler(
    filename = function() {
      "simdataframe.csv"
    },
    content = function(file) {
      write.csv(sim_PKtable(), file, row.names = FALSE)
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
