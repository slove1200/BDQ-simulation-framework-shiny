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
library(stringr)
library(bsicons)

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
    
    /* Default button style */
    #goButton {
      background-color: #C1D4D78D; /* Primary gray color */
      color: black;
      border: none;
      padding: 10px 20px;
      font-size: 16px;
      font-weight: bold;
      cursor: pointer;
    }
    
    /* Hover state */
    #goButton:hover {
      background-color: #C1D4D76D;
    }
    
    /* Active (clicked) state */
    #goButton:active {
      background-color: #C1D4D76D;
      transform: scale(0.98); /* Slightly shrink on click */
    }
                    ")),
    
    titlePanel("BEDAQUILINE DOSE REGIMEN"),

      tabsetPanel(
        id = "mainTab",  # Set id for main tabsetPanel
        
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
source(paste0(Server.directory, "BDQTTP_TrtExperienced.R"))
source(paste0(Server.directory, "BDQMSM.R"))
source(paste0(Server.directory, "BDQ_Server_Virtual_population.R"))
source(paste0(Server.directory, "BDQ_Server_DosingParse.R"))
source(paste0(Server.directory, "BDQ_Server_PK.R"))
source(paste0(Server.directory, "BDQ_Server_QT.R"))
source(paste0(Server.directory, "BDQ_Server_TTP.R"))
source(paste0(Server.directory, "BDQ_Server_MSM.R"))
source(paste0(Server.directory, "BDQ_Server_MSM_individual.R"))
source(paste0(Server.directory, "BDQ_Server_PKplots.R"))
source(paste0(Server.directory, "BDQ_Server_QTplots.R"))
source(paste0(Server.directory, "BDQ_Server_TTPplots.R"))
source(paste0(Server.directory, "BDQ_Server_MSMplots.R"))
source(paste0(Server.directory, "BDQ_Server_MSMplots_individual.R"))
source(paste0(Server.directory, "BDQ_Server_PKplots_additional.R"))


server <- function(input, output, session) {
  
  # Predefine all outputs to avoid dynamic creation errors
  output$plot <- renderPlot({ NULL })     # Placeholder
  output$plotQT <- renderPlot({ NULL })   # Placeholder
  output$plotTTP <- renderPlot({ NULL })  # Placeholder
  output$plotMSM <- renderPlot({ NULL })  # Placeholder
  
  # Dosing details ####
  source(paste0(Server.directory, "BDQ_Server_DosingParse.R"))

  # Create dynamic value boxes
  output$regimen_boxes <- renderUI({
    reg_strings <- regimen_strings(input)
    validate(need(!is.null(reg_strings), "No regimen data available"))
    
    # Create a list of value boxes - one for each regimen
    vbs <- lapply(seq_along(reg_strings), function(i) {
      value_box(
        title = tags$p(paste("Regimen", i), style = "font-size: 140%; font-weight: bold;"),
        value = tags$p(reg_strings[[i]], style = "font-size: 110%;"),
        showcase = bs_icon(bsicon_themes[[i]]),
        theme = regimen_themes[[i]]  # Use custom theme from our list
      )
    })
    
    # Wrap the value boxes in a layout with appropriate width
    layout_column_wrap(
      width = "300px",
      !!!vbs
    )
  })
  
  # PK simulation ####
  # # Render PK table
  # output$sim_PKtable <- renderTable({
  #   # Call the reactive expression to get the data frame
  #   sim_PKtable()
  # })

  # QT simulation ####
    # # Render QT table
  # output$sim_QTtable <- renderTable({
  #   # Call the reactive expression to get the data frame
  #   head(sim_QTtable())
  # })
  
  # TTP simulation ####
  # # Render TTP table
  # output$sim_TTPtable <- renderTable({
  #   # Call the reactive expression to get the data frame
  #   head(sim_TTPtable(), 30)
  # })

  # MSM simulation ####
  # # Render MSM table
  # output$sim_MSMtable <- renderTable({
  #   # Call the reactive expression to get the data frame
  #   head(sim_MSMtable(), 30)
  # })

  
  # Add an observer to coordinate all simulations
  observeEvent(input$goButton, {
    withProgress(message = 'Processing All Data', value = 0, {
      incProgress(0.12, detail = "Running PK simulation...")
      sim_PKtable <- sim_PK(input)
      
      incProgress(0.12, detail = "Running QT simulation...")
      sim_QTtable <- sim_QT(input, sim_PKtable)
      
      incProgress(0.12, detail = "Running TTP simulation...")
      sim_TTPtable <- sim_TTP(input, sim_PKtable)
      
      incProgress(0.12, detail = "Running MSM simulation...")
      if (input$population_radio == "Individual") {
          sim_MSMtable <- sim_MSMidv(input, sim_TTPtable, sim_PKtable)
        } else {
          sim_MSMtable <- sim_MSM(input, sim_TTPtable, sim_PKtable)
        }

      incProgress(0.12, detail = "Generating PK plots...")
      TypPKplot <- TypPK_plots(input, sim_PKtable)  # Call the function from the sourced file
      output$plot <- renderPlot({
        TypPKplot
      })
      
      incProgress(0.12, detail = "Generating QT plots...")
      QTplot <- QT_plots(input, sim_QTtable)  # Call the function from the sourced file
      output$plotQT <- renderPlot({
        QTplot
      })
      
      incProgress(0.12, detail = "Generating TTP plots...")
      TTPplot <- TTP_plots(input, sim_TTPtable)  # Call the function from the sourced file      
      output$plotTTP <- renderPlot({
        TTPplot
      })

      incProgress(0.12, detail = "Generating MSM plots...")
      if (input$population_radio == "Individual") {
        MSMidvplot <- MSMidv_plots(input, sim_MSMtable)
        output$plotMSM <- renderPlot({
          MSMidvplot
        })
        } else {
          MSMplot <- MSM_plots(input, sim_MSMtable)  # Call the function from the sourced file      
          output$plotMSM <- renderPlot({
            MSMplot
          })
        }
      
      # PK-additional simulation ####
      # Render combined PK-additional plot
      output$plotPKDavg <- renderPlot({
        withProgress(message = "Generating PK Daily Average plot...", value = NULL, {
          PKDavg_plots(input, sim_PKtable)  # Call the function from the sourced file
        })
      })
      
      output$plotPKWavg <- renderPlot({
        withProgress(message = "Generating PK Weekly Average plot...", value = NULL, {
          PKWavg_plots(input, sim_PKtable)  # Call the function from the sourced file
        })
      })
      
      # Show a general success message as a notification
      showNotification("All plots are generated successfully!", type = "message", duration = 3)
    })
  })

  ########################################################
  ##### RENDER IMAGE ####
    # } else if (input$ipred == 5) {
    #   dfForPlotWT <- sim_PKtable() %>%
    #     ungroup() %>%
    #     group_by(time) %>%
    #     summarize(
    #       lower = quantile(IPREDWT, probs = 0.05),
    #       median = quantile(IPREDWT, probs = 0.5),
    #       upper = quantile(IPREDWT, probs = 0.95)
    #     )
    # 
    #   e1 <- ggplot(dfForPlotWT, aes(x = time / 168, y = median)) +
    #     geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
    #     geom_line(size = 1.2) +
    #     theme_bw() +
    #     theme(text = element_text(size = 15)) +
    #     theme(axis.text = element_text(size = 15)) +
    #     scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 25)) +
    #     # scale_y_continuous(breaks = seq(0, 4, 0.5),limits = c(0,2.5)) +
    #     labs(x = "Time (weeks)", y = ("Body weight (kg)")) +
    #     ggtitle("Body Weight (kg) vs Time (weeks)")
    # 
    #   dfForPlotALB <- sim_PKtable() %>%
    #     ungroup() %>%
    #     group_by(time) %>%
    #     summarize(
    #       lower = quantile(IPREDALB, probs = 0.05),
    #       median = quantile(IPREDALB, probs = 0.5),
    #       upper = quantile(IPREDALB, probs = 0.95)
    #     )
    # 
    #   e2 <- ggplot(dfForPlotALB, aes(x = time / 168, y = median)) +
    #     geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
    #     geom_line(size = 1.2) +
    #     theme_bw() +
    #     theme(text = element_text(size = 15)) +
    #     theme(axis.text = element_text(size = 15)) +
    #     scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 25)) +
    #     labs(x = "Time (weeks)", y = ("Albumin concentration (g/dL)")) +
    #     ggtitle("Albumin Concentration (g/dL) vs Time(weeks)")
    # 
    #   plot <- grid.arrange(e1, e2, nrow = 2)

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
  
  outputOptions(output, "plot", suspendWhenHidden = FALSE)
  outputOptions(output, "plotQT", suspendWhenHidden = FALSE)
  outputOptions(output, "plotTTP", suspendWhenHidden = FALSE)
  outputOptions(output, "plotMSM", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)
