# Title: BEDAQUILINE DOSE REGIMEN Shiny Application
# Author: Yu-Jou Lin
# Date: 2024-12-01

###################### SETUP ######################
# Load required libraries
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
library(gridExtra)
library(patchwork)
library(shinyjs)

# Set directory paths
UI.directory <- "//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_UI/"
Server.directory <- "//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/BDQ_Server/"

###################### SOURCE FILES ######################
# Source UI components
source(paste0(UI.directory, "BDQ_Shiny_UI_About.R"))
source(paste0(UI.directory, "BDQ_Shiny_UI_Dosing.R"))
source(paste0(UI.directory, "BDQ_Shiny_UI_Population.R"))
source(paste0(UI.directory, "BDQ_Shiny_UI_Simulation.R"))
source(paste0(UI.directory, "BDQ_Shiny_UI_Results.R"))

###################### UI DEFINITION ######################
ui <- fluidPage(
    useShinyjs(),
    
    # Add div for the waiting game (initially hidden)
    div(
      id = "waiting-game",
      style = "display: none;",
      img(id = "click-cat", src = "cat_closed.png", alt = "Clickable Cat"),
      div(id = "click-counter", "Clicks: 0")
    ),
    
    # CSS Styles
    tags$style(HTML("
        /* Go Button Styles */
        #goButton {
            background-color: #C1D4D78D;
            color: black;
            border: none;
            padding: 10px 20px;
            font-size: 16px;
            font-weight: bold;
            cursor: pointer;
        }
        
        #goButton:hover {
            background-color: #C1D4D76D;
        }
        
        #goButton:active {
            background-color: #C1D4D76D;
            transform: scale(0.98);
        }

        /* Reset Button Styles */
        .btn-sm:hover {
            background-color: #E9ECEF;
            color: #212529;
        }

        .btn-sm:active {
            background-color: #DDE2E6;
            transform: scale(0.98);
        }

        
    ")),
    
    # Application Title
    titlePanel("BEDAQUILINE DOSE REGIMEN"),

    # Main Tab Panel
    tabsetPanel(
        id = "mainTab",
        tabPanel("Dosing", value = "Dosing", mainTabDosing),
        tabPanel("Population", value = "Population", mainTabPopulation),
        tabPanel("Simulation", value = "Simulation", mainTabSim),
        tabPanel("Results", value = "Results", mainTabResults),
        tabPanel("About", value = "About", mainTabAbout)
    )
)

###################### SOURCE SERVER COMPONENTS ######################
# Source model files
source(paste0(Server.directory, "BDQOMAT.R"))
source(paste0(Server.directory, "BDQQT.R"))
source(paste0(Server.directory, "BDQTTP.R"))
source(paste0(Server.directory, "BDQTTP_TrtExperienced.R"))
source(paste0(Server.directory, "BDQMSM.R"))

# Source server functions
source(paste0(Server.directory, "BDQ_Server_DosingParse.R"))
source(paste0(Server.directory, "BDQ_Server_Virtual_population_TBPACTS.R"))
source(paste0(Server.directory, "BDQ_Server_PK.R"))
source(paste0(Server.directory, "BDQ_Server_QT.R"))
source(paste0(Server.directory, "BDQ_Server_TTP.R"))
source(paste0(Server.directory, "BDQ_Server_MSM.R"))
source(paste0(Server.directory, "BDQ_Server_MSM_individual.R"))

# Source plotting functions
source(paste0(Server.directory, "BDQ_Server_PKplots.R"))
source(paste0(Server.directory, "BDQ_Server_QTplots.R"))
source(paste0(Server.directory, "BDQ_Server_TTPplots.R"))
source(paste0(Server.directory, "BDQ_Server_MSMplots.R"))
source(paste0(Server.directory, "BDQ_Server_MSMplots_individual.R"))
source(paste0(Server.directory, "BDQ_Server_PKplots_additional.R"))

# Source summary functions
source(paste0(Server.directory, "BDQ_Server_Population_summary.R"))
source(paste0(Server.directory, "BDQ_Server_Population_summary_plots.R"))

###################### SERVER LOGIC ######################
server <- function(input, output, session) {
    # Initialize reactive values
    stored_regimens <- reactiveVal(c(TRUE, FALSE, FALSE))
    loading <- reactiveVal(FALSE)

    # Initialize plot outputs
    output$plot <- renderPlot({ NULL })
    output$plotQT <- renderPlot({ NULL })
    output$plotTTP <- renderPlot({ NULL })
    output$plotMSM <- renderPlot({ NULL })

    ###################### RESET BUTTON OBSERVERS ######################
    # Reset buttons for each parameter
    observeEvent(input$reset_AGE, {
        updateNumericInput(session, "AGE_min", value = 17)
        updateNumericInput(session, "AGE_max", value = 69)
    })

      observeEvent(input$reset_WT, {
    updateNumericInput(session, "WT_min", value = 29)
    updateNumericInput(session, "WT_max", value = 112)
    })

    observeEvent(input$reset_ALB, {
      updateNumericInput(session, "ALB_min", value = 1.9)
      updateNumericInput(session, "ALB_max", value = 5.0)
    })

    observeEvent(input$reset_CACOR, {
      updateNumericInput(session, "CACOR_min", value = 2.00)
      updateNumericInput(session, "CACOR_max", value = 3.08)
    })

    observeEvent(input$reset_K, {
      updateNumericInput(session, "K_min", value = 3.20)
      updateNumericInput(session, "K_max", value = 6.26)
    })

    observeEvent(input$reset_MTTP, {
      updateNumericInput(session, "MTTP_min", value = 1.3)
      updateNumericInput(session, "MTTP_max", value = 42)
    })

    observeEvent(input$reset_SEX, {
      updateNumericInput(session, "SEX_female", value = 33)
  })

    ###################### LOADING OVERLAY STYLES ######################
    # Add custom CSS for loading overlay and notifications
    loading_css <- tags$head(
        tags$style(HTML("
            /* Loading Overlay */
            #loading-overlay {
                position: fixed;
                top: 0;
                left: 0;
                width: 100%;
                height: 100%;
                background-color: rgba(255, 255, 255, 0.9);
                z-index: 999;
                display: flex;
                justify-content: center;
                align-items: center;
            }

            /* Progress Bar Styles */
            .shiny-progress-container {
                position: fixed !important;
                top: 50% !important;
                left: 50% !important;
                transform: translate(-50%, -50%);
                width: 600px;
                height: 120px;
                z-index: 9999 !important;
                display: flex !important;
                flex-direction: column !important;
                justify-content: center !important;
                align-items: center !important;
            }

            /* Notification Styles */
            .shiny-notification {
                position: fixed !important;
                top: 50% !important;
                left: 50% !important;
                transform: translate(-50%, -50%) !important;
                text-align: center;
                width: auto !important;
                max-width: 600px;
                height: 120px;
                font-size: 20px !important;
                padding: 20px 30px !important;
                display: flex !important;
                justify-content: center !important;
                align-items: center !important;
            }
            
            #waiting-game {
              position: fixed;
              top: 90%;
              left: 95%;
              transform: translate(-50%, -50%);
              z-index: 1000;
              text-align: center;
              background: white;
              padding: 20px;
              border-radius: 10px;
              box-shadow: 0 0 10px rgba(0,0,0,0.2);
            }
            
            #click-cat {
              cursor: pointer;
              transition: transform 0.1s;
              width: 100px;
              height: 100px;
            }
            
            #click-cat:active {
              transform: scale(0.9);
            }
            
            #click-counter {
              font-size: 20px;
              margin-top: 10px;
              font-weight: bold;
            }
            
        ")),
        tags$script(HTML("
            let counter = 0;
            
            function resetCounter() {
                counter = 0;
                document.getElementById('click-counter').innerHTML = 'Clicks: 0';
            }
            
            $(document).on('click', '#click-cat', function() {
                counter++;
                document.getElementById('click-counter').innerHTML = 'Clicks: ' + counter;
                
                // Change image temporarily
                this.src = 'cat_open.png';
                setTimeout(() => {
                    this.src = 'cat_closed.png';
                }, 100);
            });
        "))
    )

    # Insert CSS/JavaScript once at startup
    insertUI(selector = "head", where = "beforeEnd", ui = loading_css, immediate = TRUE)

    ###################### SIMULATION HANDLER ######################
    # Main simulation observer
    observeEvent(input$goButton, {
    # Show the game
    shinyjs::show("waiting-game")
    
    # Use runjs to directly trigger click on Results tab
    shinyjs::runjs("$('a[data-value=\"Results\"]').tab('show');")
    
    # Show loading overlay
    loading(TRUE)
    
    # Insert loading overlay
    insertUI(
      selector = "body",
      where = "beforeEnd",
      ui = div(
        id = "loading-overlay"
      ),
      immediate = TRUE
    )
  
      withProgress(message = 'Processing All Data', value = 0, {
        # Store current regimen state when Go button is clicked
        stored_regimens(c(TRUE, input$RG2, input$RG3))
  
        # Update dosing regimen boxes first
        output$regimen_boxes <- renderUI({
          create_regimen_boxes(input, stored_regimens())
        })
        
        # Sampling virutal population 
        incProgress(0.12, detail = "Sampling virutal population...")
        if (input$population_radio == "Population") {
        virtual_population_df <- Pop_generation(input)
        }
        
        # Summary of individual or virtual population
        if (input$population_radio == "Individual") {
          PopSummary_plot <- create_population_plots(NULL, input)
          PopSummary_df <- Virtual_population_summary(NULL, input)
        } else {
          PopSummary_plot <- create_population_plots(virtual_population_df, input)
          PopSummary_df <- Virtual_population_summary(virtual_population_df, input)
        }
        
        output$PopSummaryTable <- renderTable({
          PopSummary_df
          names(PopSummary_df)[2] <- "Median (Range) or N (%)"
          PopSummary_df
        }, sanitize.text.function = function(x) x)
        
        output$PopSummaryPlot <- renderPlot({
          PopSummary_plot
        })
        
        
  
        # PK simulation
        incProgress(0.12, detail = "Running PK simulation...")
        sim_PKtable <- sim_PK(input, virtual_population_df)
        
        # QT simulation
        incProgress(0.12, detail = "Running QT simulation...")
        sim_QTtable <- sim_QT(input, sim_PKtable)
        
        # TTP simulation
        incProgress(0.12, detail = "Running TTP simulation...")
        sim_TTPtable <- sim_TTP(input, sim_PKtable, virtual_population_df)
        
        # MSM simulation
        incProgress(0.12, detail = "Running MSM simulation...")
        if (input$population_radio == "Individual") {
            sim_MSMtable <- sim_MSMidv(input, sim_TTPtable, sim_PKtable)
          } else {
            sim_MSMtable <- sim_MSM(input, sim_TTPtable, sim_PKtable)
          }
  
        # Generate PK plots
        incProgress(0.12, detail = "Generating PK plots...")
        TypPKplot <- TypPK_plots(input, sim_PKtable)  # Call the function from the sourced file
        output$plot <- renderPlot({
          TypPKplot
        })
        
        # Generate QT plots
        incProgress(0.12, detail = "Generating QT plots...")
        QTplot <- QT_plots(input, sim_QTtable)  # Call the function from the sourced file
        output$plotQT <- renderPlot({
          QTplot
        })
        
        # Generate TTP plots
        incProgress(0.12, detail = "Generating TTP plots...")
        TTPplot <- TTP_plots(input, sim_TTPtable)  # Call the function from the sourced file      
        output$plotTTP <- renderPlot({
          TTPplot
        })
  
        # Generate MSM plots
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
        
        # Show a centered success message
        showNotification(
          "All plots are generated successfully!", 
          type = "message", 
          duration = 3
        )
  
        # After all simulations complete
        shinyjs::hide("waiting-game")
        shinyjs::runjs("resetCounter()")  # Reset the counter for next time
        
        removeUI(selector = "#loading-overlay", immediate = TRUE)
        loading(FALSE)
        
        # After all simulations complete
        shinyjs::hide("waiting-game")
        shinyjs::runjs("resetCounter()")  # Reset the counter for next time
      })
    })

    ###################### DOWNLOAD HANDLERS ######################
    # Template download handler
    output$download_template <- downloadHandler(
        filename = function() {
            "Virtual_population_template.csv"
        },
        content = function(file) {
            template_path <- "//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/Virtual_population/TBPACTS/Virtual_population_template.csv"
            if (!file.exists(template_path)) {
                stop("Template file not found. Please contact the administrator.")
            }
            file.copy(from = template_path, to = file)
        }
    )

    ###################### FILE UPLOAD HANDLER ######################
    observeEvent(input$uploaded_data, {
        if (is.null(input$uploaded_data)) return(NULL)
        
        tryCatch({
            df <- read.csv(input$uploaded_data$datapath)
            required_cols <- c("AGE", "WT", "ALB", "CACOR", "K", "MTTP", "SEX", "RACE", "TBTYPE")
            missing_cols <- setdiff(required_cols, names(df))
            
            if (length(missing_cols) > 0) {
                showNotification(
                    paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
                    type = "error",
                    duration = 4
                )
            } else {
                showNotification("File uploaded successfully!", type = "message", duration = 2)
            }
        }, error = function(e) {
            showNotification("Error reading file. Please ensure it's a valid CSV file.",
                           type = "error", duration = 4)
        })
    })

    ###################### OUTPUT OPTIONS ######################
    outputOptions(output, "plot", suspendWhenHidden = FALSE)
    outputOptions(output, "plotQT", suspendWhenHidden = FALSE)
    outputOptions(output, "plotTTP", suspendWhenHidden = FALSE)
    outputOptions(output, "plotMSM", suspendWhenHidden = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
