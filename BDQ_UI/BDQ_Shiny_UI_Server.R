# Title: BEDAQUILINE DOSE REGIMEN Shiny Application
# Author: Yu-Jou Lin
# Date: 2024-12-01

###################### SETUP ######################
# Load required libraries
library(mrgsolve)
library(dplyr)
library(tidyr)
library(purrr)
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
library(jquerylib)
library(survival)

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
source(paste0(UI.directory, "BDQ_Shiny_UI_TTPsim.R"))

###################### UI DEFINITION ######################
ui <- fluidPage(
    useShinyjs(),
    
    # CSS Styles
    tags$style(HTML("
        /* Title Styles */
        .title-panel {
            background: linear-gradient(to right, #CDD8DA, #C1D4D7);
            padding: 15px 25px;
            margin-bottom: 35px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }

        .title-panel h2 {
            color: #333333;
            margin: 0;
            font-weight: 600;
            font-size: 28px;
            text-align: center;
            letter-spacing: 0.5px;
            text-shadow: 1px 1px 2px rgba(255, 255, 255, 0.8);
        }

        /* Main Tab Panel Styles */
        #mainTab {
            margin-bottom: 10px;
        }

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

        /* Main navigation tabs style */
        #mainTab > li > a {
            color: #333333 !important;
        }

        #mainTab > li > a.active {
            color: #333333 !important;  
            font-weight: bold !important;
        }

        /* Radio buttons and checkboxes base style */
        input[type='radio'],
        input[type='checkbox'] {
            cursor: pointer;
        }

        /* Custom styles for medication radio buttons - more specific selectors */
        input[type='radio'][id^='IE_1'] {
            background-color: #CBCAE3 !important;
            border-color: #ABAAC3 !important;
        }
        input[type='radio'][id^='IE_1']:hover {
            border-color: #ABAAC3 !important;
        }

        input[type='radio'][id^='IE_2'] {
            background-color: #E1C3C8 !important;
            border-color: #C1A3A8 !important;
        }
        input[type='radio'][id^='IE_2']:hover {
            border-color: #C1A3A8 !important;
        }

        input[type='radio'][id^='IE_3'] {
            background-color: #C1D4D7 !important;
            border-color: #A1B4B7 !important;
        }
        input[type='radio'][id^='IE_3']:hover {
            border-color: #A1B4B7 !important;
        }

        input[type='radio'][id^='IE_4'] {
            background-color: #E7D7CB !important;
            border-color: #C7B7AB !important;
        }
        input[type='radio'][id^='IE_4']:hover {
            border-color: #C7B7AB !important;
        }

        /* Radio buttons and checkboxes style */
        input[type='radio']:checked,
        input[type='checkbox']:checked {
            background-color: #809890 !important;
            border-color: #809890 !important;
        }

        /* Radio buttons hover effect */
        input[type='radio']:not(:disabled):not(:checked):hover,
        input[type='checkbox']:not(:disabled):not(:checked):hover {
            border-color: #809890 !important;
        }

        /* Focus state for form controls */
        input[type='radio']:focus,
        input[type='checkbox']:focus {
            border-color: #809890 !important;
            box-shadow: 0 0 0 0.25rem #99BAB08D !important;
        }

        /* Style for subset pills */
        .nav-pills {
            font-size: 14px !important;
        }

        .nav-pills .nav-link {
            padding: 0.25rem 1rem !important;
            color: #333333 !important;
            margin-right: 10px !important;
        }

        .nav-pills .nav-link.active {
            background-color: #555555 !important;
            color: white !important;
        }

        /* Custom styles for Add Loading Dose checkboxes */
        #LD1:checked {
            background-color: #65658F !important;
            border-color: #E5E5F1 !important;
        }
        #LD1:not(:checked):hover {
            border-color: #E5E5F1 !important;
        }
        #LD1:focus {
            box-shadow: 0 0 0 0.25rem rgba(101, 101, 143, 0.25) !important;
        }

        #LD2:checked {
            background-color: #705E64 !important;
            border-color: #F0E1E4 !important;
        }
        #LD2:not(:checked):hover {
            border-color: #F0E1E4 !important;
        }
        #LD2:focus {
            box-shadow: 0 0 0 0.25rem rgba(112, 94, 100, 0.25) !important;
        }

        #LD3:checked {
            background-color: #606A6C !important;
            border-color: #E0EAEB !important;
        }
        #LD3:not(:checked):hover {
            border-color: #E0EAEB !important;
        }
        #LD3:focus {
            box-shadow: 0 0 0 0.25rem rgba(96, 106, 108, 0.25) !important;
        }

        /* Override min-height for html-fill-item */
        .html-fill-container > .html-fill-item {
          min-height: initial !important;
        }
    ")),
    
    # Application Title with custom div
    div(class = "title-panel",
        h2("Bedaquiline Dose-PK-Efficacy/Safety-Outcome Simulation Framework")
    ),
    
    # Main Tab Panel
    tabsetPanel(
      id = "mainTab", selected = "About",
      tabPanel("Dosing", value = "Dosing", mainTabDosing),
      tabPanel("Population", value = "Population", mainTabPopulation),
      tabPanel("Simulation", value = "Simulation", mainTabSim),
      tabPanel("Results", value = "Results", mainTabResults),
      tabPanel("About", value = "About", mainTabAbout), 
      tabPanel("Extra: TTP Simulation", value = "TTPsim", mainTabTTPSim)
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
source(paste0(Server.directory, "BDQ_Server_PK_same_indv.R"))
source(paste0(Server.directory, "BDQ_Server_QT_same_indv.R"))
source(paste0(Server.directory, "BDQ_Server_TTP_same_indv.R"))
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

# Source TTP simulation functions
source(paste0(Server.directory, "TTPsim.R"))
source(paste0(Server.directory, "TTP_TRTExperiencedsim.R"))
source(paste0(Server.directory, "Server_TTP_simulation.R"))

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
    output$plotTTPsim <- renderPlot({ NULL })

    # Create a reactive value to store TTPsimplots results
    TTPsim_results <- reactiveVal()

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
    insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = tags$style(HTML("
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
        ")),
        immediate = TRUE
    )

    ###################### SIMULATION HANDLER ######################
    # Main TTP tab simulation observer
    observeEvent(input$goButton_TTP, {
      # Run simulation once and store results
      results <- TTPsimplots(input)
      TTPsim_results(results)
      
      # Use stored results for table
      output$tableTTPsim <- DT::renderDataTable({
        TTPsim_results()$TSCCdf %>% 
          select(ID, TAST, REP, TTPD, NEG) %>%
          DT::datatable(
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              scrollY = "100%",
              dom = 'Brtip',
              searching = FALSE,
              ordering = TRUE
            ),
            rownames = FALSE,
            colnames = c("ID", "Time", "Replicates", "Time to Positivity in Days", "Culture Negative (1)"),
            class = 'cell-border stripe'
          )
      })
      
      # Only render plot if in weekly mode
      if (input$simunit_TTP == "2") {
        output$plotTTPsim <- renderPlot({
          TTPsim_results()$plotTTPsim
        })
      }
    })
    
    # Main simulation observer
    observeEvent(input$goButton, {
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
        
        # Sampling virtual population 
        incProgress(0.12, detail = "Sampling virtual population...")
        if (input$population_radio == "Population") {
            if (input$dataset_source == "Default") {
                virtual_population_df <- Pop_generation(input)
            } else if (input$dataset_source == "Import") {
                virtual_population_df <- read.csv(input$uploaded_data$datapath, header = TRUE, sep = ",") %>% 
                    filter(row_number() <= input$nsim)
            }
        } else {  # Individual mode
            # Create a single-row dataframe with the individual's parameters
            virtual_population_df <- data.frame(
                ID = 1,
                AGE = as.numeric(input$AGE),
                WT = as.numeric(input$WT),
                ALB = as.numeric(input$ALB),
                CACOR = as.numeric(input$CACOR),
                K = as.numeric(input$K),
                MTTP = as.numeric(input$MTTP) * 24,  # Convert to hours
                SEX = ifelse(input$SEX == "Male", 0, 1),
                RACE = ifelse(input$RACE == "Non-Black", 0, 1),
                stringsAsFactors = FALSE
            ) %>% 
            # Ensure we have at least one row
            slice(1L)
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
        if (input$population_radio == "Individual" | input$nsim == 1) {
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
  
        # After all simulations complete, remove the overlay
        removeUI(selector = "#loading-overlay", immediate = TRUE)
        loading(FALSE)

        # Set up download handlers after data generation
        output$download_virtual_population <- downloadHandler(
            filename = function() {
                "Population_data_package.zip"
            },
            content = function(file) {
                temp_dir <- tempdir()
                temp_files <- c(
                    file.path(temp_dir, "virtual_individual_or_population.csv"),
                    file.path(temp_dir, "virtual_individual_or_population_specification.txt")
                )
                
                write.csv(virtual_population_df %>% 
                            mutate(ALB   = round(ALB, 1), 
                                   CACOR = round(CACOR, 2), 
                                   K     = round(K, 1), 
                                   WT    = round(WT, 1)), 
                         temp_files[1], 
                         row.names = FALSE)
                
                file.copy(
                    from = file.path(Server.directory, "virtual_individual_or_population_specification.txt"),
                    to = temp_files[2]
                )
                
                zip(file, files = temp_files, flags = "-j")
                unlink(temp_files)
            },
            contentType = "application/zip"
        )

        output$download_simPK <- downloadHandler(
            filename = function() {
                "PK_data_package.zip"
            },
            content = function(file) {
                # Create a temporary directory
                temp_dir <- tempdir()
                temp_files <- c(
                    file.path(temp_dir, "PK_output.csv"),
                    file.path(temp_dir, "PK_specification.txt")
                )
                
                # Write CSV file
                write.csv(sim_PKtable %>%
                            mutate(
                              IPRED      = round(exp(IPRED)*1000, 2), 
                              IPREDM2    = round(exp(IPREDM2)*1000, 2),
                              IPREDALB   = round(IPREDALB, 2), 
                              IPREDWT    = round(IPREDWT,  2), 
                              AAUCBDQ    = round(AAUCBDQ*1000, 2), 
                              AAUCM2     = round(AAUCM2*1000, 2)
                            ) %>%
                            rename(
                              "CONCBDQ"  = "IPRED", 
                              "CONCM2"   = "IPREDM2",
                              "AUCBDQ"   = "AAUCBDQ", 
                              "AUCM2"    = "AAUCM2"
                            ) %>%
                            select(-AMT, -WT, -ALB, -SEX, -CACOR, -K), 
                         temp_files[1], 
                         row.names = FALSE)
                
                # Copy specification file
                file.copy(
                    from = file.path(Server.directory, "PK_specification.txt"),
                    to = temp_files[2]
                )
                
                # Create zip file
                zip(file, files = temp_files, flags = "-j")  # -j flag removes path information
                
                # Clean up temporary files
                unlink(temp_files)
            },
            contentType = "application/zip"
        )

        output$download_simQT <- downloadHandler(
            filename = function() {
                "QT_data_package.zip"
            },
            content = function(file) {
                temp_dir <- tempdir()
                temp_files <- c(
                    file.path(temp_dir, "QT_output.csv"),
                    file.path(temp_dir, "QT_specification.txt")
                )
                
                write.csv(sim_QTtable %>%
                            mutate(
                                CACOR = round(CACOR, 2), 
                                K     = round(K, 1), 
                                IPRED = round(IPRED, 2)
                            ) %>%
                            rename("QTcF"  = "IPRED"),
                         temp_files[1], 
                         row.names = FALSE)
                
                file.copy(
                    from = file.path(Server.directory, "QT_specification.txt"),
                    to = temp_files[2]
                )
                
                zip(file, files = temp_files, flags = "-j")
                unlink(temp_files)
            },
            contentType = "application/zip"
        )

        output$download_simTTP <- downloadHandler(
            filename = function() {
                "TTP_data_package.zip"
            },
            content = function(file) {
                temp_dir <- tempdir()
                temp_files <- c(
                    file.path(temp_dir, "TTP_output.csv"),
                    file.path(temp_dir, "TTP_specification.txt")
                )
                
                write.csv(sim_TTPtable %>%
                            mutate(
                                HL      = round(HL, 2)
                            ) %>%
                            select(-TIME, -MBL, -Y) %>%
                            rename("WEEK" = "WEEKP", 
                                   "TTPpos"  = "RTTE", 
                                   "CULneg"  = "NEG"
                            ),
                         temp_files[1], 
                         row.names = FALSE)
                
                file.copy(
                    from = file.path(Server.directory, "TTP_specification.txt"),
                    to = temp_files[2]
                )
                
                zip(file, files = temp_files, flags = "-j")
                unlink(temp_files)
            },
            contentType = "application/zip"
        )

        output$download_simMSM <- downloadHandler(
            filename = function() {
                "MSM_data_package.zip"
            },
            content = function(file) {
                temp_dir <- tempdir()
                temp_files <- c(
                    file.path(temp_dir, "longTermOutcome_output.csv"),
                    file.path(temp_dir, "longTermOutcome_specification.txt")
                )
                
                write.csv(sim_MSMtable %>%
                            mutate(
                                HL2      = round(HL2, 2),
                                Log10MBLend = round(Log10MBLend, 2), 
                                P_1      = round(P_1, 3),
                                P_2      = round(P_2, 3),
                                P_3      = round(P_3, 5),
                                P_5      = round(P_5, 5)
                            ) %>%
                            rename("WEEK" = "time") %>%
                            select(-regimen, everything(), regimen) %>%
                            group_by(ID, WEEK) %>%
                            slice(1L),
                         temp_files[1], 
                         row.names = FALSE)
                
                file.copy(
                    from = file.path(Server.directory, "longTermOutcome_specification.txt"),
                    to = temp_files[2]
                )
                
                zip(file, files = temp_files, flags = "-j")
                unlink(temp_files)
            },
            contentType = "application/zip"
        )
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
            required_cols <- c("AGE", "WT", "ALB", "CACOR", "K", "MTTP", "SEX", "RACE")
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
    
    ###################### WARNING IF POPULATION NUMBER < NUMBERS FOR SIMULATION #######
    # Create a reactive value to track validation states
    validationStates <- reactiveValues(
        enough_subjects = TRUE,
        enough_time = TRUE,
        enough_timeMSM = TRUE
    )

    # Population size validation
    output$nsim_validation <- renderText({
      # Check if nsim is missing or invalid
      if (is.null(input$nsim) || is.na(input$nsim)) {
        validationStates$enough_subjects <- FALSE
        return("Please specify the number of individuals for simulation")
      }
      
      # Only proceed if in Population mode
      if (input$population_radio != "Population") {
        validationStates$enough_subjects <- TRUE
        return(NULL)
      }
      
      # Get and filter the virtual population based on data source
      filtered_data <- if (input$dataset_source == "Default") {
        tryCatch({
          read.csv("//argos.storage.uu.se/MyFolder$/yujli183/PMxLab/Projects/BDQ shiny app optimization framework/ModelCodes/Virtual_population/TBPACTS/TBPACTS_Big_Virtual_Population_SimulatedforUse.csv", 
                   header = T) %>% 
            filter(
              SEX %in% c(0, 1),
              AGE >= input$AGE_min & AGE <= input$AGE_max &
              WT >= input$WT_min & WT <= input$WT_max &
              ALB >= input$ALB_min & ALB <= input$ALB_max &
              CACOR >= input$CACOR_min & CACOR <= input$CACOR_max &
              K >= input$K_min & K <= input$K_max &
              MTTP >= input$MTTP_min * 24 & MTTP <= input$MTTP_max * 24
            )
        }, error = function(e) {
          validationStates$enough_subjects <- FALSE
          return(data.frame())
        })
      } else {
        # Use uploaded data
        if (is.null(input$uploaded_data)) {
          validationStates$enough_subjects <- FALSE
          return("Please upload a data file")
        }
        tryCatch({
          read.csv(input$uploaded_data$datapath, header = TRUE, sep = ",")
        }, error = function(e) {
          validationStates$enough_subjects <- FALSE
          return(data.frame())
        })
      }
      
      if (nrow(filtered_data) == 0) {
        validationStates$enough_subjects <- FALSE
        return("No data available with the current filtering criteria")
      }
      
      required_n <- input$nsim
      available_n <- nrow(filtered_data)
      
      # Update validation state
      validationStates$enough_subjects <- required_n <= available_n
      
      # Return warning message if needed
      if (!validationStates$enough_subjects) {
        sprintf("The number of population (%d) is lower than the number of individuals intended for simulation (%d)W", 
                available_n, required_n, available_n)
      } else {
        NULL
      }
    })

    # Simulation time validation
    observe({
        # Check if simtime or simtimeMSM is missing or invalid
        if (is.null(input$simtime) || is.na(input$simtime) || 
            is.null(input$simtimeMSM) || is.na(input$simtimeMSM)) {
            validationStates$enough_time <- FALSE
            validationStates$enough_timeMSM <- FALSE
            output$simtime_validation <- renderText({ NULL })
            output$simtimeMSM_validation <- renderText({ NULL })
            return()
        }
        
        max_dur <- {
            durations <- (if(input$LD1) input$ldur_1 * ifelse(input$lunit_1 == "2", 1, 1/7) else 0) + 
                        (input$mdur_1 * ifelse(input$munit_1 == "2", 1, 1/7))
            
            if(input$RG2) {
                reg2_dur <- (if(input$LD2) input$ldur_2 * ifelse(input$lunit_2 == "2", 1, 1/7) else 0) + 
                           (input$mdur_2 * ifelse(input$munit_2 == "2", 1, 1/7))
                durations <- c(durations, reg2_dur)
            }
            
            if(input$RG3) {
                reg3_dur <- (if(input$LD3) input$ldur_3 * ifelse(input$lunit_3 == "2", 1, 1/7) else 0) + 
                           (input$mdur_3 * ifelse(input$munit_3 == "2", 1, 1/7))
                durations <- c(durations, reg3_dur)
            }
            
            max(durations, na.rm = TRUE)
        }
        
        # Update validation states
        validationStates$enough_time <- input$simtime >= max_dur
        validationStates$enough_timeMSM <- input$simtimeMSM >= max_dur
        
        output$simtime_validation <- renderText({
            if (!validationStates$enough_time) {
                paste0("Warning: Simulation time (", input$simtime, 
                      " weeks) is less than the maximum dosing duration (", 
                      round(max_dur, 1), " weeks).")
            } else {
                NULL
            }
        })

        output$simtimeMSM_validation <- renderText({
            if (!validationStates$enough_timeMSM) {
                paste0("Warning: Long-term outcome simulation time (", input$simtimeMSM, 
                      " weeks) is less than the maximum dosing duration (", 
                      round(max_dur, 1), " weeks).")
            } else {
                NULL
            }
        })
    }) %>% bindEvent(
        input$simtime,
        input$simtimeMSM,
        input$LD1, input$ldur_1, input$lunit_1, input$mdur_1, input$munit_1,
        input$LD2, input$ldur_2, input$lunit_2, input$mdur_2, input$munit_2,
        input$LD3, input$ldur_3, input$lunit_3, input$mdur_3, input$munit_3,
        input$RG2, input$RG3
    )

    # Combined validation observer
    observe({
        if (validationStates$enough_subjects && 
            validationStates$enough_time && 
            validationStates$enough_timeMSM) {
            shinyjs::enable("goButton")
        } else {
            shinyjs::disable("goButton")
        }
    })

    ###################### Render HL effect plot for individual mode #######
    # Render the PNG image
    output$HLEFFplot_indv <- renderImage({
      # Path to the PNG file
      filePath <- paste0(UI.directory, "HLEFF_halfLife_TSCC.png")
      
      # Return a list with the image path and optional width/height
      list(
        src = filePath,
        contentType = 'image/png',
        alt = "Plot image",
        width = "100%"# Optional: Adjust as needed
      )
    }, deleteFile = FALSE)  # Set to FALSE if the file should not be deleted after rendering
    
    ###################### Render HL effect plot for population mode ####### 
    # Render the PNG image
    output$HLEFFplot_pop <- renderImage({
      # Path to the PNG file
      filePath <- paste0(UI.directory, "HLEFF_halfLife_TSCC.png")
      
      # Return a list with the image path and optional width/height
      list(
        src = filePath,
        contentType = 'image/png',
        alt = "Plot image",
        width = "100%" # Optional: Adjust as needed
      )
    }, deleteFile = FALSE)  # Set to FALSE if the file should not be deleted after rendering

    ###################### OUTPUT OPTIONS ######################
    outputOptions(output, "plot", suspendWhenHidden = FALSE)
    outputOptions(output, "plotQT", suspendWhenHidden = FALSE)
    outputOptions(output, "plotTTP", suspendWhenHidden = FALSE)
    outputOptions(output, "plotMSM", suspendWhenHidden = FALSE)

    ###################### DOWNLOAD CODE HANDLER ######################
    output$download_code <- downloadHandler(
        filename = function() {
            "BDQ_Shiny_App_Source_Code.zip"
        },
        content = function(file) {
            # Create a temporary directory
            temp_dir <- tempdir()
            
            # List of files to include in the zip - R and CSV files
            files_to_zip <- c(
                # UI Components
                list.files(UI.directory, pattern = "\\.(R|csv)$", full.names = TRUE),
                
                # Server Components
                list.files(Server.directory, pattern = "\\.(R|csv)$", full.names = TRUE)
            )
            
            # Create directories in temp folder
            dir.create(file.path(temp_dir, "BDQ_UI"), recursive = TRUE, showWarnings = FALSE)
            dir.create(file.path(temp_dir, "BDQ_Server"), recursive = TRUE, showWarnings = FALSE)
            
            # Copy only existing R and CSV files
            for(f in files_to_zip) {
                if(file.exists(f)) {  # Only copy if file exists
                    if(grepl("BDQ_UI", f)) {
                        file.copy(f, file.path(temp_dir, "BDQ_UI", basename(f)), overwrite = TRUE)
                    } else {
                        file.copy(f, file.path(temp_dir, "BDQ_Server", basename(f)), overwrite = TRUE)
                    }
                }
            }
            
            # Create zip file
            zip::zipr(file, files = list.files(temp_dir, recursive = TRUE, full.names = TRUE, pattern = "\\.(R|csv)$"))
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
