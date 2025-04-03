#### This is for UI - TTP Simulation tab ####

# Simulation settings ####
mainTabTTPSim <- tabPanel(
  title = "Extra: TTP Simulation",
  value = "TTPsim",
  tags$head(tags$style(HTML("
    /* Other existing styles */
    #tableTTPsim {
      font-size: 12px !important;  /* Reduce font size */
    }
    #tableTTPsim th, #tableTTPsim td {
      padding: 7px 10px !important;  /* Reduce cell padding */
    }
    
    .dataTables_info {
      display: none !important;
    }

    .dataTables_info, .dataTables_paginate {
      font-size: 10px !important;  /* Reduce size of footer elements */
    }

    /* Make DataTable container height automatic */
    .html-fill-container > .html-fill-item.datatables {
      flex-basis: auto !important;
    }
    
    /* Target only TTP_distribution_choice radio buttons */
    #TTP_distribution_choice .radio-inline input[type='radio'] {
      width: 12px !important;
      height: 12px !important;
      vertical-align: middle !important;
    }
    
    #TTP_distribution_choice .radio-inline {
      margin-right: 15px !important;
      font-size: 12px !important;
    }
    
    #TTP_distribution_choice .shiny-options-group {
      display: flex !important;
      align-items: center !important;
    }

  "))),
  tabsetPanel(
    id = "TTPsimTab", type = "pills",
    tabPanel("User Manual", value = "TTP User Manual"),
    tabPanel("TTP Simulation", value = "TTP Simulation")
  ),
  br(),
  conditionalPanel(
    condition = "input.TTPsimTab == 'TTP User Manual'",
    page_fillable(
      layout_columns(
        card(
          card_header("Overview", style = "font-size: 20px; background-color: #CDD8DA;"),
          card_body(
            tags$span("This simulation module allows users to generate individual time-to-positivity (TTP) profiles using the PK-TTP models developed by Svensson and Karlsson 
                       (J Antimicrob Chemother, 2017) and Tanneau et al. (Br J Clin Pharmacol, 2020) in flexible simulation settings. 
                       The outputs include individual TTP data and the time-to-sputum culture conversion (TSCC) plot (if applicable) which enable users to do further 
                       analysis on the TTP data.",
                       tags$strong("In this module, individual drug exposure is not used as a covariate. Instead, the overall combination therapy, as described by the half-life of mycobacterial load decline (see below), is considered."),
                     style = "font-size: 14px; padding-left: 5px; line-height: 1.9;"),
            layout_columns(
              card(
                card_header("Input Parameters", style = "font-size: 16px; background-color: #E8ECEE;"),
                card_body(
                  tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                    tags$li("Number of Individuals to simulate"),
                    tags$li("Baseline Time-to-positivity signal for individual subjects in days:", 
                      tags$br(), 
                      tags$ul(
                        tags$li(tags$strong("Use default distribution: "), "Uses the distribution from a representative TB virtual population with meidan 5.0 days, minimum 1.25 days and maximum 42 days"),
                        tags$li(tags$strong("Define custom range: "), "Allows you to specify minimum and maximum values of the distribution")
                      )
                    ),
                    tags$li(
                      tags$strong("Half-life of Mycobacterial Load Decline:"), 
                      tags$ul(
                        tags$li("A parameter which reflects how different regimens influence bacterial elimination"), 
                      tags$li(tags$strong("The default value is 0.81, which represents the half-life reported in the developed model without bedaquiline treatment")), 
                      tags$li(
                        "The graph of conversion rate over half-life of mycobacterial load is demonstrated in the population under background regimen (no bedaquiline)",
                        tags$br(), 
                        "while the model had been developed", 
                        tags$br(), 
                        "This could help users select the most suitable value of half-life in the current tuberculosis treatment settings"), 
                      )
                    ),
                    tags$li(tags$strong("Culture Sampling Timepoints:"), 
                            " Timepoints for taking sputum culture samples", 
                            tags$ul(
                              tags$li(tags$strong("The timepoints should be entered as comma-separated values")), 
                              tags$li("Choose Time Unit in weeks or days")
                            )
                    ),
                    tags$li(tags$strong("Number of Culture Replicates per Sampling Timepoint:"), 
                            "The default value is 1 per sampling timepoint, maximum of 3"),
                    tags$li(tags$strong("Definition of Sputum culture conversion (SCC):"), 
                            tags$br(), 
                            tags$ul(
                              tags$li("SCC is defined as two consecutive negative sputum cultures collected at X days apart, without intervened by any postive cultures. The default is 28 days"),
                              tags$li("Time to SCC is defined as the time from the first negative culture if SCC is reached")
                            )
                            )
                  )
                )
              ),
              card(
                card_header("Output", style = "font-size: 16px; background-color: #E8ECEE;"),
                card_body(
                  tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                    tags$li("Individual TTP Data: Table showing culture results for each subject. Table with specifications can be downloaded in a zipped file"),
                    tags$li("TSCC Plot: Visual representation of time to sputum culture conversion (only when sampling schedule is set in weeks)")
                  )
                )
              ),
              col_widths = c(12, 12)
            )
          )
        ),
        col_widths = c(12)
      )
    )
  ),
  conditionalPanel(
    condition = "input.TTPsimTab == 'TTP Simulation'",
    page_fillable(
      layout_columns(
        # Card 1: User input
        card(
          card_header("User Input", style = "font-size: 20px; background-color: #CDD8DA;"),
          card_body(
            numericInput("nsim_TTP", 
                        label = tags$span(style="font-size: 13px; font-weight: bold;", "Number of Subjects"), 
                        value = 33, min = 1, max = 3000, 
                        width = "100%"), 
            # Baseline Time-to-positivity options
            radioButtons("TTP_distribution_choice", 
                        label = tags$span(style="font-size: 13px; font-weight: bold;", "Baseline Time-to-positivity (TTP) Distribution"),
                        choiceNames = list(
                          tags$span(style="font-size: 12px;", "Use default distribution"),
                          tags$span(style="font-size: 12px;", "Define custom range")
                        ),
                        choiceValues = list("Default", "Custom"),
                        selected = "Default",
                        inline = TRUE,
                        width = "100%"),
            # Conditional panel for default distribution
            conditionalPanel(
              condition = "input.TTP_distribution_choice == 'Default'",
              tags$div(
                tags$p(style = "font-size: 12px; color: #666;", 
                      "Using the distribution from a representative TB virtual population with median 5.0 days, minimum 1.25 days and maximum 42 days")
              )
            ),
            # Conditional panel for custom range
            conditionalPanel(
              condition = "input.TTP_distribution_choice == 'Custom'",
              tags$div(
                fluidRow(
                  column(width = 6,
                    numericInput("MTTP_TTP_min", 
                                label = tags$span(style="font-size: 12px; font-weight: bold;", "Minimum Baseline TTP (days)"), 
                                value = 1.25, min = 1.25, max = 41.9, step = 0.1, 
                                width = "100%")
                  ),
                  column(width = 6,
                    numericInput("MTTP_TTP_max", 
                                label = tags$span(style="font-size: 12px; font-weight: bold;", "Maximum Baseline TTP (days)"), 
                                value = 42, min = 1.26, max = 42, step = 0.1, 
                                width = "100%")
                  )
                )
              )
            ),
            numericInput("HLEFF_TTP", 
                        label = tags$span(style="font-size: 13px; font-weight: bold;", "Half-life of Mycobacterial Load"), 
                        value = 0.81, min = 0.01, max = 100, 
                        width = "100%"),
            # Add image output for TSCC2.png
            imageOutput("HLEFFplot_TTPsim", height = "auto"),
            tags$span("The graph of conversion rate over half-life of mycobacterial load is demonstrated in patients with multidrug-resistant tuberculosis. 
                       Dashed vertical line represents the half-life reported in the developed model without bedaquiline treatment but five-drug background regimen including ethionamide, pyrazinamide, ofloxacin, kanamycin, and cycloserine.
                       This could help users select the most suitable value of half-life in the current tuberculosis treatment settings.",
                      style = "font-size: 10px;"),
            textInput("simtime_TTP", 
                     label = tags$span(style="font-size: 13px; font-weight: bold;", "Time for Culture Sampling (comma-separated)"),
                     value = "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24", 
                     width = "100%"), 
            selectInput("simunit_TTP",  
                     label = tags$span(style="font-size: 13px; font-weight: bold;", "Culture Sampling Time Unit"), 
                     c("week" = "2", "day" = "1"), selected = "week", 
                     width = "100%"), 
            numericInput("REP_TTP", 
                        label = tags$span(style="font-size: 13px; font-weight: bold;", "Number of Culture Replicates per Sampling Timepoint"), 
                        value = 1, min = 1, max = 3, 
                        width = "100%"), 
            numericInput("DEF_TTP", 
                        label = tags$span(style="font-size: 13px; font-weight: bold;", "Definition of Sputum Culture Conversion in Days"), 
                        value = 28, min = 1, max = 45, 
                        width = "100%"),
            actionButton("goButton_TTP", "Start simulation"),
            # Add progress bar
            tags$div(
              style = "margin-top: 10px;",
              uiOutput("progressBox_TTP")
            )
          )
        ),
        layout_columns(
          # Card 2: Individual TTP table
          card(
            card_header(
              class = "d-flex mb-3",
              style = "background-color: #CDD8DA;",
              div("Individual TTP Data", 
                  class = "me-auto p-0", 
                  style = "font-size: 20px;")
            ),
            card_body(
              downloadButton("download_TTPsim", "Download TTP Data",
              style = "font-size: 10px;"),
              DT::dataTableOutput("tableTTPsim")
            )
          ),
          conditionalPanel(
            condition = "input.simunit_TTP == '2'",  # Only show when weeks selected
            # Card 3: Summary TTP plot (only shown for weeks)
            card(
              card_header("TSCC Plot", style = "font-size: 20px; background-color: #CDD8DA;"),
              card_body(
                plotOutput("plotTTPsim"), 
                tags$span("The graph is based on results from the developed time-to-event model. 
                           As a result, the graph may vary when using a different random seed for the simulation.",
                          style = "font-size: 10px;")
              )
            )
          ),
          col_widths = c(12, 12)
        ),
        col_widths = c(6, 6)
      )
    )
  )
)