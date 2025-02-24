#### This is for UI - TTP Simulation tab ####

# Simulation settings ####
mainTabTTPSim <- tabPanel(
  title = "Extra: TTP Simulation",
  value = "TTPsim",
  tags$head(tags$style(HTML("
    #tableTTPsim {
      font-size: 12px !important;  /* Reduce font size */
    }
    #tableTTPsim th, #tableTTPsim td {
      padding: 4px 8px !important;  /* Reduce cell padding */
    }
    .dataTables_wrapper {
      font-size: 12px !important;  /* Reduce size of wrapper elements */
    }
    .dataTables_info, .dataTables_paginate {
      font-size: 10px !important;  /* Reduce size of footer elements */
    }
    /* Make DataTable container height automatic */
    .html-fill-container > .html-fill-item.datatables {
      flex-basis: auto !important;
      height: auto !important;
      min-height: 0 !important;
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
                       tags$strong("In this module, the individual drug exposure is not considered but the overall combination therapy, as described by the half-life of mycobacterial load modifier (see below)."),
                     style = "font-size: 14px; padding-left: 5px; line-height: 1.9;"),
            layout_columns(
              card(
                card_header("Input Parameters", style = "font-size: 16px; background-color: #E8ECEE;"),
                card_body(
                  tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                    tags$li("Number of Individuals to simulate"),
                    tags$li("Type of Population:", 
                      tags$br(), 
                      tags$ul(
                        tags$li(tags$strong("Treatment-naïve: "), "For individuals with no prior TB treatment"),
                        tags$li(tags$strong("Treatment-experienced: "), "For individuals with prior TB treatment")
                      ) 
                    ),
                    tags$li("Baseline Time-to-positivity signal for individual subjects in days"),
                    tags$li(
                      tags$strong("Half-life of Mycobacterial Load Modifier:"), 
                      tags$ul(
                        tags$li("A parameter that can be used to adjust the half-life of mycobacterial load, 
                      reflecting how different regimens influence bacterial elimination."), 
                      tags$li(tags$strong("The default value is 0, which means no adjustment is made to the reference half-life reported in the developed model")), 
                      tags$ul(
                        tags$li(
                          "Half-life of mycobacterial load is longer with % of a positive value (+) and shorter with % of a negative value (-) of the modifier.",
                          tags$br(), 
                          "For example, a value of 30 indicates a 30% longer half-life, whereas -30 means a 30% shorter half-life"), 
                        tags$li(tags$strong("The minimum values of half-life modifier is -100%"), 
                          ", since a > 100% shorter half-life is not possible")
                      ),
                      tags$li(
                        "The graph of conversion rate over relative change (%) of half-life modifier is demonstrated in the population under background regimen (no bedaquiline)",
                        tags$br(), 
                        "while the model had been developed"), 
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
            selectInput("STUDY_TTP", 
                       label = tags$span(style="font-size: 13px; font-weight: bold;", "Type of Population"), 
                       choices = c("Treatment-naïve", "Treatment-experienced"), 
                       width = "100%"),
            numericInput("MTTP_TTP", 
                        label = tags$span(style="font-size: 13px; font-weight: bold;", "Baseline Time-to-positivity in MGIT Culture (days)"), 
                        value = 6.8, min = 0.1, max = 42, step = 0.1, 
                        width = "100%"),
            numericInput("HLEFF_TTP", 
                        label = tags$span(style="font-size: 13px; font-weight: bold;", "% Longer Half-life of Mycobacterial Load"), 
                        value = -40, min = -100, max = 500, 
                        width = "100%"),
            # Add image output for TSCC2.png
            imageOutput("HLEFFplot_TTPsim", height = "auto"),
            textInput("simtime_TTP", 
                     label = tags$span(style="font-size: 13px; font-weight: bold;", "Time for Culture Sampling (comma-separated)"),
                     value = "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24", 
                     width = "100%"), 
            selectInput("simunit_TTP",  
                       label = tags$span(style="font-size: 13px; font-weight: bold;", "Culture Sampling Time Unit"), 
                       choices = c("week" = "2", "day" = "1"), selected = "week", 
                       width = "100%"), 
            numericInput("REP_TTP", 
                        label = tags$span(style="font-size: 13px; font-weight: bold;", "Number of Culture Replicates per Sampling Timepoint"), 
                        value = 1, min = 1, max = 3, 
                        width = "100%"), 
            numericInput("DEF_TTP", 
                        label = tags$span(style="font-size: 13px; font-weight: bold;", "Definition of Sputum Culture Conversion in Days"), 
                        value = 28, min = 1, max = 45, 
                        width = "100%"),
            actionButton("goButton_TTP", "Start simulation")
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
                plotOutput("plotTTPsim")
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