#### This is for UI - TTP Simulation tab ####

# Simulation settings ####
mainTabTTPSim <- tabPanel(
  title = "TTP Simulation",
  value = "TTPsim",
  br(),
  tags$head(tags$style(HTML("
    #tableTTPsim {
      font-size: 14px !important;  /* Adjust font size */
    }
    #tableTTPsim th, #tableTTPsim td {
      padding: 10px !important;  /* Adjust row height by changing padding */
    }
  "))),
  page_fillable(
    layout_columns(
      # Card 1: User input
      card(
        card_header("User Input", style = "font-size: 20px; background-color: #CDD8DA;"),
        card_body(
          numericInput("nsim_TTP", 
                      label = tags$span(style="font-size: 13px; font-weight: bold;", "Number of Subjects"), 
                      value = 33, min = 1, max = 3000, 
                      width = "70%"), 
          selectInput("STUDY_TTP", 
                     label = tags$span(style="font-size: 13px; font-weight: bold;", "Type of Patient Population"), 
                     choices = c("Treatment-naïve", "Treatment-experienced"), 
                     width = "70%"),
          numericInput("MTTP_TTP", 
                      label = tags$span(style="font-size: 13px; font-weight: bold;", "Baseline Time-to-positivity in MGIT Culture (days)"), 
                      value = 6.8, min = 0.1, max = 42, step = 0.1, 
                      width = "70%"),
          numericInput("HLEFF_TTP", 
                      label = tags$span(style="font-size: 13px; font-weight: bold;", "% Longer Half-life of Mycobacterial Load"), 
                      value = -40, min = -100, max = 500, 
                      width = "70%"),
          textInput("simtime_TTP", 
                   label = tags$span(style="font-size: 13px; font-weight: bold;", "Time for Culture Sampling (comma-separated)"),
                   value = "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24", 
                   width = "70%"), 
          selectInput("simunit_TTP",  
                     label = tags$span(style="font-size: 13px; font-weight: bold;", "Culture Sampling Time Unit"), 
                     c("week" = "2", "day" = "1"), selected = "week", 
                     width = "70%"), 
          numericInput("REP_TTP", 
                      label = tags$span(style="font-size: 13px; font-weight: bold;", "Number of Culture Replicates per Sampling Timepoint"), 
                      value = 1, min = 1, max = 3, 
                      width = "70%"), 
          numericInput("DEF_TTP", 
                      label = tags$span(style="font-size: 13px; font-weight: bold;", "Definition of Sputum Culture Conversion in Days"), 
                      value = 28, min = 1, max = 45, 
                      width = "70%"),
          actionButton("goButton_TTP", "Start simulation")
        )
      ),
      layout_columns(
        # Card 2: Individual TTP table
        card(
          card_header("Individual TTP Data", style = "font-size: 20px; background-color: #CDD8DA;"),
          card_body(
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
) # end of TTP Simulation setting panel