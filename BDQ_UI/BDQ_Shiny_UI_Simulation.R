#### This is for UI - Simulation tab ####

# Simulation settings ####
mainTabSim <- tabPanel(
  title = "Simulation",
  value = "Simulation",  # Explicit value for the Simulation tab
  br(),
  page_fillable(
    layout_columns(
      # First card with custom background color and styled card header/body
      card(
        card_header("Advanced Setting", style = "font-size: 20px; background-color: #D8BFD8;"),
        card_body(
          tags$style(HTML("
            #HLEFFplot {
              height: auto !important; /* Removes the fixed height */
            }
          ")),
          selectInput("STUDY", 
            label = tags$span(style="font-size: 13px; font-weight: bold;", "Type of Model for PK-TTP"), 
            choices = c("Treatment-naïve", "Treatment-experienced"), 
            width = "70%"),
          numericInput("REP", 
            label = tags$span(style="font-size: 13px; font-weight: bold;", "Numbers of Culture Replicates"), 
            value = 1, min = 1, max = 3, 
            width = "70%")
          # numericInput("HLEFF", 
          #                label = tags$span(style = "font-size: 13px; font-weight: bold;", "Bacterial Clearance (%) Faster"), 
          #                value = 0, min = -50, max = 150, 
          #                width = "70%"),
          # tags$span(style="font-size: 13px; font-style: italic;", 
          #   "***HL and surge peak, surge amplitude are highly correlated"), 
          # imageOutput("HLEFFplot")
        )
      ),
      # Second card with custom background color and styled card header/body
      card(
        card_header("Simulation Setting", style = "font-size: 20px; background-color: #CDD8DA;"),
        card_body(
          numericInput("nsim", 
            label = tags$span(style="font-size: 13px; font-weight: bold;", "Number of simulated individuals per regimen"), 
            value = 1, min = 1, max = 300, 
            width = "70%"),
          textOutput("nsim_validation"),
          tags$style("#nsim_validation{color: red; margin-top: -15px; margin-bottom: -15px; font-style: italic;}"),
          numericInput("simtime", 
            label = tags$span(style="font-size: 13px; font-weight: bold;", "Simulation time in weeks (PK, efficacy, safety)"), 
            value = 24, min = 1, max = 200, 
            width = "70%"),
          textOutput("simtime_validation"),
          tags$style("#simtime_validation{color: red; margin-top: -15px; margin-bottom: -15px; font-style: italic;}"),
          numericInput("simtimeMSM", 
            label = tags$span(style="font-size: 13px; font-weight: bold;", "Simulation time in weeks (Long-term outcome)"), 
            value = 48, min = 1, max = 1000, 
            width = "70%"),
          textOutput("simtimeMSM_validation"),
          tags$style("#simtimeMSM_validation{color: red; margin-top: -15px; margin-bottom: -15px; font-style: italic;}"),
          selectInput("IIV", 
            label = tags$span(style="font-size: 13px; font-weight: bold;", "Interindividual variability"), 
            choices = c("OFF", "ON"), 
            selected = "OFF", 
            width = "70%"),
          actionButton("goButton", "Start simulation")
        )
      ),
      col_widths = c(6,6)
    )
  )
) # end of Simulation setting panel