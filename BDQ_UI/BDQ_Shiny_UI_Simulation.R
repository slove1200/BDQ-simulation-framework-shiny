#### This is for UI - Simulation tab ####

# Simulation settings ####
mainTabSim <- tabPanel(
  title = "Simulation",
  value = "Simulation",  # Explicit value for the Simulation tab
  br(),
  page_fillable(
    layout_columns(
      # card with custom background color and styled card header/body
      card(
        card_header("Simulation Setting", style = "font-size: 20px; background-color: #CDD8DA;"),
        card_body(
          numericInput("nsim", 
            label = tags$span(style="font-size: 13px; font-weight: bold;", "Number of Simulated Individuals per Regimen"), 
            value = 1, min = 1, max = 300, 
            width = "100%"),
          textOutput("nsim_validation"),
          tags$style("#nsim_validation{color: red; margin-top: -15px; margin-bottom: -15px; font-style: italic;}"),
          numericInput("REP", 
            label = tags$span(style="font-size: 13px; font-weight: bold;", "Number of MGIT Culture Replicates per Sampling Timepoint"), 
            value = 1, min = 1, max = 3, 
            width = "100%"),
          textOutput("REP_validation"),
          tags$style("#REP_validation{color: red; margin-top: -15px; margin-bottom: -15px; font-style: italic;}"),
          numericInput("simtime", 
            label = tags$span(style="font-size: 13px; font-weight: bold;", "Simulation Time in Weeks (PK, Efficacy, Safety)"), 
            value = 24, min = 1, max = 200, 
            width = "100%"),
          textOutput("simtime_validation"),
          tags$style("#simtime_validation{color: red; margin-top: -15px; margin-bottom: -15px; font-style: italic;}"),
          numericInput("simtimeMSM", 
            label = tags$span(style="font-size: 13px; font-weight: bold;", "Simulation Time in Weeks (Long-term Outcome)"), 
            value = 48, min = 1, max = 1000, 
            width = "100%"),
          textOutput("simtimeMSM_validation"),
          tags$style("#simtimeMSM_validation{color: red; margin-top: -15px; margin-bottom: -15px; font-style: italic;}"),
          selectInput("IIV", 
            label = tags$span(style="font-size: 13px; font-weight: bold;", "Interindividual Variability"), 
            choices = c("OFF", "ON"), 
            selected = "OFF", 
            width = "100%"),
          actionButton("goButton", "Start simulation")
        )
      ),
      col_widths = c(6)
    )
  )
) # end of Simulation setting panel