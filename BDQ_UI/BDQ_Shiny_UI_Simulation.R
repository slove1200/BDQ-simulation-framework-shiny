#### This is for UI - Simulation tab ####

# Simulation settings ####
mainTabSim <- tabPanel(
  title = "Simulation",
  value = "Simulation",  # Explicit value for the Simulation tab
  br(),
  page_fillable(
    layout_columns(
      card(
        card_header("Sampling Schedule", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
        card_body(style = "font-weight: bold;",
                  "Choose sampling schedule"
        )
      ),
      # Second card with custom background color and styled card header/body
      card(
        card_header("Simulation Setting", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
        card_body(
          numericInput("nsim", label = tags$span(style="font-weight: bold;","Number of simulated individuals per regimen"), value = 1, min = 1, max = 10000),
          numericInput("simtime", label = tags$span(style="font-weight: bold;","Simulation time (PK, efficacy, safety)"), value = 24, min = 1, max = 24000),
          selectInput("sunit", label = tags$span(style="font-weight: bold;","Simulation time unit (PK, efficacy, safety)"), c("week" = "2", "day" = "1")),
          selectInput("IIV", label = tags$span(style="font-weight: bold;","Interindiviual variability"), c("OFF", "ON"), selected = "OFF"), 
          numericInput("simtimeMSM", label = tags$span(style="font-weight: bold;","Simulation time in weeks (Long-term outcome)"), value = 48, min = 1, max = 24000),
          actionButton("goButton", "Start simulation")
        )
      ),
      col_widths = c(5,5)
    )
  )
) # end of Simulation setting panel