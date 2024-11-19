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
        card_header("Advanced Setting", style = "font-size: 22px; background-color: #D8BFD8;"),
        card_body(
          selectInput("STUDY", label = tags$span(style="font-weight: bold;","Type of Model for PK-TTP"), choices = c("Treatment-naïve", "Treatment-experienced"), width = "70%"),
          numericInput("REP", label = tags$span(style="font-weight: bold;","Numbers of Culture Replicates"), value = 1, min = 1, max = 3, width = "70%"),
          numericInput("HL", label = tags$span(style="font-weight: bold;", "Bacterial Clearance (%) Faster"), value = 20, min = -300, max = 300, width = "70%"),
          "***HL and surge peak, surge amplitude are highly correlated"
        )
      ),
      # Second card with custom background color and styled card header/body
      card(
        card_header("Simulation Setting", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
        card_body(
          numericInput("nsim", label = tags$span(style="font-weight: bold;","Number of simulated individuals per regimen"), value = 1, min = 1, max = 300, width = "70%"),
          numericInput("simtime", label = tags$span(style="font-weight: bold;","Simulation time (PK, efficacy, safety)"), value = 24, min = 1, max = 300, width = "70%"),
          selectInput("sunit", label = tags$span(style="font-weight: bold;","Simulation time unit (PK, efficacy, safety)"), c("week" = "2", "day" = "1"), width = "70%"),
          selectInput("IIV", label = tags$span(style="font-weight: bold;","Interindiviual variability"), c("OFF", "ON"), selected = "OFF", width = "70%"), 
          numericInput("simtimeMSM", label = tags$span(style="font-weight: bold;","Simulation time in weeks (Long-term outcome)"), value = 48, min = 1, max = 1000, width = "70%"),
          actionButton("goButton", "Start simulation")
        )
      ),
      col_widths = c(6,6)
    )
  )
) # end of Simulation setting panel