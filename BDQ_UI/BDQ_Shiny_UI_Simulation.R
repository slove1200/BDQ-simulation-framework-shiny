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
        card_header("Advanced Settings", style = "font-size: 22px; background-color: #D8BFD8;"  # Using card_header for the title
        ),
        card_body(
          fluidRow(
            # Column for Background therapy
            column(6,
                   card(
                     card_header("TTP Model Settings", style = "font-size: 18px; background-color: #D8BFD84D;"),
                     card_body(
                       selectInput("STUDY", label = tags$span(style="font-weight: bold;","Type of Model for PK-TTP"), choices = c("Treatment-naïve", "Treatment-experienced"), width = "70%"),
                       numericInput("REP", label = tags$span(style="font-weight: bold;","Numbers of Culture Replicates"), value = 1, min = 1, max = 3, width = "70%"),
                       numericInput("HL", label = tags$span(style="font-weight: bold;", "Bacterial Clearance (%) Faster"), value = 20, min = -300, max = 300, width = "70%"),
                       "***HL and surge peak, surge amplitude are highly correlated"
                     )
                   )
            ),
            # Column for Model settings
            column(6,
                   card(
                     card_header("Multistate Model Settings", style = "font-size: 18px; background-color: #D8BFD84D;"),
                     card_body(
                       numericInput("SA", label = tags$span(style="font-weight: bold;","Peak Amplitude of Conversion (%) Increased"), value = 20, min = -300, max = 300, width = "70%"), 
                       numericInput("PT", label = tags$span(style="font-weight: bold;","Peak Time of Conversion (%) Faster"), value = 20, min = -300, max = 300, width = "70%")
                     )
                   )
            )
          ),
          # Add second row for two more columns
          fluidRow(
            # Third column
            column(6,
                   card(
                     style = "overflow: visible;",  # Add this to allow overflow on the card
                     card_header("QT Model Settings", 
                                 style = "font-size: 18px; background-color: #D8BFD84D;"),
                     card_body(
                       style = "overflow: visible;",  # Set overflow for card body
                       div(
                         style = "overflow: visible;",  # Ensure select input dropdown is fully visible
                         selectInput("QTCor", label = tags$span(style="font-weight: bold;", "Type of QT Correction Factor"), choices = c("QTcF", "QTc-TBT"), width = "70%")
                       )
                     )
                   )
            ),
            # Fourth column
            column(6,
                   card(
                     card_header("Others", style = "font-size: 18px; background-color: #D8BFD84D;"),
                     card_body(
                       "Additional content for Advanced settings goes here."
                     )
                   )
            )
          ),
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
      col_widths = c(8,4)
    )
  )
) # end of Simulation setting panel