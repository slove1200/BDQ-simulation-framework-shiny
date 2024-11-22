#### This is for UI - Results tab ####
# Results settings ####
mainTabResults <- tabPanel(
  title = "Results",
  value = "Results",
  tags$head(
    tags$style(HTML("
      /* Value box styles */
      .value-box {
        margin-bottom: 1rem;
      }
      
      /* Remove scrollbars from cards by default */
      .card {
        overflow: unset !important;
      }
      
      .card-body {
        overflow: unset !important;
      }

    "))
  ),
  tabsetPanel(
    id = "resTab", type = "pills",
    tabPanel("Overview"),
    tabPanel("Patient Characteristics")
  ),
  br(),
  conditionalPanel(
    condition = "input.resTab=='Overview'",
    page_fillable(
      layout_columns(
        # Dosing details
        card(
          card_header("Dosing regimens", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(
            # Add a text output element to display regimen details
            uiOutput("regimen_boxes")
          )
        ),
        # First card with custom background color and styled card header/body
        card(
          card_header(
            class = "d-flex mb-3",
            style = "background-color: #CDD8DA;", 
            div("Pharmacokinetics", 
                class = "me-auto p-0", 
                style = "font-size: 22px; background-color: #CDD8DA;"),
            div(class = "pe-2 header-radio", 
                style = "font-size: 16px;",
                radioButtons("PKplot_radio", 
                             label = NULL, 
                             choices = c("Full Concentration", "Daily Avg Concentration", "Weekly Avg Concentration"), 
                             inline = TRUE)
                )
              ),  
          card_body(#tableOutput("sim_PKtable"),
                    conditionalPanel(
                      condition = "input.PKplot_radio == 'Full Concentration'",
                      plotOutput("plot")
                    ), 
                    conditionalPanel(
                      condition = "input.PKplot_radio == 'Daily Avg Concentration'",
                      plotOutput("plotPKDavg")
                    ), 
                    conditionalPanel(
                      condition = "input.PKplot_radio == 'Weekly Avg Concentration'",
                      plotOutput("plotPKWavg")
                    )
          )
        ),
        # Second card with custom background color and styled card header/body
        card(
          card_header("Efficacy (Time to positivity signal)", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(#tableOutput("sim_TTPtable"),
                    plotOutput("plotTTP")
          )
        ),
        # Third card with custom background color and styled card header/body
        card(
          card_header("Safety (QT)", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(#tableOutput("sim_QTtable"),
                    plotOutput("plotQT")
          )
        ),
        # Fourth card with custom background color and styled card header/body
        card(
          card_header("Long-term outcome", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(#tableOutput("sim_MSMtable"),
                    plotOutput("plotMSM")
          )
        ),
        col_widths = c(12,12,6,6,12),
        row_heights = c(2,5,5,8)
      )
    )
  ), # end of conditionalPanel Overview, 
  conditionalPanel(
    condition = "input.resTab=='Patient Characteristics'",
    page_fillable(
      layout_columns(
        card(
          card_header("Table", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(tableOutput("PopSummaryTable")
          )
        ),
        # Second card with custom background color and styled card header/body
        card(
          card_header("Plot", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(plotOutput("PopSummaryPlot")
          )
        ),
        col_widths = c(5,7)
      )
    )
  )
) # end of Results setting panel
