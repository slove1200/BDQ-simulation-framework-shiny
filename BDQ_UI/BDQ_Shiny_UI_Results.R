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

      /* Specific styles for Output tab cards */
      #resTab-Output .card {
        height: 500px !important;
        margin-bottom: 20px;
      }
      
      #resTab-Output .card-body {
        height: calc(100% - 40px) !important;
        padding: 10px;
      }

      /* Download section styles */
      .download-section {
        margin-bottom: 20px;
      }

      .download-header {
        font-size: 18px;
        font-weight: bold;
        margin-bottom: 10px;
      }
    "))
  ),
  tabsetPanel(
    id = "resTab", type = "pills",
    tabPanel("Overview"),
    tabPanel("Patient Characteristics"),
    tabPanel("Output",
      br(),
      page_fillable(
        layout_columns(
          card(
            card_header("Download Simulation Results", style = "font-size: 20px; background-color: #CDD8DA;"),
            card_body(
              layout_columns(
                div(
                  # Pharmacokinetics section
                  div(class = "download-section",
                      tags$div(class = "download-header", "Pharmacokinetics data"),
                      downloadButton("download_simPK", "PK_output.csv")
                  ),
                  # Efficacy section
                  div(class = "download-section",
                      tags$div(class = "download-header", "Efficacy data (Time to positivity signal)"),
                      downloadButton("download_simTTP", "TTP_output.csv")
                  ),
                  # Population characteristics section
                  div(class = "download-section",
                      tags$div(class = "download-header", "Virtual individual or population characteristics"),
                      downloadButton("download_virtual_population", "virtual_individual_or_population.csv")
                  ),
                ), 
                div(
                  # Safety section
                  div(class = "download-section",
                      tags$div(class = "download-header", "Safety data (QT)"),
                      downloadButton("download_simQT", "QT_output.csv")
                  ),
                  # Long-term outcome section
                  div(class = "download-section",
                      tags$div(class = "download-header", "Long-term outcome data"),
                      downloadButton("download_simMSM", "longTermOutcome_output.csv")
                  )
                ), 
                col_widths = c(6, 6)
              )
            )
          ), 
          col_widths = 12
        )
      )
    )
  ),
  br(),
  conditionalPanel(
    condition = "input.resTab=='Overview'",
    page_fillable(
      layout_columns(
        # Dosing details
        card(
          card_header("Dosing regimens", style = "font-size: 20px; background-color: #CDD8DA;"),
          card_body(
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
                style = "font-size: 20px; background-color: #CDD8DA;"),
            div(class = "pe-2 header-radio", 
                style = "font-size: 14px;",
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
          card_header("Efficacy (Time to positivity signal)", style = "font-size: 20px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(#tableOutput("sim_TTPtable"),
                    plotOutput("plotTTP")
          )
        ),
        # Third card with custom background color and styled card header/body
        card(
          card_header("Safety (QT)", style = "font-size: 20px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(#tableOutput("sim_QTtable"),
                    plotOutput("plotQT")
          )
        ),
        # Fourth card with custom background color and styled card header/body
        card(
          card_header("Long-term outcome", style = "font-size: 20px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(#tableOutput("sim_MSMtable"),
                    plotOutput("plotMSM")
          )
        ),
        col_widths = c(12,12,6,6,12),
        row_heights = c(2,5,5,8)
      )
    )
  ), # end of conditionalPanel Overview
  conditionalPanel(
    condition = "input.resTab=='Patient Characteristics'",
    page_fillable(
      layout_columns(
        card(
          card_header("Table", style = "font-size: 20px; background-color: #CDD8DA;"),
          card_body(
            tableOutput("PopSummaryTable")
          )
        ),
        # Second card with custom background color and styled card header/body
        card(
          card_header("Plot", style = "font-size: 20px; background-color: #CDD8DA;"),
          card_body(
            plotOutput("PopSummaryPlot")
          )
        ),
        col_widths = c(5,7)
      )
    )
  )
) # end of Results setting panel
