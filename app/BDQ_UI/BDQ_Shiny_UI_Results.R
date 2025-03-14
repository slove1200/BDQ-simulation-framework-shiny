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
        font-size: 14px;
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
                      tags$div(class = "download-header", "Pharmacokinetics Data"),
                      downloadButton("download_simPK", "Download PK Data")
                  ),
                  # Efficacy section
                  div(class = "download-section",
                      tags$div(class = "download-header", "Efficacy Data (Time to Positivity Signal)"),
                      downloadButton("download_simTTP", "Download TTP Data")
                  ),
                  # Population characteristics section
                  div(class = "download-section",
                      tags$div(class = "download-header", "Virtual Individual or Population Characteristics"),
                      downloadButton("download_virtual_population", "Download Individual/Population Data")
                  ),
                ), 
                div(
                  # Safety section
                  div(class = "download-section",
                      tags$div(class = "download-header", "Safety Data (QT)"),
                      downloadButton("download_simQT", "Download QT Data")
                  ),
                  # Long-term outcome section
                  div(class = "download-section",
                      tags$div(class = "download-header", "Long-term Outcome Data"),
                      downloadButton("download_simMSM", "Download Long-term Outcome Data")
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
          card_header("Dosing Regimens", style = "font-size: 20px; background-color: #CDD8DA;"),
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
          card_header("Efficacy (Time to Positivity Signal)", style = "font-size: 20px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(#tableOutput("sim_TTPtable"),
                    plotOutput("plotTTP"), 
                    tags$span("The graph is based on results from the logistic model, which describes the probability of obtaining a positive sample. 
                               As a result, the graph may vary when using a different random seed for the simulation.",
                              style = "font-size: 10px;")
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
          card_header("Long-term Outcome", style = "font-size: 20px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(#tableOutput("sim_MSMtable"),
                    plotOutput("plotMSM"),
                    tags$span("The graph is based on results from the parametric multistate model, which describes the probability of being in each state. 
                               As a result, the graph may vary when using a different random seed for the simulation.",
                              style = "font-size: 10px;")
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
