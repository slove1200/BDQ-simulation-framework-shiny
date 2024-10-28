#### This is for UI - Results tab ####

# Results settings ####
mainTabResults <- tabPanel(
  title = "Results",
  value = "Results",  # Explicit value for the Results tab
  tabsetPanel(
    id = "resTab", type = "pills",
    tabPanel("Overview"),
    tabPanel("Patient Characteristics"),
    tabPanel("Pharmacokinetics"),
    tabPanel("Efficacy"),
    tabPanel("Safety (QT)"),
    tabPanel("Long-term Outcome")
  ),
  br(),
  conditionalPanel(
    condition = "input.resTab=='Overview'",
    page_fillable(
      layout_columns(
        card(
          card_header("Pharmacokinetics", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(#tableOutput("sim_PKtable"),
                    plotOutput("plot")
          )
        ),
        # Second card with custom background color and styled card header/body
        card(
          card_header("Efficacy", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
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
        col_widths = c(12,6,6,12),
        row_heights = c(5,5,8)
      )
    )
  ), # end of conditionalPanel Overview, 
  conditionalPanel(
    condition = "input.resTab=='Patient Characteristics'",
    page_fillable(
      layout_columns(
        card(
          card_header("Table", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body("Put table summary here"
          )
        ),
        # Second card with custom background color and styled card header/body
        card(
          card_header("Plot", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body("Put plot summary here"
          )
        ),
        col_widths = c(12,12),
        row_heights = c(6,6)
      )
    )
  ), # end of conditionalPanel Patient Characteristics
  conditionalPanel(
    condition = "input.resTab=='Pharmacokinetics'",
    page_fillable(
      layout_columns(
        card(
          card_header("Daily average concentration", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(plotOutput("plotPKDavg")
          )
        ),
        # Second card with custom background color and styled card header/body
        card(
          card_header("Weekly average concentration", style = "font-size: 22px; background-color: #CDD8DA;"),  # Using card_header for the title
          card_body(plotOutput("plotPKWavg")
          )
        ),
        col_widths = c(12,12),
        row_heights = c(6,6)
      )
    )
  ) # end of conditionalPanel Patient Characteristics
) # end of Results setting panel
