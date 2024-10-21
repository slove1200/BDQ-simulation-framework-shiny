#### This is for UI - Results tab ####

# Results settings ####
mainTabResults <- tabPanel(
  "Results",
  tabsetPanel(
    id = "resTab", type = "pills",
    tabPanel("Overview"),
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
          card_body("Proportions of patients over time"
          )
        ),
        col_widths = c(12,6,6,12),
        row_heights = c(5,5,4)
      )
    )
  ) # end of conditionalPanel Overview
) # end of Results setting panel
