#### This is for UI - Source Code tab ####

mainTabCode <- function() {
  tabPanel(
    title = "Source Code",
    value = "Source Code",  # Explicit value for the Code Output tab
    br(),
    page_fillable(
        layout_columns(
            # First card for Code Output
            card(
                card_header("Source Code", style = "font-size: 20px; background-color: #D8BFD8;"),
                card_body(
                    downloadButton("download_code", "Download Source Code and Files Needed"),
                    tags$span(tags$strong("Included Files:", style = "font-size: 14px;")),
                    tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px;line-height: 1.9;",
                        tags$li("UI Components:", 
                            tags$ul(
                                tags$li("BDQ_Shiny_UI_Dosing.R"),
                                tags$li("BDQ_Shiny_UI_Population.R"),
                                tags$li("BDQ_Shiny_UI_Simulation.R"),
                                tags$li("BDQ_Shiny_UI_Results.R"),
                                tags$li("BDQ_Shiny_UI_About.R"),
                                tags$li("BDQ_Shiny_UI_SourceCode.R")
                            )
                        ),
                        tags$li("Server Components:", 
                            tags$ul(
                                tags$li("Model Files (BDQOMAT.R, BDQTTP.R, etc.)"),
                                tags$li("Server Functions (BDQ_Server_*.R)"),
                                tags$li("Plotting Functions"),
                                tags$li("Summary Functions")
                            )
                        ), 
                        tags$li("Dataset Components:", 
                                tags$ul(
                                  tags$li("Virtual Population Template .csv File"),
                                  tags$li("Simulated Virtual Population .csv File")
                                )
                        )
                    )
                )
            ),
            
            # Second card for Instructions
            card(
                card_header("Instructions", style = "font-size: 20px; background-color: #D8BFD8;"),
                card_body(
                    tags$span(tags$strong("Download all necessary source code files to run the BDQ Shiny app locally.", 
                             style = "font-size: 14px;")),
                    div(style = "font-size: 14px; padding-left: 5px; line-height: 1.9;",
                        tags$div(style = "margin-left: 0;", tags$strong("Step 1: "), "Download the zip file containing all source code and files needed"),
                        tags$div(style = "margin-left: 0;", tags$strong("Step 2: "), "Extract the contents to your desired directory"),
                        tags$div(style = "margin-left: 0;", tags$strong("Step 3: "), "Update the directory paths in the main server file"),
                        tags$div(style = "margin-left: 0;", tags$strong("Step 4: "), "Install required R packages"),
                        tags$div(style = "margin-left: 0;", tags$strong("Step 5: "), "Run the app using runApp()")
                    )
                )
            ),
            col_widths = c(4,8)
        )
    )
  )
}
