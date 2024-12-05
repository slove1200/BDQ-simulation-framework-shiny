#### This is for UI - Population tab ####
# Function to create the UI for a regimen column with coMed controls in Population tab
CoMedColumn <- function(regimen_num, background_color, default_LD = FALSE, addition_RG = TRUE) {
  
  # Helper to create the coMed controls
  coMedControlsPK <- function(regimen_num) {
    div(
      radioButtons(inputId = paste0("IE_", regimen_num, "_PK"), 
                   label = NULL,
                   choices = c("None" = "None", 
                               "Efavirenz" = "Efavirenz", 
                               "Lopinavir/r" = "Lopinavir/r", 
                               "Nevirapine" = "Nevirapine", 
                               "Rifampicin" = "Rifampicin", 
                               "Rifapentine" = "Rifapentine")
      ),
      class = "comed-radioButtonsPK bslib-gap-spacing html-fill-item html-fill-container")
  }

  coMedControlsQT <- function(regimen_num) {
    div(
      radioButtons(inputId = paste0("IE_", regimen_num, "_QT"), 
                   label = NULL,
                   choices = c("None" = "None",
                               "Clofazimine" = "Clofazimine", 
                               "Moxifloxacin" = "Moxifloxacin", 
                               "Both" = "Both")
      ),
      class = "comed-radioButtonsQT bslib-gap-spacing html-fill-item html-fill-container")
  }
  
  # Optional conditionalPanel for regimens other than 1
  if (addition_RG) 
    conditionalPanel(
      condition = paste0("input.RG", regimen_num, " == true"),
      card(
        max_height = "320px",
        card_header(
          paste("Regimen", regimen_num), 
          style = paste0("font-size: 16px; background-color: ", background_color, "; ")
        ),
        div(
          card(
            card_header(
              "PK", 
              style = paste0("font-size: 14px; background-color: ", background_color, "; ")
            ), 
            coMedControlsPK(regimen_num),
            style = "padding: 6px; margin-bottom: 12px;"  # Added margin-bottom
          ),
          card(
            card_header(
              "QT", 
              style = paste0("font-size: 14px; background-color: ", background_color, "; ")
            ), 
            coMedControlsQT(regimen_num),
            style = "padding: 6px; margin-bottom: 0;"
          ),
          style = "column-count: 2;" # CSS for 2-column layout
        )
      )
    )
  else 
    card(
      max_height = "320px",
      style = "overflow: hidden;",
      card_header(
        paste("Regimen", regimen_num), 
        style = paste0("font-size: 16px; background-color: ", background_color, "; ")
      ),
      div(
        card(
          card_header(
            "PK", 
            style = paste0("font-size: 14px; background-color: ", background_color, "; ")
          ), 
          coMedControlsPK(regimen_num),
          style = "padding: 6px; margin-bottom: 12px;"  # Added margin-bottom
        ),
        card(
          card_header(
            "QT", 
            style = paste0("font-size: 14px; background-color: ", background_color, "; ")
          ), 
          coMedControlsQT(regimen_num),
          style = "padding: 6px; margin-bottom: 0;"
        ),
        style = "column-count: 2;" # CSS for 2-column layout
      )
    )
}


# Function to create Population tabPanel
mainTabPopulation <- tabPanel(
  "Population",
  br(),
  page_fillable(
    layout_columns(
      style = "display: grid; grid-template-columns: 62.5% 37.5%; gap: 1rem;",
      card(
        card_header(
          tags$style(HTML("
                          .shiny-options-group {
                            margin-top: 0 !important;
                          }
                          .card-body.bslib-gap-spacing {
                            margin: 0 !important;
                          }
                          .form-group.shiny-input-radiogroup {
                            margin-bottom: 0 !important;
                          }
                          .bslib-card .card-body {
                            overflow: unset;
                          }
                          /* Set font size for numeric inputs */
                          .form-control {
                            font-size: 13px !important;
                          }
                          /* Set font size for radio buttons in PK and QT sections */
                          .comed-radioButtonsPK .radio label,
                          .comed-radioButtonsQT .radio label {
                            font-size: 13px !important;
                          }
                          /* Set font size for reset buttons */
                          .btn-sm {
                            font-size: 13px !important;
                          }
                          /* Remove spinner buttons from numeric inputs */
                          input[type='number']::-webkit-inner-spin-button,
                          input[type='number']::-webkit-outer-spin-button {
                            -webkit-appearance: none;
                            margin: 0;
                          }
                          input[type='number'] {
                            -moz-appearance: textfield; /* Firefox */
                          }
                          /* Set font size for select input options */
                          .selectize-dropdown-content .option,
                          .selectize-input .item {
                            font-size: 13px !important;
                          }
                          
                          /* Style for centered text in default view */
                          .default-text {
                            font-size: 12px;
                            line-height: 1.5;
                            color: #555555;
                          }

                          /* Style for dataset radio buttons */
                          #dataset_source .radio-inline {
                            font-size: 14px !important;
                          }
                          
                          /* Remove margin from radio group */
                          #dataset_source {
                            margin: 0 !important;
                          }

                          .semi-transparent-overlay {
                            position: absolute;
                            top: 0;
                            left: 0;
                            width: 100%;
                            height: 100%;
                            background-color: rgba(255, 255, 255, 0.8);
                            z-index: 2;
                          }
                          .overlay-container {
                            position: relative;
                          }

                          /* Set font size for file input browse button */
                          .form-file-button {
                            font-size: 13px !important;
                          }
                          /* Set font size for file input text */
                          .form-file-text {
                            font-size: 13px !important;
                          }
                          /* Set font size for default buttons */
                          .btn-default {
                            font-size: 13px !important;
                          }
                          .shiny-input-container {
                            margin-bottom: 0 !important;
                          }
                    ")),
          class = "d-flex",
          style = "background-color: #CDD8DA;", 
          div("Covariates", 
              class = "me-auto p-0", 
              style = "font-size: 20px; "),
          div(
            class = "pe-2 header-radio", 
            style = "font-size: 14px;",
            radioButtons("population_radio", label = NULL, choices = c("Individual", "Population"), inline = TRUE))
        ),
        card_body(
          div(
            class = "panel-container",
            
            # Conditional content based on radio selection
            conditionalPanel(
              condition = "input.population_radio == 'Individual'",
              fluidRow(
                # Original Individual view content
                column(6,
                       card(
                         card_header("Continuous Covariates", style = "font-size: 16px; background-color: #CDD8DA4D;"),
                         card_body(
                           style = "font-weight: bold;",
                           numericInput("AGE", 
                             label = tags$span(style="font-size: 13px; font-weight: bold;", "Age (years)"), 
                             value = 32, min = 15, max = 100),
                           numericInput("WT", 
                             label = tags$span(style="font-size: 13px; font-weight: bold;", "Baseline Body Weight (kg)"), 
                             value = 56.6, min = 30, max = 1400, step = 0.1),
                           numericInput("ALB", 
                             label = tags$span(style="font-size: 13px; font-weight: bold;", "Baseline Albumin Concentration (g/dL)"), 
                             value = 3.65, min = 1, max = 300, step = 0.01),
                           numericInput("CACOR", 
                             label = tags$span(style="font-size: 13px; font-weight: bold;", "Baseline Corrected Calcium Level (IU/L)"), 
                             value = 2.44, min = 0.1, max = 20, step = 0.01),
                           numericInput("K", 
                             label = tags$span(style="font-size: 13px; font-weight: bold;", "Baseline Potassium Level (IU/L)"), 
                             value = 4.2, min = 0.1, max = 20, step = 0.1),
                           numericInput("MTTP", 
                             label = tags$span(style="font-size: 13px; font-weight: bold;", "Baseline Time-to-positivity in MGIT Culture (days)"), 
                             value = 6.8, min = 0.1, max = 42, step = 0.1)
                         )
                       )
                ),
                column(6,
                       card(
                         style = "overflow: visible;",
                         card_header("Categorical Covariates", style = "font-size: 16px; background-color: #CDD8DA4D;"),
                         card_body(
                           style = "overflow: visible;",
                           selectInput("SEX", 
                             label = tags$span(style="font-size: 13px; font-weight: bold;", "Sex"), 
                             choices = c("Male", "Female")),
                           selectInput("RACE", 
                             label = tags$span(style="font-size: 13px; font-weight: bold;", "Race"), 
                             choices = c("Non-Black", "Black")),
                           selectInput("XDR", 
                             label = tags$span(style="font-size: 13px; font-weight: bold;", "Drug Resistance"), 
                             choices = c("MDR-TB", "pre-XDR-TB", "XDR-TB"))
                         )
                       )
                )
              )
            ),
            
            # New Population view content
            conditionalPanel(
              condition = "input.population_radio == 'Population'",
              fluidRow(
                # First row: Full-width cards for Covariates info
                column(12,
                  # Dataset card
                  card(
                    style = "background-color: #CDD8DA4D;",
                    card_header(
                      class = "d-flex",
                      style = "background-color: #CDD8DA4D;",
                      div("Dataset", 
                          class = "me-auto", 
                          style = "font-size: 16px;"),
                      div(
                        class = "pe-2",
                        radioButtons(
                          "dataset_source",
                          label = NULL,
                          choices = c("Default", "Import"),
                          inline = TRUE,
                          selected = "Default"
                        )
                      )
                    ),
                    card_body(
                      conditionalPanel(
                        condition = "input.dataset_source == 'Default'",
                        div(
                          style = "font-size: 14px; font-weight: bold; padding: 6px; height: 100%;",
                          "A virtual population is generated by conditional distribution modelling 
                          (Smania and Jonsson, CPT:PSP, 2021). Distribution of covariates are comparable 
                          to a total of 556 participants in three TB clinical trials (two phase IIb trials
                          [NCT01498419, NCT02193776] and one phase III trial [NCT02333799]). Trial Data are 
                          available in the TB-PACTS data platform (https://c-path.org/tools-platforms/tb-pacts/).
                          "
                        )
                      ),
                      conditionalPanel(
                        condition = "input.dataset_source == 'Import'",
                        div(
                          style = "display: flex; flex-direction: column; height: 100%;",
                          # Top section with template and upload
                          div(
                            style = "display: flex; gap: 1rem; margin-bottom: 1rem;",
                            # Download template section (1/3)
                            div(
                              style = "flex: 1; padding: 1rem; background-color: #CDD8DA5D; border-radius: 4px;",
                              div(
                                style = "margin-bottom: 0.5rem;",
                                h6("Virtual Population Template", 
                                   style = "font-size: 13px; font-weight: bold; margin: 0; padding: 6px 0;")
                              ),
                              downloadButton("download_template", "Download .csv template", class = "btn-sm")
                            ),
                            # Upload dataset section (2/3)
                            div(
                              style = "flex: 2; padding: 1rem; background-color: #CDD8DA5D; border-radius: 4px;",
                              fileInput(
                                "uploaded_data",
                                label = h6("Upload Dataset (CSV format)", 
                                          style = "font-size: 13px; font-weight: bold; margin: 0;"),
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv"
                                ),
                                width = "100%"
                              )
                            )
                          ),
                          # Instructions section
                          div(
                            style = "font-size: 13px; padding: 1rem; background-color: #CDD8DA5D; border-radius: 4px;",
                            tags$p(
                              style = "margin-bottom: 0.5rem; font-weight: bold;",
                              "Instructions:"
                            ),
                            tags$ol(
                              style = "margin-bottom: 0;",
                              tags$li("Download the template CSV file"),
                              tags$li("Fill in your population data following the template format"),
                              tags$li("Upload your completed CSV file"),
                              tags$li("Ensure all required columns are present and data types match the template")
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                # Second row: Original Population view content
                column(5,
                  div(
                    class = "overlay-container",
                    # Add overlay that shows only when Import is selected
                    conditionalPanel(
                      condition = "input.dataset_source == 'Import'",
                      div(class = "semi-transparent-overlay")
                    ),
                    # Original cards
                    card(
                      card_header("Drug Resistance Profile", style = "font-size: 16px; background-color: #CDD8DA4D;"),
                      card_body(
                        numericInput("popMDR", label = tags$span(style="font-size: 13px; font-weight: bold;","% of Drug Sensitive or MDR-TB"), value = 70, min = 1, max = 100),
                        numericInput("poppXDR", label = tags$span(style="font-size: 13px; font-weight: bold;","% of pre-XDR-TB"), value = 20, min = 1, max = 100),
                        numericInput("popXDR", label = tags$span(style="font-size: 13px; font-weight: bold;","% of XDR-TB"), value = 10, min = 1, max = 100)
                      )
                    ),
                    card(
                      card_header("Race", style = "font-size: 16px; background-color: #CDD8DA4D;"),
                      card_body(
                        numericInput("popRACE", label = tags$span(style="font-size: 13px; font-weight: bold;","% of Black Race"), value = 40, min = 1, max = 100)
                      )
                    )
                  )
                ),
                column(7,
                  div(
                    class = "overlay-container",
                    # Add overlay that shows only when Import is selected
                    conditionalPanel(
                      condition = "input.dataset_source == 'Import'",
                      div(class = "semi-transparent-overlay")
                    ),
                    # Original Population Details card
                    card(
                      card_header("Population Details", style = "font-size: 16px; background-color: #CDD8DA4D;"),
                      card_body(
                        style = "font-weight: bold;",
                        # Header row
                        fluidRow(
                          column(4, "Covariates", style = "font-size: 15px; font-weight: bold;"),
                          column(3, h6("Min", style = "text-align: center; font-size: 15px; font-weight: bold;")),
                          column(3, h6("Max", style = "text-align: center; font-size: 15px; font-weight: bold;")),
                          column(2, "")  # Space for reset buttons
                        ),
                        # Age row
                        fluidRow(
                          column(4, "Age (years)", style = "font-size: 13px; font-weight: bold; padding-top: 7px;"),
                          column(3, numericInput("AGE_min", NULL, value = 17, min = 0, max = 100)),
                          column(3, numericInput("AGE_max", NULL, value = 69, min = 0, max = 100)),
                          column(2, actionButton("reset_AGE", "Reset", class = "btn-sm"), style = "padding-left: 0px;")
                        ),
                        
                        # Weight row
                        fluidRow(
                          column(4, "Weight (kg)", style = "font-size: 13px; font-weight: bold; padding-top: 7px;"),
                          column(3, numericInput("WT_min", NULL, value = 29, min = 0, max = 140)),
                          column(3, numericInput("WT_max", NULL, value = 112, min = 0, max = 140)),
                          column(2, actionButton("reset_WT", "Reset", class = "btn-sm"), style = "padding-left: 0px;")
                        ),
                        
                        # Albumin row
                        fluidRow(
                          column(4, "Albumin (g/dL)", style = "font-size: 13px; font-weight: bold; padding-top: 7px;"),
                          column(3, numericInput("ALB_min", NULL, value = 1.9, min = 0, max = 300)),
                          column(3, numericInput("ALB_max", NULL, value = 5.0, min = 0, max = 300)),
                          column(2, actionButton("reset_ALB", "Reset", class = "btn-sm"), style = "padding-left: 0px;")
                        ),
                        
                        # Calcium row
                        fluidRow(
                          column(4, "Calcium (IU/L)", style = "font-size: 13px; font-weight: bold; padding-top: 7px;"),
                          column(3, numericInput("CACOR_min", NULL, value = 2.00, min = 0, max = 20)),
                          column(3, numericInput("CACOR_max", NULL, value = 3.08, min = 0, max = 20)),
                          column(2, actionButton("reset_CACOR", "Reset", class = "btn-sm"), style = "padding-left: 0px;")
                        ),
                        
                        # Potassium row
                        fluidRow(
                          column(4, "Potassium (IU/L)", style = "font-size: 13px; font-weight: bold; padding-top: 7px;"),
                          column(3, numericInput("K_min", NULL, value = 3.20, min = 0, max = 20)),
                          column(3, numericInput("K_max", NULL, value = 6.26, min = 0, max = 20)),
                          column(2, actionButton("reset_K", "Reset", class = "btn-sm"), style = "padding-left: 0px;")
                        ),
                        
                        # MGIT TTP row
                        fluidRow(
                          column(4, "MGIT TTP (days)", style = "font-size: 13px; font-weight: bold; padding-top: 7px;"),
                          column(3, numericInput("MTTP_min", NULL, value = 1.3, min = 0, max = 42)),
                          column(3, numericInput("MTTP_max", NULL, value = 42, min = 0, max = 42)),
                          column(2, actionButton("reset_MTTP", "Reset", class = "btn-sm"), style = "padding-left: 0px;")
                        ),
                        
                        # Sex row (special case with only one input)
                        fluidRow(
                          column(4, "Sex (% Female)", style = "font-size: 13px; font-weight: bold; padding-top: 7px;"),
                          column(6, numericInput("SEX_female", NULL, value = 50, min = 0, max = 100)),
                          column(2, actionButton("reset_SEX", "Reset", class = "btn-sm"), style = "padding-left: 0px;")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # Second card with custom background color and styled card header/body
      card(
        card_header("Concomitant Medication", style = "font-size: 20px; background-color: #CDD8DA;"  # Using card_header for the title
        ),
        card_body(
          CoMedColumn(1, "#CBCAE38D", default_LD = TRUE, addition_RG = FALSE), 
          CoMedColumn(2, "#E1C3C88D", default_LD = FALSE, addition_RG = TRUE),
          CoMedColumn(3, "#C1D4D78D", default_LD = FALSE, addition_RG = TRUE), 
          CoMedColumn(4, "#E7D7CB8D", default_LD = FALSE, addition_RG = TRUE)  
        )
      )
    ) # end of page_fillable
  ) # end of population tabPanel
) # end of population tabPanel
