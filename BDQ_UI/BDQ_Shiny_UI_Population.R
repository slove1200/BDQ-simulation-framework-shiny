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
          style = paste0("font-size: 18px; background-color: ", background_color, "; ")
        ),
        div(
          card(card_header(
            "PK", 
            style = paste0("font-size: 16px; background-color: ", background_color, "; ")), 
            coMedControlsPK(regimen_num)
            ),
          card(card_header(
            "QT", 
            style = paste0("font-size: 16px; background-color: ", background_color, "; ")), 
            coMedControlsQT(regimen_num)
            ),
          style = "column-count: 2;" # CSS for 3-column layout
        )
      )
    )
  else 
    card(
      max_height = "320px",
      style = "overflow: hidden;",  # Prevent overflow
      card_header(
        paste("Regimen", regimen_num), 
        style = paste0("font-size: 18px; background-color: ", background_color, "; ")
      ),
      style = "overflow: hidden;",
      div(
        card(card_header(
          "PK", 
          style = paste0("font-size: 16px; background-color: ", background_color, "; ")), 
          coMedControlsPK(regimen_num), 
          style = "padding: 6px; margin-bottom: 0;"),
        card(card_header(
          "QT", 
          style = paste0("font-size: 16px; background-color: ", background_color, "; ")), 
          coMedControlsQT(regimen_num), 
          style = "padding: 6px; margin-bottom: 0;"),
        style = "column-count: 2;" # CSS for 3-column layout
      )
    )
}


# Function to create Population tabPanel
mainTabPopulation <- tabPanel(
  "Population",
  br(),
  page_fillable(
    layout_columns(
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
                          .semi-transparent-overlay {
                            position: absolute;
                            top: 0;
                            left: 0;
                            width: 100%;
                            height: 100%;
                            background-color: rgba(255, 255, 255, 0.7); /* Semi-transparent background */
                            z-index: 2;
                            display: flex;
                            justify-content: center;
                            align-items: center; /* Center the text */
                          }
                            .overlay-text {
                            background-color: rgba(200, 200, 200, 0.8); /* Light gray background for text */
                            padding: 20px; /* Add some padding */
                            border-radius: 5px; /* Optional: rounded corners */
                            font-size: 18px; /* Adjust the font size */
                            color: #555555; /* Text color */
                            font-weight: bold;
                          }
                          .panel-container {
                            position: relative; /* To ensure the overlay stays on top of the content */
                          }
                    ")),
          class = "d-flex",
          style = "background-color: #CDD8DA;", 
          div("Covariates", 
              class = "me-auto p-0", 
              style = "font-size: 22px; "),
          div(
            class = "pe-2 header-radio", 
            style = "font-size: 16px;",
            radioButtons("population_radio", label = NULL, choices = c("Individual", "Population"), inline = TRUE))
        ),
        card_body(
          div(
            class = "panel-container",
            
            # Overlay appears when "Population" is clicked
            conditionalPanel(
              condition = "input.population_radio == 'Population'",
              # Adding texts on Population panel
              div(class = "semi-transparent-overlay",
                  div(class = "overlay-text", "A virtual population is generated by 
                      conditional distribution modelling (Smania and Jonsson, CPT:PSP, 2021).
                      Distribution of covariates are comparable to participants in TMC207-C208 
                      (NCT00449644) and TMC207-C209 (NCT00910871) Phase II clinical trials 
                      sponsored by Janssen Research & Development.")
              )
            ),
            
            # Shared content between Individual and Population
            fluidRow(
              # Column for continuous covariates
              column(6,
                     card(
                       card_header("Continuous Covariates", style = "font-size: 18px; background-color: #CDD8DA4D;"),
                       card_body(
                         style = "font-weight: bold;",
                         numericInput("AGE", label = "Age (years)", value = 32, min = 15, max = 100),
                         numericInput("WT", label = "Baseline Body Weight (kg)", value = 56.6, min = 30, max = 1400, step = 0.1),
                         numericInput("ALB", label = "Baseline Albumin Concentration (g/dL)", value = 3.65, min = 1, max = 300, step = 0.01),
                         numericInput("CACOR", label = "Baseline Corrected Calcium Level (IU/L)", value = 2.44, min = 0.1, max = 20, step = 0.01),
                         numericInput("K", label = "Baseline Potassium Level (IU/L)", value = 4.2, min = 0.1, max = 20, step = 0.1),
                         numericInput("MTTP", label = "Baseline Time-to-positivity in MGIT Culture (days)", value = 6.8, min = 0.1, max = 42, step = 0.1)
                       )
                     )
              ),
              # Column for categorical covariates
              column(6,
                     card(
                       style = "overflow: visible;",  # Add this to allow overflow on the card
                       card_header("Categorical Covariates", style = "font-size: 18px; background-color: #CDD8DA4D;"),
                       card_body(
                         style = "overflow: visible;",  # Add this to allow overflow on the card
                         selectInput("SEX", label = tags$span(style="font-weight: bold;","Sex"), choices = c("Male", "Female")),
                         selectInput("RACE", label = tags$span(style="font-weight: bold;","Race"), choices = c("Non-Black", "Black")),
                         selectInput("XDR", label = tags$span(style="font-weight: bold;","Drug Resistance"), choices = c("MDR-TB", "pre-XDR-TB", "XDR-TB"))
                       )
                     )
              )
            )
          )
        )
      ),
      
      # Second card with custom background color and styled card header/body
      card(
        card_header("Concomitant Medication", style = "font-size: 22px; background-color: #CDD8DA;"  # Using card_header for the title
        ),
        card_body(
          CoMedColumn(1, "#CBCAE38D", default_LD = TRUE, addition_RG = FALSE), 
          CoMedColumn(2, "#E1C3C88D", default_LD = FALSE, addition_RG = TRUE),
          CoMedColumn(3, "#C1D4D78D", default_LD = FALSE, addition_RG = TRUE), 
          CoMedColumn(4, "#E7D7CB8D", default_LD = FALSE, addition_RG = TRUE)  
        )
      ),
      col_widths = c(7, 5)#,
      #row_heights = c(5, 3)
    )
  ) # end of page_fillable
) # end of population tabPanel
