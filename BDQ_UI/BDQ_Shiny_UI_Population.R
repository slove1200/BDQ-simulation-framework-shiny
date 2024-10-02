#### This is for UI - Population tab ####
# Function to create the UI for a regimen column with coMed controls in Population tab
CoMedColumn <- function(regimen_num, background_color, default_LD = FALSE, addition_RG = TRUE) {
  
  # Helper to create the coMed controls
  coMedControls <- function(regimen_num) {
    div(
      checkboxGroupInput(inputId = paste0("IE_", regimen_num), 
                         label = NULL,
                         choices = c("None" = "None", 
                                     "Clofazimine" = "Clofazimine", 
                                     "Efavirenz" = "Efavirenz", 
                                     "Lopinavir/r" = "Lopinavir/r", 
                                     "Moxifloxacin" = "Moxifloxacin", 
                                     "Nevirapine" = "Nevirapine", 
                                     "Rifampicin" = "Rifampicin", 
                                     "Rifapentine" = "Rifapentine")
      ),
      class = "comed-checkbox")
  }
  
  # Optional conditionalPanel for regimens other than 1
  if (addition_RG) 
    conditionalPanel(
      condition = paste0("input.RG", regimen_num, " == true"),
      card(
        max_height = "200px",
        style = "overflow: unset;",  # Prevent overflow
        card_header(
          paste("Regimen", regimen_num), 
          style = paste0("font-size: 18px; background-color: ", background_color, "; ")
        ),
        div(
          coMedControls(regimen_num),
          style = "column-count: 2;" # CSS for 2-column layout
        )
      ),
      tags$head(
        # JavaScript to handle "None" blocking other options
        tags$script(HTML(paste0(
          "
          $(document).on('change', 'input[name=\"IE_", regimen_num, "\"]', function() {
            var selectedValues = $('input[name=\"IE_", regimen_num, "\"]:checked').map(function() {
              return this.value;
            }).get();

            if (selectedValues.includes('None')) {
              $('input[name=\"IE_", regimen_num, "\"]').not('[value=\"None\"]').prop('disabled', true);
              $('input[name=\"IE_", regimen_num, "\"]').not('[value=\"None\"]').prop('checked', false);
            } else {
              $('input[name=\"IE_", regimen_num, "\"]').prop('disabled', false);
            }
          });
          "
        )))
      )
    )
  else 
    card(
      max_height = "200px",
      style = "overflow: unset;",  # Prevent overflow
      card_header(
        paste("Regimen", regimen_num), 
        style = paste0("font-size: 18px; background-color: ", background_color, "; ")
      ),
      style = "overflow: unset;",
      div(
        coMedControls(regimen_num),
        style = "column-count: 2"  # CSS for 2-column layout
      ),
      tags$head(
        # JavaScript to handle "None" blocking other options
        tags$script(HTML(paste0(
          "
          $(document).on('change', 'input[name=\"IE_", regimen_num, "\"]', function() {
            var selectedValues = $('input[name=\"IE_", regimen_num, "\"]:checked').map(function() {
              return this.value;
            }).get();

            if (selectedValues.includes('None')) {
              $('input[name=\"IE_", regimen_num, "\"]').not('[value=\"None\"]').prop('disabled', true);
              $('input[name=\"IE_", regimen_num, "\"]').not('[value=\"None\"]').prop('checked', false);
            } else {
              $('input[name=\"IE_", regimen_num, "\"]').prop('disabled', false);
            }
          });
          "
        )))
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
                          div.header-checkbox div.checkbox {
                            margin-bottom: 0;
                          }
                          div.header-checkbox > div.shiny-input-container {
                            width: unset;
                          }
                    ")),
          class = "d-flex mb-3",
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
          conditionalPanel(
            condition = "input.population_radio == 'Individual'",
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
                         # median 163 hrs, remember to change input (as model use hours); also MTTP2 or ??
                         numericInput("MTTP2", label = "Baseline Time-to-positivity in MGIT Culture (days)", value = 6.8, min = 0.1, max = 42, step = 0.1) 
                       )
                     )
              ),
              # Column for categorical covariates
              column(6,
                     card(
                       card_header("Categorical Covariates", style = "font-size: 18px; background-color: #CDD8DA4D;"),
                       card_body(
                         selectInput("SEX", label = tags$span(style="font-weight: bold;","Sex"), choices = c("Male", "Female")),
                         selectInput("RACE", label = tags$span(style="font-weight: bold;","Race"), choices = c("Non-Black", "Black")),
                         selectInput("XDR", label = tags$span(style="font-weight: bold;","Drug Resistance"), choices = c("MDR-TB", "pre-XDR-TB", "XDR-TB"))
                       )
                     )
              )
            )
          ), # first conditional panel individual clicked
          conditionalPanel(
            condition = "input.population_radio == 'Population'",
            fluidRow(

              column(12, 
                     "     Tab for default population distribtuion created given numbers of individuals", 
                     br(), 
                     br()), 
              # Column for continuous covariates
              column(6,
                     card(
                       card_header("Continuous Covariates", style = "font-size: 18px; background-color: #CDD8DA4D;"),
                       card_body(
                         style = "font-weight: bold;",
                         "Probably choose min, max, median, SD of the values and shape of distribution?"
                       )
                     )
              ),
              # Column for categorical covariates
              column(6,
                     card(
                       card_header("Categorical Covariates", style = "font-size: 18px; background-color: #CDD8DA4D;"),
                       card_body(
                         style = "font-weight: bold;",
                         "Probably choose percentage of each category?"
                       )
                     )
              )
            )
          ) # second conditional panel if population clicked
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
      
      # Third card with custom background color and styled card header/body
      card(
        card_header("Advanced Settings", style = "font-size: 22px; background-color: #D8BFD8;"  # Using card_header for the title
        ),
        card_body(
          fluidRow(
            # Column for Background therapy
            column(6,
                   card(
                     card_header("Background Therapy", style = "font-size: 18px; background-color: #D8BFD84D;"),
                     card_body(
                       style = "font-weight: bold;",
                       numericInput("HL1", label = "Half-life of Bacterial Clearance (%) Lower", value = 20, min = -300, max = 300, width = "70%"),
                       "***HL and surge peak are highly correlated"
                     )
                   )
            ),
            # Column for Model settings
            column(6,
                   card(
                     card_header("Model Settings", style = "font-size: 18px; background-color: #D8BFD84D;"),
                     card_body(
                       selectInput("STUDY", label = tags$span(style="font-weight: bold;","Type of Model for PK-TTP"), choices = c("Treatment-naïve", "Treatment-experienced")),
                       numericInput("REP", label = tags$span(style="font-weight: bold;","Numbers of Culture Replicates"), value = 1, min = 1, max = 3),
                       numericInput("SA1", label = tags$span(style="font-weight: bold;","Peak of Conversion (%) Faster"), value = 20, min = -300, max = 300)
                     )
                   )
            )
          ),
          "Additional content for Advanced settings goes here."
        )
      ),
      col_widths = c(8, 4, 8)#,
      #row_heights = c(5, 3)
    )
  ) # end of page_fillable
) # end of population tabPanel  
