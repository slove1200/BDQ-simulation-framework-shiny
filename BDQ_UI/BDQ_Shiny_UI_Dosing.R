#### This is for UI - Dosing tab ####

# Helper function to create dose input controls
doseControls <- function(regimen_num) {
  div(
      conditionalPanel(
        condition = paste0("input.LD", regimen_num, " == true"),
        h6("Loading dose", style = "font-weight: bold;"),
        tags$div(
          style = "margin-top: 1rem;",
          numericInput(paste0("ldose_", regimen_num), label = "Loading dose of BDQ (mg)", value = 400, min = 100, max = 20000)
        ),
        tags$div(
          style = "margin-top: 1rem;",
          numericInput(paste0("ldur_", regimen_num), label = "Loading dose duration", value = 2, min = 1, max = 20000)
        ),
        tags$div(
          style = "margin-top: 1rem;",
          selectInput(paste0("lunit_", regimen_num), label = "Unit", c("week" = "2", "day" = "1"), selected = "week")
        ),
        tags$div(
          style = "margin-top: 1rem;",
          selectInput(paste0("lfreq_", regimen_num), label = "Loading dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly"), selected = "Once daily")
        )
      ),
      br(),
      h6("Maintenance dose", style = "font-weight: bold;"),
      tags$div(
        style = "margin-top: 1rem;",
        numericInput(paste0("mdose_", regimen_num), label = "Maintenance dose of BDQ (mg)", value = 200, min = 100, max = 20000)
      ),
      tags$div(
        style = "margin-top: 1rem;",
        numericInput(paste0("mdur_", regimen_num), label = "Maintenance dose duration", value = 22, min = 1, max = 20000)
      ),
      tags$div(
        style = "margin-top: 1rem;",
        selectInput(paste0("munit_", regimen_num), label = "Unit", c("week" = "2", "day" = "1"), selected = "week")
      ),
      tags$div(
        style = "margin-top: 1rem;",
        selectInput(paste0("mfreq_", regimen_num), label = "Maintenance dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly"), selected = "Three times weekly")
      )
    )
}

# Function to create a regimen card
regimenCard <- function(regimen_num, background_color, default_LD = FALSE, addition_RG = TRUE) {
  card_content <- card(
    card_header(
      paste("Regimen", regimen_num),
      style = paste0("font-size: 20px; background-color: ", background_color, ";")
    ),
    card_body(
      style = paste0("background-color: ", background_color, "8D;"),
      checkboxInput(paste0("LD", regimen_num), "Add loading dose", value = default_LD),
      doseControls(regimen_num)
    )
  )
  
  if (addition_RG) {
    conditionalPanel(
      condition = paste0("input.RG", regimen_num, " == true"),
      card_content,
      style = "display: none;"
    )
  } else {
    card_content
  }
}

# Main Dosing tab
mainTabDosing <- tabPanel(
  "Dosing",
  br(),
  tags$script(HTML("
    $(document).ready(function() {
      $('#RG2').on('change', function() {
        if (!this.checked && $('#RG3').prop('checked')) {
          this.checked = true;
          alert('Please uncheck Regimen 3 first before unchecking Regimen 2.');
        }
      });
    });
  ")),
  page_fillable(
    tags$div(
      style = "display: grid; grid-template-columns: 15% 26% 26% 26%; gap: 25px;",
      # Add Regimens card
      div(
        style = "align-self: start;",
        card(
          class = "add-regimens-card",
          card_header("Add Regimens", style = "font-size: 20px; background-color: #F5F5F5;"),
          card_body(
            checkboxInput("RG2", "Regimen 2", value = FALSE),
            conditionalPanel(
              condition = "input.RG2 == true",
              checkboxInput("RG3", "Regimen 3", value = FALSE)
            )
          ),
          style = "width: 100%;"
        )
      ),
      # Regimen Cards
      regimenCard(1, "#CBCAE3", default_LD = TRUE, addition_RG = FALSE),
      regimenCard(2, "#E1C3C8", default_LD = FALSE, addition_RG = TRUE),
      regimenCard(3, "#C1D4D7", default_LD = FALSE, addition_RG = TRUE)
    )
  )
)