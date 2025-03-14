#### This is for UI - Dosing tab ####

# Helper function to create dose input controls
doseControls <- function(regimen_num) {
  div(
      conditionalPanel(
        condition = paste0("input.LD", regimen_num, " == true"),
        h6("Loading dose", style = "font-weight: bold;"),
        tags$div(
          style = "margin-top: 1rem;",
          numericInput(paste0("ldose_", regimen_num), label = "Loading dose of bedaquiline (mg)", value = 400, min = 100, max = 20000)
        ),
        tags$div(
          style = "margin-top: 1rem;",
          numericInput(paste0("ldur_", regimen_num), label = "Loading dose duration", value = 2, min = 1, max = 20000)
        ),
        tags$div(
          style = "margin-top: 1rem;",
          selectInput(paste0("lunit_", regimen_num), label = "Unit", c("week" = "2", "day" = "1"), selected = "2")
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
        numericInput(paste0("mdose_", regimen_num), label = "Maintenance dose of bedaquiline (mg)", value = 200, min = 100, max = 20000)
      ),
      tags$div(
        style = "margin-top: 1rem;",
        numericInput(paste0("mdur_", regimen_num), label = "Maintenance dose duration", value = 22, min = 1, max = 20000)
      ),
      tags$div(
        style = "margin-top: 1rem;",
        selectInput(paste0("munit_", regimen_num), label = "Unit", c("week" = "2", "day" = "1"), selected = "2")
      ),
      tags$div(
        style = "margin-top: 1rem;",
        selectInput(paste0("mfreq_", regimen_num), label = "Maintenance dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly"), selected = "Three times weekly")
      ), 
      br(),
      br(),
      checkboxInput(paste0("MD2_", regimen_num), "Add maintenance dose 2", value = FALSE),
      br(),
      conditionalPanel(
        condition = paste0("input.MD2_", regimen_num, " == true"),
        h6("Maintenance dose 2", style = "font-weight: bold;"),
        tags$div(
          style = "margin-top: 1rem;",
          numericInput(paste0("m2dose_", regimen_num), label = "Maintenance dose 2 of bedaquiline (mg)", value = 200, min = 100, max = 20000)
        ),
        tags$div(
          style = "margin-top: 1rem;",
          numericInput(paste0("m2dur_", regimen_num), label = "Maintenance dose 2 duration", value = 16, min = 1, max = 20000)
        ),
        tags$div(
          style = "margin-top: 1rem;",
          selectInput(paste0("m2unit_", regimen_num), label = "Unit", c("week" = "2", "day" = "1"), selected = "2")
        ),
        tags$div(
          style = "margin-top: 1rem;",
          selectInput(paste0("m2freq_", regimen_num), label = "Maintenance dose 2 frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly"), selected = "Once daily")
        )
      )
    )
}

# Helper function to create dose interruption and restart controls
interruptionControls <- function(regimen_num) {
  div(
    conditionalPanel(
      condition = paste0("input.interrupt_", regimen_num, " == true"),
      tags$div(
        style = "margin-top: 1rem;",
        numericInput(paste0("offbdqdur_", regimen_num), label = "Time after the last bedaquiline dose", value = 8, min = 1, max = 100)
      ),
      tags$div(
        style = "margin-top: 1rem;",
        selectInput(paste0("offbdqunit_", regimen_num), label = "Unit", c("week" = "2", "day" = "1"), selected = "2")
      ),
      br(),
      tags$div(
        style = "margin-top: 1rem;",
        checkboxInput(paste0("restart_LD", regimen_num), "Add loading dose", value = FALSE)
      ),
      conditionalPanel(
        condition = paste0("input.restart_LD", regimen_num, " == true"),
        br(),
        br(),
        h6("Loading dose", style = "font-weight: bold;"),
        tags$div(
          style = "margin-top: 1rem;",
          numericInput(paste0("restart_ldose_", regimen_num), label = "Loading dose of bedaquiline (mg)", value = 500, min = 100, max = 20000)
        ),
        tags$div(
          style = "margin-top: 1rem;",
          numericInput(paste0("restart_ldur_", regimen_num), label = "Loading dose duration", value = 3, min = 1, max = 20000)
        ),
        tags$div(
          style = "margin-top: 1rem;",
          selectInput(paste0("restart_lunit_", regimen_num), label = "Unit", c("week" = "2", "day" = "1"), selected = "1")
        ),
        tags$div(
          style = "margin-top: 1rem;",
          selectInput(paste0("restart_lfreq_", regimen_num), label = "Loading dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly"), selected = "Once daily")
        )
      ),
      br(),
      h6("Maintenance dose", style = "font-weight: bold;"),
      tags$div(
        style = "margin-top: 1rem;",
        numericInput(paste0("restart_mdose_", regimen_num), label = "Maintenance dose of bedaquiline (mg)", value = 200, min = 100, max = 20000)
      ),
      tags$div(
        style = "margin-top: 1rem;",
        numericInput(paste0("restart_mdur_", regimen_num), label = "Maintenance dose duration", value = 8, min = 1, max = 20000)
      ),
      tags$div(
        style = "margin-top: 1rem;",
        selectInput(paste0("restart_munit_", regimen_num), label = "Unit", c("week" = "2", "day" = "1"), selected = "2")
      ),
      tags$div(
        style = "margin-top: 1rem;",
        selectInput(paste0("restart_mfreq_", regimen_num), label = "Maintenance dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly"), selected = "Once daily")
      ),
      br(),
      tags$div(
        style = "margin-top: 1rem;",
        checkboxInput(paste0("restart_MD2_", regimen_num), "Add maintenance dose 2", value = FALSE)
      ),
      br(),
      conditionalPanel(
        condition = paste0("input.restart_MD2_", regimen_num, " == true"),
        h6("Maintenance dose 2", style = "font-weight: bold;"),
        tags$div(
          style = "margin-top: 1rem;",
          numericInput(paste0("restart_m2dose_", regimen_num), label = "Maintenance dose 2 of bedaquiline (mg)", value = 100, min = 100, max = 20000)
        ),
        tags$div(
          style = "margin-top: 1rem;",
          numericInput(paste0("restart_m2dur_", regimen_num), label = "Maintenance dose 2 duration", value = 8, min = 1, max = 20000)
        ),
        tags$div(
          style = "margin-top: 1rem;",
          selectInput(paste0("restart_m2unit_", regimen_num), label = "Unit", c("week" = "2", "day" = "1"), selected = "week")
        ),
        tags$div(
          style = "margin-top: 1rem;",
          selectInput(paste0("restart_m2freq_", regimen_num), label = "Maintenance dose 2 frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly"), selected = "Once daily")
        )
      )
    )
  )
}

# Function to create a regimen card
regimenCard <- function(regimen_num, background_color, default_LD = FALSE, default_MD2 = FALSE, addition_RG = TRUE) {
  # Create main regimen card
  main_card <- card(
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
  
  # Create restart regimen card
  restart_card <- card(
    card_header(
      checkboxInput(paste0("interrupt_", regimen_num), "Dose interruption and restart", value = FALSE),
      style = paste0("font-size: 16px; background-color: ", background_color, ";")
    ),
    card_body(
      style = paste0("background-color: ", background_color, "8D;"),
      interruptionControls(regimen_num)
    )
  )
  
  # Combine both cards in a div
  combined_cards <- div(
    main_card,
    restart_card
  )
  
  if (addition_RG) {
    conditionalPanel(
      condition = paste0("input.RG", regimen_num, " == true"),
      combined_cards,
      style = "display: none;"
    )
  } else {
    combined_cards
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
      regimenCard(1, "#CBCAE3", default_LD = TRUE, default_MD2 = FALSE, addition_RG = FALSE),
      regimenCard(2, "#E1C3C8", default_LD = FALSE, default_MD2 = FALSE, addition_RG = TRUE),
      regimenCard(3, "#C1D4D7", default_LD = FALSE, default_MD2 = FALSE, addition_RG = TRUE)
    )
  )
)