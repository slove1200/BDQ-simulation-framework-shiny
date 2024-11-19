#### This is for UI - Dosing tab ####

# Function to create the UI for a regimen column ####
regimenColumn <- function(regimen_num, background_color, default_LD = FALSE, addition_RG = TRUE) {
  
  # Helper to create the dose controls
  doseControls <- function(regimen_num) {
    div(
      conditionalPanel(
        condition = paste0("input.LD", regimen_num, " == true"),
        h4("Loading dose"),
        numericInput(paste0("ldose_", regimen_num), label = "Loading dose of BDQ (mg)", value = 400, min = 100, max = 20000),
        numericInput(paste0("ldur_", regimen_num), label = "Loading dose duration", value = 2, min = 1, max = 20000),
        selectInput(paste0("lunit_", regimen_num), label = "Unit", c("week" = "2", "day" = "1"), selected = "week"),
        selectInput(paste0("lfreq_", regimen_num), 
                    label = "Loading dose frequency", 
                    c("Twice daily", "Once daily", "Three times weekly", "Once weekly"), 
                    selected = "Once daily"
        )
      ),
      br(),
      h4("Maintenance dose"),
      numericInput(paste0("mdose_", regimen_num), label = "Maintenance dose of BDQ (mg)", value = 200, min = 100, max = 20000),
      numericInput(paste0("mdur_" , regimen_num), label = "Maintenance dose duration", value = 22, min = 1, max = 20000),
      selectInput(paste0("munit_" , regimen_num), label = "Unit", c("week" = "2", "day" = "1"), selected = "week"),
      selectInput(paste0("mfreq_" , regimen_num),
                  label = "Maintenance dose frequency", 
                  c("Twice daily", "Once daily", "Three times weekly", "Once weekly"),
                  selected = "Three times weekly"
      )
    )
  }
  
  # Column layout
  column(
    width = 3, 
    style = "width: 26%;",
    
    # Optional conditionalPanel for regimens other than 1
    if (addition_RG) conditionalPanel(
      condition = paste0("input.RG", regimen_num, " == true"),
      div(
        style = paste0("background-color: ", background_color, "; padding: 15px; border-radius: 10px;"),
        h3(paste("Regimen", regimen_num)),
        br(),
        checkboxInput(paste0("LD", regimen_num), "Add loading dose", value = default_LD),
        doseControls(regimen_num)
      ), 
      # When calling a conditionalPanel a div tag is created. 
      # This div tag by default is visible - once the condition is checked it's hidden.
      # To hide it right from the start we can add a style attribute setting "display: none;" for conditionalPanel.
      style = "display: none;"
    ) else div(
      style = paste0("background-color: ", background_color, "; padding: 15px; border-radius: 10px;"),
      h3(paste("Regimen", regimen_num)),
      br(),
      checkboxInput(paste0("LD", regimen_num), "Add loading dose", value = default_LD),
      doseControls(regimen_num)
    )
  )
}


# Function to create Dosing tabPanel
mainTabDosing <- tabPanel(
  "Dosing",
  br(),
  fluidRow(
    
    sidebarPanel(
      width = 2, 
      h4("Add regimen"), 
      br(),
      
      # Checkbox for Regimen 2
      checkboxInput("RG2", "Regimen 2", value = FALSE),  # Checkbox to show/hide Regimen 2
      
      # Conditionally show Regimen 3 only if Regimen 2 is checked
      conditionalPanel(
        condition = "input.RG2 == true", 
        checkboxInput("RG3", "Regimen 3", value = FALSE),  # Checkbox to show/hide Regimen 3
        # Conditionally show Regimen 4 only if Regimen 3 is checked
        style = "display: none;"
      ),
      style = "border-radius: 15px;"
    ), # end of sidebarPanel Add regimen
    
    # Always display Regimen 1 (no condition for Regimen 1)
    regimenColumn(1, "#CBCAE3", default_LD = TRUE, addition_RG = FALSE), 
    regimenColumn(2, "#E1C3C8", default_LD = FALSE, addition_RG = TRUE),
    regimenColumn(3, "#C1D4D7", default_LD = FALSE, addition_RG = TRUE)
  ) # end of first fluidRow
) # end of tab Dosing column