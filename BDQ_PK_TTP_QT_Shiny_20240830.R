# Title : PK Shiny app
######## .
# Load libraries ####
library(mrgsolve)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(shiny)
library(grid)
library(gridExtra)
library(DT)


# ui ####
ui <-
  fluidPage(
    navbarPage(
      "BEDAQUILINE DOSE REGIMEN",
      tabsetPanel(
        id = "mainTab", selected = "Single Regimen",
        tabPanel(
          "About",
          tabsetPanel(
            id = "tab1", type = "pills",
            tabPanel("PK Model"),
            tabPanel("Shiny app")
          ),
          tabPanel(
            "PK Model",
            conditionalPanel(
              condition = "input.tab1=='PK Model'",
              h2("Objective"),
              h4("To visualize and explore the Bedaquiline and M2 Pharmacokinetic Model developed by Svensson et al. by modifying the parameter values in the dose regimen."),
              br(),
              h2("Structural Model"),
              imageOutput("img1"),
              h4("Schematic Illustration of Structural Model"),
              br(),
              h2("Summary of Model Characteristics"),
              p("The data used to develop model were obtained from two phase IIb studies called C208 and C209 (TMC207-C208, ClinicalTrials.gov number NCT00449644 and TMC207-C209, ClinicalTrials.gov number NCT00910871) shared to the PreDiCT-TB consortium"),
              p("It is a Nonlinear mixed-effects models consisting of structural components describing the typical PK characteristics and stochastic components describing the random variability in the population were utilized."),
              p("The typical PK characteristics was modelled from Data from 335 patients (23 from study C208 stage 1, 79 from study C208 stage 2, and 233 from study C209)  with MDR-TB receiving 24 weeks of bedaquiline on top of a longer individualized background regimen"),
              p("Bedaquiiline  absorption was described with two transit compartments and Estimated parameters were the mean absorption time (MAT; i.e., typical time to 90% complete absorption) and the fraction of MAT, which consisted of delay in the transit compartments or first-order absorption, respectively"),
              p("Bedaquiline and M2 disposition were well described by three and one-compartment models, respectively. bedaquiline and one and two-compartment models for M2. The typical values for bioavailability (F) and the fraction of bedaquiline metabolized to M2 (fm) were fixed to one, hence, the disposition parameters estimated were relative to F for bedaquiline and relative to F times fm for M2"),
              p("Semiphysiological models were developed to characterize the changes in weight and albumin over time. Weight and albumin were correlated, typically increasing after the start of treatment, and significantly affected bedaquiline and M2 plasma disposition"),
              p("Age and race were significant covariates, whereas concomitant human immunodeficiency virus (HIV) infection, sex, or having extensively drug-resistant TB was not"),
              p("Increasing age was found to decrease CL of bedaquiline and M2 linearly with 0.9% (95% CI 5 0.8–1.0%) per year and patients of black race were estimated to have 84% (95% CI 5 75–94%) higher typical CL of bedaquiline and M2 compared to non-black subjects. This was the first population model simultaneously characterizing bedaquiline and M2 PKs in its intended use population."),
              p("The developed model has been used for efficacy and safety exposure-response analyses in different studies"),
              br(),
              h3("Reference"),
              p("E. M. Svensson, A. G. Dosne, and M. O. Karlsson, ‘Population Pharmacokinetics of Bedaquiline and Metabolite M2 in Patients with Drug-Resistant Tuberculosis: The Effect of Time-Varying Weight and Albumin’, CPT Pharmacometrics Syst Pharmacol, vol. 5, no. 12, pp. 682–691, Dec. 2016, doi: 10.1002/PSP4.12147."),
              br()
            )
          ),
          tabPanel(
            "Shiny app",
            conditionalPanel(
              condition = "input.tab1=='Shiny app'",
              h2("How to use shiny app"),
              h4("1. Select the navigation bar"),
              h5("*Select Single Regimen navigation bar for one dose regimen"),
              h5("*Select Multiple Regimen navigation bar for three dose regimen"),
              br(),
              h4("2. Select the Dosing Details tab"),
              h4("Define the dosing details in the tab input"),
              br(),
              h4("Add a loading dose if desired"),
              h4("Define the loading dose details in the tab input"),
              p("*Loading dose"),
              p("*Loading dose duration (days or weeks)"),
              p("*Loading dose interval"),
              p("*Loading dose frequency (once daily, twice daily, three times weekly, once weekly"),
              br(),
              h4("Define the Maintenance dose details in the tab input"),
              p("*Maintenance dose"),
              p("*Maintenance dose duration (days or weeks)"),
              p("*Maintenance dose interval"),
              p("*Maintenance dose frequency"),
              br(),
              h4("3.Select the Model Covariates tab"),
              h4("Define the model covariates values in the tab input"),
              p("*Age"),
              p("*Race Effect"),
              p("*Baseline Body Weight (kg)"),
              p("*Baseline Albumin Concentration (g/dL)"),
              br(),
              h4("4. Select the Drug-drug Interaction tab"),
              h4("Choose a Drug-drug Interaction effect"),
              p("*None"),
              p("*Efavirenz"),
              p("*Nevirapine"),
              p("*Lopinavir/r"),
              p("*Rifampicin"),
              p("*Rifapentine"),
              br(),
              h4("5. Select the Simulation Setting tab"),
              h4("Define the simulation output"),
              p("*Number of simulated individuals"),
              p("*Simulation time"),
              p("*Interindiviual variability"),
              br(),
              h4("Choose plot output from radiobuttons"),
              p("*Full Concentration Curve"),
              p("*Trough Concentration Curve"),
              p("*Mean Daily Concentration Curve"),
              p("*Mean Weekly Concentration Curve"),
              p("*Body Weight & Albumin Concentration"),
              br(),
              h4("**if you select the multiple regimen navigation bar, you will perform step 1 to 5 for three dosing regimen**")
            )
          )
        ),
        # br()))),#end of second conditional panel
        
        tabPanel(
          "Single Regimen",
          tabsetPanel(
            id = "tab2", type = "pills",
            tabPanel("Dosing Details"),
            tabPanel("Model Covariates"),
            tabPanel("Drug-drug Interaction"),
            tabPanel("Simulation Settings")
          ),
          fluidRow(
            column(
              width = 3,
              br(),
              conditionalPanel(
                condition = "input.tab2=='Simulation Settings'",
                numericInput("nsim", label = "Number of simulated individuals", value = 1, min = 1, max = 10000),
                numericInput("simtime", label = "Simulation Time", value = 24, min = 1, max = 24000),
                selectInput("sunit", label = "Simulation Time-Unit", c("week" = "2", "day" = "1")),
                selectInput("IIV", label = "Interindiviual variability", c("OFF", "ON"), selected = "0FF"),
                hr(),
                radioButtons("ipred",
                             label = "Type of plot output",
                             c(
                               "Full concentration" = "1",
                               "Trough concentration" = "2",
                               "Daily average concentration" = "3",
                               "Weekly average concentration" = "4",
                               "Body weight & albumin concentration" = "5",
                               "QTc interval" = "6", 
                               "TTP over weeks" = "7"
                             )
                ),
                br(),
                em("*Protein binding for BDQ and M2 is >99.9%"),
                br(),
                em("**BDQ concetration is linked to drug efficacy"),
                br(),
                em("**M2 concentration is linked to drug safety"),
                br()
              ), # end of first conditional panel
              conditionalPanel(
                condition = "input.tab2=='Dosing Details'",
                checkboxInput("load_dose", "Add loading dose", value = TRUE),
                conditionalPanel(
                  condition = "input.load_dose == true",
                  numericInput("ldose", label = "Loading dose of BDQ (mg)", value = 400, min = 100, max = 20000),
                  numericInput("ldur", label = "Loading dose duration", value = 2, min = 1, max = 20000),
                  selectInput("lunit", label = "Unit", c("week" = "2", "day" = "1"), selected = "week"),
                  selectInput("lfreq",
                              label = "Loading dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly"),
                              selected = "Once daily"
                  )
                ),
                br(),
                h4("Add maintenance dose"),
                numericInput("mdose", label = "Maintenance dose of BDQ (mg)", value = 200, min = 100, max = 20000),
                numericInput("mdur", label = "Maintenance dose duration", value = 22, min = 1, max = 20000),
                selectInput("munit", label = "Unit", c("week" = "2", "day" = "1"), selected = "week"),
                selectInput("mfreq",
                            label = "Maintenance dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly"),
                            selected = "Three times weekly"
                ),
                br(),
                p("**Approved dosing regimen**"),
                p("- 400 mg daily x 2 weeks then", br(), "- 200 mg three times weekly x 22 weeks"),
                br(),
                p("**Proposed dosing regimen**"),
                p("- 200 mg twice daily x 8 weeks then", br(), "- 100 mg daily x 16 weeks"),
                br(),
                p("**Simulate any combination of loading and maintenance dose of interest**"),
                br()
              ), # end of second conditional panel
              conditionalPanel(
                condition = "input.tab2=='Model Covariates'",
                numericInput("AGE", label = "Age (years)", value = 32, min = 15, max = 100),
                selectInput("SEX", label = "Gender", c("Male", "Female")),
                selectInput("RACE", label = "Race effect", c("Non-Black", "Black")),
                numericInput("WT", label = "Baseline body weight (kg)", value = 56.6, min = 30, max = 150),
                numericInput("ALB", label = "Baseline albumin concentration (g/dL)", value = 3.65, min = 1, max = 30),
                numericInput("CACOR", label = "Baseline corrected calcium level (IU/L)", value = 2.44, min = 0.1, max = 20, step = 0.1),
                numericInput("K", label = "Baseline potassium level (IU/L)", value = 4.2, min = 0.1, max = 20, step = 0.1),
                selectInput("XDR", label = "Drug resistance", c("MDR-TB", "(pre)-XDR-TB")),
                numericInput("MTTP2", label = "Baseline time-to-positivity in MGIT culture (hours)", value = 163, min = 1, max = 1008),
                em("*Age, race, aLbumin concentration (g/dL) and body weight (kg) are significant covariates in the PK model"),
                br(),
                em("*Age, sex, race, corrected calcium Level (IU/L) and potassium Level (IU/L) are significant covariates in the PK-safety model"),
                br(), 
                em("*Drug resistance profile and baseline time-to-positivity in MGIT culture are significant covariates in the PK-efficacy model"),
                br()
              ), # end of third conditional panel
              conditionalPanel(
                condition = "input.tab2=='Drug-drug Interaction'",
                h4("Choose a Drug Interaction Effect"),
                radioButtons(
                  "IE", "Drug-drug Interaction",
                  c(
                    "None" = "NON",
                    "Efavirenz" = "EFZ",
                    "Nevirapine" = "NVP",
                    "Lopinavir/r" = "LPV",
                    "Rifampicin " = "RIF",
                    "Rifapentine" = "RPT",
                    "Clofazimine" = "CFZ",
                    "Moxifloxacin" = "MFX"
                  )
                )
              )
            ),
            column(
              width = 7,
              hr(),
              h2("Graphical Illustration"),
              hr(),
              plotOutput("plot", height = "500px")
            )
          )
        ), # end of third column, first fluidRow
        
        ## compare regimen ####
        tabPanel(
          "Compare Regimen",
          tabsetPanel(
            id = "tab3", type = "pills",
            tabPanel("Dosing Details"),
            tabPanel("Model Covariates"),
            tabPanel("Drug-drug Interaction"),
            tabPanel("Simulation Settings")
          ),
          fluidRow(
            column(
              width = 4,
              br(),
              conditionalPanel(
                condition = "input.tab3=='Dosing Details'",
                h4("DOSE REGIMEN 1"),
                checkboxInput("load_dose1", "Add loading dose"),
                conditionalPanel(
                  condition = "input.load_dose1 == true",
                  numericInput("ldose1", label = "Loading dose of BDQ (mg)", value = 400, min = 100, max = 20000),
                  numericInput("ldur1", label = "Loading dose duration", value = 2, min = 1, max = 20000),
                  selectInput("lunit1", label = "Unit", c("week" = "2", "day" = "1")),
                  selectInput("linter1", label = "Loading dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly"))
                ),
                h4("Add Maintenance dose"),
                numericInput("mdose1", label = "Maintenance dose of BDQ (mg)", value = 200, min = 100, max = 20000),
                numericInput("mdur1", label = "Maintenance dose duration", value = 22, min = 1, max = 20000),
                selectInput("munit1", label = "Unit", c("week" = "2", "day" = "1")),
                selectInput("minter1", label = "Maintenance dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly")),
                br(),
                h4("DOSE REGIMEN 2"),
                checkboxInput("load_dose2", "Add loading dose"),
                conditionalPanel(
                  condition = "input.load_dose2 == true",
                  numericInput("ldose2", label = "Loading dose of BDQ (mg)", value = 400, min = 100, max = 20000),
                  numericInput("ldur2", label = "Loading dose duration", value = 2, min = 1, max = 20000),
                  selectInput("lunit2", label = "Unit", c("week" = "2", "day" = "1")),
                  selectInput("linter2", label = "Loading dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly"))
                ),
                h4("Add Maintenance dose"),
                numericInput("mdose2", label = "Maintenance dose of BDQ (mg)", value = 200, min = 100, max = 20000),
                numericInput("mdur2", label = "Maintenance dose duration", value = 22, min = 1, max = 20000),
                selectInput("munit2", label = "Unit", c("week" = "2", "day" = "1")),
                selectInput("minter2", label = "Maintenance dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly")),
                br(),
                h4("DOSE REGIMEN 3"),
                checkboxInput("load_dose3", "Add loading dose"),
                conditionalPanel(
                  condition = "input.load_dose3 == true",
                  numericInput("ldose3", label = "Loading dose of BDQ (mg)", value = 400, min = 100, max = 20000),
                  numericInput("ldur3", label = "Loading dose duration", value = 2, min = 1, max = 20000),
                  selectInput("lunit3", label = "Unit", c("week" = "2", "day" = "1")),
                  selectInput("linter3", label = "Loading dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly"))
                ),
                h4("Add Maintenance dose"),
                numericInput("mdose3", label = "Maintenance dose of BDQ (mg)", value = 200, min = 100, max = 20000),
                numericInput("mdur3", label = "Maintenance dose duration", value = 22, min = 1, max = 20000),
                selectInput("munit3", label = "Unit", c("week" = "2", "day" = "1")),
                selectInput("minter3", label = "Maintenance dose frequency", c("Twice daily", "Once daily", "Three times weekly", "Once weekly")),
                br(),
                p("**Approved Dosage Regimen**"),
                p("- 400mg q24h x2WKS - 3x200mg q168h x22WKS"),
                br(),
                p("**Proposed Dose regimen**"),
                p("- 400mg q12h x8WKS - 100mg q24h x16WKS"),
                br(),
                p("**Simulate any Combination of Loading and Maintenance Dose of interest**"),
                br()
              ), # end of second conditional panel
              conditionalPanel(
                condition = "input.tab3=='Model Covariates'",
                h4("Choose Model Covariate"),
                numericInput("AGE3", label = "Age (years)", value = 47, min = 15, max = 100),
                selectInput("RACE3", label = "Race effect", c("Non-Black", "Black")),
                numericInput("WT3", label = "Baseline Body Weight (kg)", value = 55, min = 30, max = 1400),
                numericInput("ALB3", label = "Baseline Albumin Concentration (g/dL)", value = 3.5, min = 1, max = 300),
                em("*Race, ALbumin Concentration (g/dL) and Body weight (kg) are significant covariates in the model"),
                br()
              ), # end of third conditional panREGIMel
              
              
              
              conditionalPanel(
                condition = "input.tab3=='Drug-drug Interaction'",
                h3("Choose a Drug Interaction Effect"),
                h4("Dose Regimen 1"),
                radioButtons(
                  "IE1", "Drug-drug Interaction",
                  c(
                    "None" = "NON1",
                    "Efavirenz" = "EFZ1",
                    "Nevirapine" = "NVP1",
                    "Lopinavir/r" = "LPV1",
                    "Rifampicin " = "RIF1",
                    "Rifapentine" = "RPT1"
                  )
                ),
                br(),
                h4("Dose Regimen 2 "),
                radioButtons(
                  "IE2", "Drug-drug Interaction",
                  c(
                    "None" = "NON2",
                    "Efavirenz" = "EFZ2",
                    "Nevirapine" = "NVP2",
                    "Lopinavir/r" = "LPV2",
                    "Rifampicin " = "RIF2",
                    "Rifapentine" = "RPT2"
                  )
                ),
                br(),
                h4("Dose Regimen 3"),
                radioButtons(
                  "IE3", "Drug-drug Interaction",
                  c(
                    "None" = "NON3",
                    "Efavirenz" = "EFZ3",
                    "Nevirapine" = "NVP3",
                    "Lopinavir/r" = "LPV3",
                    "Rifampicin " = "RIF3",
                    "Rifapentine" = "RPT3"
                  )
                )
              ), # end of third conditional panel
              br(),
              conditionalPanel(
                condition = "input.tab3=='Simulation Settings'",
                numericInput("nsim3", label = "Number of simulated individuals", value = 1, min = 1, max = 10000),
                numericInput("simtime3", label = "Simulation Time", value = 24, min = 1, max = 24000),
                selectInput("sunit3", label = "Simulation Time-Unit", c("week" = "2", "day" = "1")),
                selectInput("variab3", label = "Interindiviual variability", c("OFF", "ON"), selected = "0FF"),
                hr(),
                radioButtons("ipred3",
                             label = "Type of plot output",
                             c(
                               "Full Concentration Curve" = "1",
                               "Trough Concentration Curve" = "2",
                               "Mean Daily Concentration Curve" = "3",
                               "Mean Weekly Concentration Curve" = "4",
                               "Body Weight & Albumin Concentration" = "5"
                             )
                ),
                br(),
                em("*Protein binding for BDQ and M2 is >99.9%"),
                br(),
                em("**BDQ concetration is linked drug efficacy"),
                br(),
                em("***M2 concentrationbis linked drug safety"),
                br()
              )
            ), # end of first conditional panel
            column(
              width = 8,
              hr(),
              h2("Graphical Illustration"),
              hr(),
              plotOutput("plot3", height = "700px")
            )
          )
        ), # end of third column, first fluidRow
        
        ## Data Output ####
        tabPanel(
          "Data Output",
          tabsetPanel(
            id = "tab4", type = "pills",
            tabPanel("Simulation QT Output"),
            tabPanel("Simulation Output"),
            tabPanel("Trough Simulation Output"),
            tabPanel("Cavg-daily Output"),
            tabPanel("Cavg-weekly Output")
          ),
          br(),
          conditionalPanel(
            condition = "input.tab4=='Simulation QT Output'",
            h4("Simulated QT Dataframe"),
            fluidRow(
              column(
                offset = 2,
                width = 6,
                DT::dataTableOutput("sim_dataQTDT"),
                # tableOutput("sim_dataQT")
              ), # end of first column
              column(
                width = 1,
                br(),
                downloadButton("download_simdataQT", "Download Simulated QT Dataframe")
              )
            )
          ),
          conditionalPanel(
            condition = "input.tab4=='Simulation Output'",
            h4("Simulated Full Profile Dataframe"),
            fluidRow(
              column(
                offset = 2,
                width = 6,
                DT::dataTableOutput("sim_dataDT"),
                # tableOutput("sim_data")
              ), # end of first column
              column(
                width = 1,
                br(),
                downloadButton("download_simdata", "Download Simulated Dataframe")
              )
            )
          ), # end of second column, first fluidRow and tabPanel 2 "Summary statistics"
          conditionalPanel(
            condition = "input.tab4=='Trough Simulation Output'",
            h4("Simultated Trough Dataframe"),
            fluidRow(
              column(
                offset = 2,
                width = 8,
                DT::dataTableOutput("trough_dataDT"),
                # tableOutput("trough_data")
              ), # end of first column
              column(
                width = 1,
                br(),
                downloadButton("download_troughdata", "Download Trough Dataframe")
              )
            )
          ), # end of second column, first fluidRow and tabPanel 2 "Summary statistics"
          conditionalPanel(
            condition = "input.tab4=='Cavg-daily Output'",
            h4("Simultated Cavg-daily Dataframe"),
            fluidRow(
              column(
                offset = 2,
                width = 8,
                DT::dataTableOutput("cavgdaily_dataDT"),
                # tableOutput("cavgdaily_data")
              ), # end of first column
              column(
                width = 1,
                br(),
                downloadButton("download_cavgdaily", "Download Cavg-daily Dataframe")
              )
            )
          ), # end of second column, first fluidRow and tabPanel 2 "Summary statistics"
          conditionalPanel(
            condition = "input.tab4=='Cavg-weekly Output'",
            h4("Simultated Cavg-weekly Dataframe"),
            fluidRow(
              column(
                offset = 2,
                width = 8,
                DT::dataTableOutput("cavgweekly_dataDT"),
                # tableOutput("cavgweekly_data")
              ), # end of first column
              column(
                width = 1,
                br(),
                downloadButton("download_cavgweekly", "Download Cavg-weekly Dataframe")
              )
            )
          )
        ), # end of second column, first fluidRow and tabPanel 2 "Summary statistics"
        
        
        
        tabPanel(
          "Code",
          tabsetPanel(
            id = "tab5", type = "pills",
            tabPanel("Model Code"),
            tabPanel("ui.R"),
            tabPanel("server.R")
          ),
          conditionalPanel(
            condition = "input.tab5=='ui.R'",
            h4("code "),
            fluidRow(
              column( # offset=2,
                width = 6,
                # DT::dataTableOutput("sim_dataDT"),
                verbatimTextOutput("ui_script")
              ), # end of first column
              column(
                width = 1,
                br(),
                downloadButton(
                  "download_ui",
                  "Download ui.R"
                )
              )
            )
          ) # end of second column, first fluidRow and tabPanel 2 "Summary statistics"
        )
      )
    ) # end of navbarPage
  ) # end of fluidPage

#### Define function used in server ####
convertTimeUnit <- function(val) {
  if (val == "1") {                          # if input unit is "day"
    return(24)
  } else {                                   # else: "week"
    return(168)
  }
}

defineEventVariable <- function(dur, unit, val) {
  if (val == "Twice daily") {
    ii <<- 12
    dosingtime <<- 0
    addl <<- (dur * unit / 24) * 2 - 1
  } else if (val == "Once daily") {
    ii <<- 24
    dosingtime <<- 0
    addl <<- (dur * unit / 24) * 1 - 1
  } else if (val == "Three times weekly") {
    ii <<- 168
    dosingtime <<- c(0, 48, 96)
    addl <<- dur * unit / 168 - 1
  } else {
    ii <<- 168
    dosingtime <<- 0
    addl <<- dur * unit / 168 - 1
  }
}

createEventDataset <- function(nsamples, dose, timeModifier) {
  return(as.data.frame(ev(
    ID = 1:nsamples,
    ii = ii,
    cmt = 1,
    amt = dose,
    addl = addl,
    time = dosingtime + timeModifier)))
}

PKSimulation <- function(IIVval, IEval, mod, df, sim_time, sunit) {
  
  ## IIV "ON"/"OFF"
  if(IIVval == "OFF") {
    mod <- zero_re(mod)
  }
  else {
    mod <- zero_re(mod, sigma)
  }
  
  ## DDI "EFZ", "NVP", "RIF", "LPV", "RPT", "NON"
  if(IEval == "EFZ") {
    mod <- update(mod, param = list(THETA25 = 2.1, THETA26 = 2.1))
  }
  else if(IEval == "NVP") {
    mod <- update(mod, param = list(THETA25 = 0.95, THETA26 = 1.58))
  }
  else if(IEval == "RIF") {
    mod <- update(mod, param = list(THETA25 = 4.8, THETA26 = 4.8))
  }
  else if(IEval %in% c("LPV", "RPT")) {
    mod <- update(mod, param = list(THETA25 = 4.0, THETA26 = 4.0))
  }
  
  ## Return simulated PK dataset
  return(
    mod %>%
      data_set(df) %>%
      mrgsim(end = sim_time * sunit, delta = 1) %>%
      as.data.frame()
  )
}


#### Define server logic ####
server <- function(input, output, session) {
  
  ## define sim_dataframePK() ####
  sim_dataframePK <- reactive({
    ## Simulation settings
    # 1. "nsim"
    nsamples <- as.numeric(input$nsim)      # Number of simulated individuals
    
    # 2. "simtime" and "simunit"
    sim_time <- as.numeric(input$simtime)   # Time of simulation imputed (transformed in hours during simulation)
    sunit <- convertTimeUnit(input$sunit)   # Simulation unit: "1" day, "2" week
    
    ## Dosing details
    # 1. "loading dose"
    if (input$load_dose == TRUE) {
      ldose <- as.numeric(input$ldose)      # Loading dose amount (mg)
      ldur <- as.numeric(input$ldur)        # Loading dose duration (transformed in hours during simulation)
      lunit <- convertTimeUnit(input$lunit) # Loading dose unit: "1" day, "2" week
      
      # Interval intake for loading dose
      defineEventVariable(ldur, lunit, input$lfreq)
      
      # Event dataset for loading dose
      dfLoad <- createEventDataset(nsamples, ldose, 0)
    }
    
    # 2. Maintenance Dose
    mdose <- as.numeric(input$mdose)      # Maintenance dose amount (mg)
    mdur <- as.numeric(input$mdur)        # Maintenance dose duration(transformed in hours during simulation)
    munit <- convertTimeUnit(input$munit) # Maintenance dose unit: "1" day, "2" week
    
    # Interval intake for maintenance dose
    defineEventVariable(mdur, munit, input$mfreq)
    
    ## Event dataset for (loading dose +) maintenance dose
    if (input$load_dose == TRUE) {
      dfMaintenance <- createEventDataset(nsamples, mdose, ldur * lunit)
      dfPK <- rbind(dfLoad, dfMaintenance)
      dfPK <- dfPK[order(dfPK$ID, dfPK$time), ]
    } else {
      dfMaintenance <- createEventDataset(nsamples, mdose, 0)
      dfPK <- dfMaintenance
    }
    
    ####################
    ## Common model covariates
    # 1 "RACE"
    if (input$RACE == "Non-Black") {
      RACE <- 1
    } else {
      RACE <- 2
    }
    
    dfPK$RACE <- RACE
    
    # 2"WT"
    WT <- input$WT
    dfPK$THETA6 <- WT
    
    # 3 "ALB"
    ALB <- input$ALB
    dfPK$THETA1 <- ALB
    
    # 4 AGE
    AGE <- input$AGE
    dfPK$AGE <- AGE
    
    # # Generate dataset with covariates
    # ###### Sampling of RACE
    # unique.dfPK <- unique(dfPK$ID)
    # sample.dfPK <- sample(unique.dfPK, 0.34 * length(unique.dfPK))
    # dfPK <- dfPK %>%
    #   dplyr::mutate(RACE = ifelse(ID %in% sample.dfPK, 2, 1))
    # 
    # # AGE SAMPLING
    # set.seed(100)
    # dfPK <- dfPK %>%
    #   group_by(ID) %>%
    #   dplyr::mutate(AGE = round(runif(1, 18, 68)))
    
    ## Set parameters in dataset
    dfPK$AGE <- AGE
    dfPK$RACE <- RACE
    dfPK$THETA6 <- WT
    dfPK$THETA1 <- ALB
    
    ###############################################
    ### PK simulation
    # Load mrgsolve model
    mod <- mcode("BDQOMAT", code)
    
    # Run simulation
    set.seed(3468)
    out <- PKSimulation(input$IIV, input$IE, mod, dfPK, sim_time, sunit)
    out$REGIMEN <- "1"
    
    return(out)
  })
  
  dfReadyForQT <- reactive({
    dfQT <- sim_dataframePK() %>% filter(AMT == 0)
    dfQT <- subset(dfQT, select = c(ID, REGIMEN, time, IPREDM2, RACE, AGE))
    # dfQT <- dfQT %>% mutate(CONCM2_weekly = dd2$weekly_M2/168*1000)
    dfQT <- dfQT %>% mutate(CONCM2 = exp(IPREDM2) * 1000) ## mg/L to ng/mL (concentration unit used in QT model)
    dfQT$TIMW <- 0#dfQT$time / 24 / 7 ## TIMW = TAST/24/7 for time effect in QT model
    ## Covariates
    # 1. "SEX"
    if (input$SEX == "Male") {
      dfQT$SEX <- 0
    } else {
      dfQT$SEX <- 1
    }
    
    # 2. Electrolytes level (Corrected Ca and Potassium level)
    dfQT$CACOR <- input$CACOR # 2.440
    dfQT$K <- input$K # 4.200
    
    # 3. DDI (Clofazimine or Moxifloxacin)
    if (input$IE == "CFZ") {
      dfQT$CLOFA <- 1
    }
    if (input$IE == "MFX") {
      dfQT$MOXI <- 1
    }
    
    return(dfQT)
  })
  
  #### Simulation: QT
  sim_dataframeQT <- reactive({
    ## Simulation settings
    # 1. "nsim"
    nsamples <- as.numeric(input$nsim)      # Number of simulated individuals
    
    # 2. "simtime" and "simunit"
    sim_time <- as.numeric(input$simtime)   # Time of simulation imputed (transformed in hours during simulation)
    sunit <- convertTimeUnit(input$sunit)   # Simulation unit: "1" day, "2" week
    
    ###############################################
    ### PK simulation
    modQT <- mcode("BDQQT", codeQT)
    
    ## Interindividual variability ON/OFF
    if (input$IIV == "OFF") {
      set.seed(3468)
      outQT <- modQT %>%
        zero_re() %>%
        data_set(dfReadyForQT()) %>%
        mrgsim(end = sim_time * sunit, delta = 1) %>%
        as.data.frame()
      outQT$REGIMEN <- "1"
      return(outQT)
    } else {
      set.seed(3468)
      outQT <- modQT %>%
        zero_re(sigma) %>%
        data_set(dfReadyForQT()) %>%
        mrgsim(end = sim_time * sunit, delta = 1) %>%
        as.data.frame()
      outQT$REGIMEN <- "1"
      return(outQT)
    }
  })
  
  
  #### DATAFRAME FOR TROUGH CONC AT 24 HRS
  sim_dataframe24 <- reactive({
    df <- sim_dataframePK() %>% filter(AMT == 0)
    df <- df %>% filter(time %% 24 == 0)
    df$DAY <- df$time / 24
    return(df)
  })
  
  ####### Reactive MEAN DAILY CONCENTRATION
  Cavg_daily <- reactive({
    d1 <- sim_dataframePK() %>% filter(AMT == 0)
    
    d1 <- d1 %>% filter(time %% 24 == 0)
    
    d1$DAY <- d1$time / 24
    
    d1 <- subset(d1, select = c(ID, REGIMEN, time, DAY, RACE, AUCBDQ, AUCM2))
    
    ###### BDQ
    d1$AUCWBDQ <- "0"
    i <- 2
    while (i <= length(d1$ID)) {
      d1$AUCWBDQ[i] <- d1$AUCBDQ[i] - d1$AUCBDQ[i - 1]
      
      i <- i + 1
    }
    
    ########## M2
    d1$AUCWM2 <- "0"
    
    i <- 2
    while (i <= length(d1$ID)) {
      d1$AUCWM2[i] <- d1$AUCM2[i] - d1$AUCM2[i - 1]
      
      i <- i + 1
    }
    
    d1 <- d1 %>% mutate(
      AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ),
      AUCWM2 = ifelse(time == 0, 0, AUCWM2)
    )
    
    ####### as numeric
    d1$AUCWBDQ <- as.numeric(as.character(d1$AUCWBDQ))
    d1$AUCWM2 <- as.numeric(as.character(d1$AUCWM2))
    
    ###### summarise by
    dd1 <- d1 %>%
      group_by(ID, time, REGIMEN, DAY) %>%
      dplyr::summarise(
        daily_BDQ = mean(AUCWBDQ, na.rm = T),
        daily_M2 = mean(AUCWM2, na.rm = T)
      )
    
    return(dd1)
  })
  
  
  #### WEEKLY Cavg
  Cavg_weekly <- reactive({
    d2 <- sim_dataframePK() %>% filter(AMT == 0)
    
    ####### Use the substituted new data frame d1
    d2 <- d2 %>% filter(time %% 168 == 0)
    
    d2$WEEK <- d2$time / 168
    
    d2 <- subset(d2, select = c(ID, time, REGIMEN, WEEK, AUCBDQ, AUCM2))
    
    ###### BDQ
    d2$AUCWBDQ <- "0"
    
    i <- 2
    while (i <= length(d2$ID)) {
      d2$AUCWBDQ[i] <- d2$AUCBDQ[i] - d2$AUCBDQ[i - 1]
      
      i <- i + 1
    }
    
    ########## M2
    d2$AUCWM2 <- "0"
    
    i <- 2
    while (i <= length(d2$ID)) {
      d2$AUCWM2[i] <- d2$AUCM2[i] - d2$AUCM2[i - 1]
      
      i <- i + 1
    }
    
    
    d2 <- d2 %>% mutate(
      AUCWBDQ = ifelse(time == 0, 0, AUCWBDQ),
      AUCWM2 = ifelse(time == 0, 0, AUCWM2)
    )
    
    ####### as numeric
    d2$AUCWBDQ <- as.numeric(as.character(d2$AUCWBDQ))
    d2$AUCWM2 <- as.numeric(as.character(d2$AUCWM2))
    
    ###### summarise by
    dd2 <- d2 %>%
      group_by(ID, time, REGIMEN, WEEK) %>%
      dplyr::summarise(
        weekly_BDQ = mean(AUCWBDQ, na.rm = T),
        weekly_M2 = mean(AUCWM2, na.rm = T)
      )
    
    return(dd2)
  })
  
  dfReadyForTTP <- reactive({
    TTPdf   <- tidyr::crossing(
      ID    = c(1:input$nsim), 
      WEEKP = c(1:24), 
      REP   = c(1:3), 
      EVID  = 0, 
      AMT   = 0,
      FLAG  = 1, 
      TTPD  = c(0, 1:42), 
      LASTR = 0) %>%
      mutate(TIME = seq_along(TTPD) - 1) # dummy time column
    
    TTPdf2 <- TTPdf %>% filter(TTPD == 0) %>% mutate(FLAG = 2, TIME = NA)
    
    TTPdf_fin <- rbind(TTPdf, TTPdf2) %>%
      mutate(EVID  = ifelse(TTPD == 0 & FLAG == 1,  4, EVID), 
             AMT   = ifelse(TTPD == 0 & FLAG == 1,  1, AMT), 
             LASTR = ifelse(TTPD == 42, 1, LASTR), 
             TASTW = WEEKP) %>%
      mutate(CMT = ifelse(AMT == 1, 1, NA)) %>%
      arrange(ID, WEEKP, REP, TTPD) %>%
      zoo::na.locf()
    
    # covariate distribution sampling
    ###### Sampling of pre-XDR+XDR
    unique.TTPdf_fin <- unique(TTPdf_fin$ID)
    sample.TTPdf_fin <- sample(unique.TTPdf_fin, 0.3 * length(unique.TTPdf_fin))
    TTPdf_fin <- TTPdf_fin %>%
      dplyr::mutate(XDR = ifelse(ID %in% sample.TTPdf_fin, 1, 0))
    
    # MTTP2 SAMPLING
    set.seed(100)
    TTPdf_fin <- TTPdf_fin %>%
      group_by(ID) %>%
      dplyr::mutate(MTTP2 = runif(1, 55.2, 1008))
    
    dfCAVG <- Cavg_weekly() %>% rename("WEEKP" ="WEEK") %>% 
      filter(WEEKP != 0) %>% 
      mutate(CAVG  =  weekly_BDQ/168) %>% # unit µg/mL
      ungroup() %>% select(ID, WEEKP, CAVG)    
    
    dfTTP <- TTPdf_fin %>% full_join(dfCAVG)
    
    return(dfTTP)
  })
  
  #### Simulation: TTP
  sim_dataframeTTP <- reactive({
    ## Simulation settings
    # 1. "nsim"
    nsamples <- as.numeric(input$nsim)      # Number of simulated individuals
    
    # 2. "simtime" and "simunit"
    sim_time <- as.numeric(input$simtime)   # Time of simulation imputed (transformed in hours during simulation)
    sunit <- convertTimeUnit(input$sunit)   # Simulation unit: "1" day, "2" week
    
    
    ###############################################
    ### PK simulation
    modTTP <- mcode("BDQTTP", codeTTP)
    
    ## Interindividual variability ON/OFF
    if (input$IIV == "OFF") {
      set.seed(3468)
      outTTP <- modTTP %>%
        zero_re() %>%
        data_set(dfReadyForTTP()) %>%
        mrgsim(end = sim_time * sunit, delta = 1) %>%
        as.data.frame()
      outTTP$REGIMEN <- "1"
      return(outTTP)
    } else { #### no sigma in TTP
      set.seed(3468)
      outTTP <- modTTP %>%
        data_set(dfReadyForTTP()) %>%
        mrgsim(end = sim_time * sunit, delta = 1) %>%
        as.data.frame()
      outTTP$REGIMEN <- "1"
      return(outTTP)
    }
  })
  
  ########################################################
  ##### RENDER IMAGE ####
  #### render image
  output$img1 <- renderImage({
    list(
      src = "www/stmod.png",
      width = "100%",
      height = 330
    )
  })
  
  ######## OUTPUTS reactive plot
  ipred <- reactive({
    input$ipred
  })
  plot <- reactive({
    input$plot
  })
  sim_time <- reactive({
    input$sim_time
  })
  
  output$plot <- renderPlot({
    RACE <- input$RACE
    IE <- input$IE
    
    if (input$ipred == 1) {
      # if(input$IIV == "ON") {
      dfForPlotBDQ <- sim_dataframePK() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(exp(IPRED), probs = 0.05),
          median = quantile(exp(IPRED), probs = 0.5),
          upper = quantile(exp(IPRED), probs = 0.95)
        )
      
      a1 <- ggplot(dfForPlotBDQ, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        # scale_y_continuous(breaks = seq(0, 4, 0.5),limits = c(0,2.5)) +
        labs(x = "Time (weeks)", y = c("BDQ concentration (mg/L)")) +
        ggtitle("BDQ Concentration (mg/L) vs Time")
      
      dfForPlotM2 <- sim_dataframePK() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(exp(IPREDM2), probs = 0.05),
          median = quantile(exp(IPREDM2), probs = 0.5),
          upper = quantile(exp(IPREDM2), probs = 0.95)
        )
      
      a2 <- ggplot(dfForPlotM2, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("M2 concentration (mg/L)")) +
        ggtitle("M2 Concentration (mg/L) vs Time")
      
      plot <- grid.arrange(a1, a2, nrow = 2)
    } else if (input$ipred == 2) {
      dfForPlotCtBDQ <- sim_dataframe24() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(exp(IPRED), probs = 0.05),
          median = quantile(exp(IPRED), probs = 0.5),
          upper = quantile(exp(IPRED), probs = 0.95)
        )
      
      b1 <- ggplot(dfForPlotCtBDQ, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("BDQ concentration (mg/L)")) +
        ggtitle("BDQ Concentration (mg/L) vs Time")
      
      dfForPlotCtM2 <- sim_dataframe24() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(exp(IPREDM2), probs = 0.05),
          median = quantile(exp(IPREDM2), probs = 0.5),
          upper = quantile(exp(IPREDM2), probs = 0.95)
        )
      
      b2 <- ggplot(dfForPlotCtM2, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("M2 concentration (mg/L)")) +
        ggtitle("M2 Concentration (mg/L) vs Time")
      
      plot <- grid.arrange(b1, b2, nrow = 2)
    } else if (input$ipred == 3) {
      dfForPlotDBDQ <- Cavg_daily() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(daily_BDQ, probs = 0.05),
          median = quantile(daily_BDQ, probs = 0.5),
          upper = quantile(daily_BDQ, probs = 0.95)
        )
      
      c1 <- ggplot(dfForPlotDBDQ, aes(x = time / 168, y = median / 24)) +
        geom_ribbon(aes(ymin = lower / 24, ymax = upper / 24), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("BDQ concentration (mg/L)")) +
        ggtitle("Daily Average BDQ Concentration (mg/L) vs Time (weeks)")
      
      dfForPlotDM2 <- Cavg_daily() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(daily_M2, probs = 0.05),
          median = quantile(daily_M2, probs = 0.5),
          upper = quantile(daily_M2, probs = 0.95)
        )
      
      c2 <- ggplot(dfForPlotDM2, aes(x = time / 168, y = median / 24)) +
        geom_ribbon(aes(ymin = lower / 24, ymax = upper / 24), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("M2 concentration (mg/L)")) +
        ggtitle("Daily Average M2 Concentration (mg/L) vs Time (weeks)")
      
      plot <- grid.arrange(c1, c2, nrow = 2)
    } else if (input$ipred == 4) {
      dfForPlotWBDQ <- Cavg_weekly() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(weekly_BDQ, probs = 0.05),
          median = quantile(weekly_BDQ, probs = 0.5),
          upper = quantile(weekly_BDQ, probs = 0.95)
        )
      
      d1 <- ggplot(dfForPlotWBDQ, aes(x = time / 168, y = median / 168)) +
        geom_ribbon(aes(ymin = lower / 168, ymax = upper / 168), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("BDQ concentration (mg/L)")) +
        ggtitle("Weekly Average BDQ Concentration (mg/L) vs Time (weeks)")
      
      dfForPlotWM2 <- Cavg_weekly() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(weekly_M2, probs = 0.05),
          median = quantile(weekly_M2, probs = 0.5),
          upper = quantile(weekly_M2, probs = 0.95)
        )
      
      d2 <- ggplot(dfForPlotWM2, aes(x = time / 168, y = median / 168)) +
        geom_ribbon(aes(ymin = lower / 168, ymax = upper / 168), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
        labs(x = "Time (weeks)", y = c("M2 concentration (mg/L)")) +
        ggtitle("Weekly Average M2 Concentration (mg/L) vs Time (weeks)")
      
      plot <- grid.arrange(d1, d2, nrow = 2)
    } else if (input$ipred == 5) {
      dfForPlotWT <- sim_dataframePK() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(IPREDWT, probs = 0.05),
          median = quantile(IPREDWT, probs = 0.5),
          upper = quantile(IPREDWT, probs = 0.95)
        )
      
      e1 <- ggplot(dfForPlotWT, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 25)) +
        # scale_y_continuous(breaks = seq(0, 4, 0.5),limits = c(0,2.5)) +
        labs(x = "Time (weeks)", y = ("Body weight (kg)")) +
        ggtitle("Body Weight (kg) vs Time (weeks)")
      
      dfForPlotALB <- sim_dataframePK() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(IPREDALB, probs = 0.05),
          median = quantile(IPREDALB, probs = 0.5),
          upper = quantile(IPREDALB, probs = 0.95)
        )
      
      e2 <- ggplot(dfForPlotALB, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 25)) +
        labs(x = "Time (weeks)", y = ("Albumin concentration (g/dL)")) +
        ggtitle("Albumin Concentration (g/dL) vs Time(weeks)")
      
      plot <- grid.arrange(e1, e2, nrow = 2)
    } else if (input$ipred==6) {
      dfForPlotQT <- sim_dataframeQT() %>%
        ungroup() %>%
        group_by(time) %>%
        summarize(
          lower = quantile(IPRED, probs = 0.05),
          median = quantile(IPRED, probs = 0.5),
          upper = quantile(IPRED, probs = 0.95)
        )
      
      plot <- ggplot(dfForPlotQT, aes(x = time / 168, y = median)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
        geom_line(size = 1.2) +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        theme(axis.text = element_text(size = 15)) +
        scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 25)) +
        scale_y_continuous(breaks = seq(380, 450, 5)) +
        labs(x = "Time (weeks)", y = ("QTc (ms)")) +
        ggtitle("QTc (ms) vs Time (weeks)")
    } else {
      # --- Get the proportion of samples without positive signal ---
      outTTE <- sim_dataframeTTP() %>% filter(RTTE == 1) ## positive signal at specific time
      TTEcount <- outTTE %>% group_by(WEEKP) %>% count(TTPD)
      ttpd <- crossing(TTPD = c(1:42), WEEKP = c(1:24))
      TTEcount <- merge(ttpd, TTEcount, all.x = TRUE) %>% mutate(n = ifelse(is.na(n), 0, n))
      TTEcount <- TTEcount[order(TTEcount$WEEKP, TTEcount$TTPD), ]
      
      TTEcount <- TTEcount %>% group_by(WEEKP) %>% mutate(cumn = cumsum(n), proportion = NA)
      TTEcount <- TTEcount %>% group_by(WEEKP) %>% mutate(proportion = ifelse(TTPD != 42, (1 - (cumn/sum(n))), lag(proportion)))
      TTEcount <- TTEcount %>% filter(TTPD != 42)
      
      # --- plot for WEEKP specified in the article ---
      TTEcountw <- TTEcount %>% 
        filter(WEEKP %in% c(1:8, 10, 12, 14, 16, 18, 20))
      
      # Custom labeller function to change facet titles
      week_labels <- as_labeller(function(week) {
        paste("Week", week)
      })
      
      plot <- ggplot(TTEcountw, aes(TTPD, proportion*100)) +
        facet_wrap(~WEEKP, labeller = week_labels, ncol = 3, scales = "free_x") +
        geom_line(size = 1) +
        theme_bw() +
        xlab("Time in MGIT after inoculation (days)") +
        ylab("Proportion of samples without positive signal (%)") +
        ggtitle("Simulated TTP in MGIT per week after start of treatment") +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous(breaks = seq(0, 100, by = 20))
    }
    return(plot)
  })
  
  
  
  
  
  ## DOWNLOAD SIMULATED DATAFRAME ####
  # Dataset of Simulated dataframe
  output$sim_dataQTDT <- DT::renderDataTable({
    sim_dataframeQT()
  })
  
  output$sim_dataDT <- DT::renderDataTable({
    sim_dataframePK()
  })
  
  
  # Downloadable csv of Sim dataframe
  output$download_simdata <- downloadHandler(
    filename = function() {
      "simdataframe.csv"
    },
    content = function(file) {
      write.csv(sim_dataframePK(), file, row.names = FALSE)
    }
  )
  
  # Dataset of Trough dataframe
  output$trough_dataDT <- DT::renderDataTable({
    sim_dataframe24()
  })
  
  # Downloadable csv of Trough dataframe
  output$download_troughdata <- downloadHandler(
    filename = function() {
      "simdataframe24.csv"
    },
    content = function(file) {
      write.csv(sim_dataframe24(), file, row.names = FALSE)
    }
  )
  
  
  # Dataset of Cavg-daily  dataframe
  output$cavgdaily_dataDT <- DT::renderDataTable({
    Cavg_daily()
  })
  
  # Downloadable csv of Sim dataframe
  output$download_cavgdaily <- downloadHandler(
    filename = function() {
      "Cavg_daily.csv"
    },
    content = function(file) {
      write.csv(Cavg_daily(), file, row.names = FALSE)
    }
  )
  
  # Dataset of Cavg weekly dataframe
  output$cavgweekly_dataDT <- DT::renderDataTable({
    Cavg_weekly()
  })
  
  # Downloadable csv of Sim dataframe
  output$download_cavgweekly <- downloadHandler(
    filename = function() {
      "Cavgweekly.csv"
    },
    content = function(file) {
      write.csv(Cavg_weekly(), file, row.names = FALSE)
    }
  )
  
  output$ui_script <- renderText({
    text <- readLines(rstudioapi::getSourceEditorContext()$path)
    
    # split the text into a list of character vectors
    #   Each element in the list contains one line
    splitText <- paste(str = text, collapse = "\t\n")
    
    # wrap a paragraph tag around each element in the list
    # replacedText <- lapply(splitText, p)
    
    return(splitText)
  })
}

shinyApp(ui = ui, server = server)
