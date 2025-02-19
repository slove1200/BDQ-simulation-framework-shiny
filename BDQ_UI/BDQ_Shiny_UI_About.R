mainTabAbout <- tabPanel(
    "About",
    tabsetPanel(
        id = "aboutTab", type = "pills",
        tabPanel("User Manual", value = "User Manual"),
        tabPanel("Source Code", value = "Source Code")
    ),
    br(),
    conditionalPanel(
        condition = "input.aboutTab == 'User Manual'",
        page_fillable(
            layout_columns(
                # Overview Card
                card(
                    card_header("Overview", style = "font-size: 20px; background-color: #CDD8DA;"),
                    card_body(
                        tags$span("This interactive application is aimed for interested users to visualize and explore 
                                   different dosing strategies of bedaquiline in an integrated pharmacokinetic, pharmacodynamic and 
                                   long-term outcome modelling framework using patient characteristics, concomitant medications in flexible simulation settings.",
                                   style = "font-size: 14px; padding-left: 5px; line-height: 1.9;"),
                        layout_columns(
                          # Model Description
                          card(
                            card_header("Models Included in the Application", style = "font-size: 16px; background-color: #E8ECEE;"),
                            card_body(
                                tags$span(tags$strong("This application incorporates:", style = "font-size: 14px;")),
                                tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                                    tags$li("Pharmacokinetic (PK) model developed by Svensson et al. (CPT: PSP, 2016)"),
                                    tags$li("PK-efficacy model based on two models developed by Svensson and Karlsson 
                                    (J Antimicrob Chemother, 2017) and Tanneau et al. (Br J Clin Pharmacol, 2020)"),
                                    tags$li("PK-safety model developed by Tanneau et al. (CPT: PSP, 2021)"),
                                    tags$li("Long-term outcome model developed by Lin et al. (J Antimicrob Chemother, 2024)")
                                )
                            )
                          ),
                          
                          # Navigation Structure
                          card(
                            card_header("Navigation Structure", style = "font-size: 16px; background-color: #E8ECEE;"),
                            card_body(
                              tags$span(tags$strong("The application consists of five main tabs:", style = "font-size: 14px;")),
                              tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                                tags$li("Dosing - Set dosing regimens"),
                                tags$li("Population - Set population characteristics"),
                                tags$li("Simulation - Set simulation parameters"),
                                tags$li("Results - Visualize and output results"),
                                tags$li("About - User manual and source code")
                              )
                            )
                          ),
                          col_widths = c(7, 5)
                        )
                    )
                ),
                
                # Dosing Card
                card(
                    card_header("1. Dosing Tab", style = "font-size: 20px; background-color: #CDD8DA;"),
                    card_body(
                        layout_columns(
                            # Select doses (formerly "For Each Regimen")
                            card(
                                card_header("Select Doses", style = "font-size: 16px; background-color: #E8ECEE;"),
                                card_body(
                                    tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                                        tags$li("Loading dose is optional. If no loading dose is added, maintainence dose will start from time 0"),
                                        tags$li(
                                            "Set loading dose and maintenance dose:",
                                            tags$ul(
                                                tags$li("Dose amount (mg)"),
                                                tags$li("Duration (days or weeks)"),
                                                tags$li("Frequency (twice daily, once daily, three times weekly, once weekly)")
                                            )
                                        )
                                    )
                                )
                            ),
                            
                            # Adding Multiple Regimens
                            card(
                                card_header("Add Multiple Regimens", style = "font-size: 16px; background-color: #E8ECEE;"),
                                card_body(
                                    tags$ul(style = "font-size: 14px; padding-left: 20px; line-height: 1.9;",
                                        tags$li("By default, Regimen 1 is always shown. ",
                                                tags$br(), 
                                                "The default values for regimens are the approved bedaquiline dosing",
                                                tags$br(), 
                                                "(400 mg daily for 2 weeks, followed by 200 mg thrice weekly for 22 weeks)"),
                                        tags$li("Use checkboxes to add Regimen 2 and Regimen 3",
                                                tags$br(), 
                                                "Note: Regimen 3 can only be added if Regimen 2 is active")
                                    )
                                )
                            ),
                            col_widths = c(6, 6)
                        )
                    )
                ),
                
                # Population Section
                card(
                    card_header("2. Population Tab", style = "font-size: 20px; background-color: #CDD8DA;"),
                    card_body(
                        tags$span("Choose to simulate in population level or individial level. In population mode, a group of subjects will be sampled from a large virtual population, 
                                   in which the covariate distribution was simulated using conditional distribution modelling (Smania and Johnsson, CPT: PSP, 2021). 
                                   Numbers of subjects can be set in the 3. Simulation tab.", 
                                   style = "font-size: 14px; padding-left: 5px; line-height: 1.9;")
                        ),
                        layout_columns(
                            # Individual Mode Card
                            card(
                                card_header("Individual Mode", style = "font-size: 16px; background-color: #E8ECEE;"),
                                card_body(
                                    tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                                        tags$li(
                                          tags$strong("Set continuous covariates (value of a typical individual is presented as default):"),
                                            tags$ul(
                                                tags$li("Age (years)"),
                                                tags$li("Baseline Body Weight (kg)"),
                                                tags$li("Baseline Albumin Concentration (g/dL)"),
                                                tags$li("Baseline Corrected Calcium Level (mmol/L). Calculated from the equation: Corrected Ca (mmol/L) = Measured Ca (mmol/L) + 0.8 × (4 − Albumin (g/dL))"),
                                                tags$li("Baseline Potassium Level (mmol/L)"),
                                                tags$li("Baseline Time-to-Positivity in MGIT Culture (days)")
                                            )
                                        ),
                                        tags$li(
                                          tags$strong("Set categorical covariates (mode of the population is presented as default):"),
                                            tags$ul(
                                                tags$li("Sex (Male/Female)"),
                                                tags$li("Race (Non-Black/Black)")
                                            )
                                        )
                                    )
                                )
                            ),
                            
                            # Population Mode Card
                            card(
                                card_header("Population Mode", style = "font-size: 16px; background-color: #E8ECEE;"),
                                card_body(
                                    tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                                        tags$li(
                                          tags$strong("Dataset Options:"),
                                            tags$ul(
                                                tags$li("Default: Use virtual population based on orginial data obtained from three TB clinical trials available in the TB-PACTS platform"),
                                                tags$li(
                                                    "Import: Upload custom population data by users",
                                                    tags$ul(
                                                        tags$li("Download population data specification template provided"),
                                                        tags$li("Fill in your population data following the template format and ensure all required columns are present"),
                                                        tags$li("Upload completed file in a CSV format")
                                                    )
                                                )
                                            )
                                        ),
                                        tags$li(
                                          tags$strong("If the default virtual population is used:"),
                                            tags$ul(
                                                tags$li("Ranges for continuous variables and proportions for categorical variables can be customized"),
                                                tags$li("Min/Max or proportions of each covariate in the virtual population are present as default")
                                            )
                                        )
                                    )
                                )
                            ),
                        col_widths = c(6,6)
                    ), 
                    # Half-life Modifier Card
                    card(
                        card_header("Half-life Modifier", style = "font-size: 16px; background-color: #E8ECEE;"),
                        card_body(
                            tags$span(
                                tags$strong("Half-life modifier is a parameter that can be used to adjust the half-life of mycobacterial load, 
                                reflecting how different background regimens influence bacterial elimination."),
                                "The default value is 0, which means no adjustment is made to the reference half-life reported in the developed model.",
                                tags$br(),
                                "Half-life of mycobacterial load is longer with % of a positive value (+) and shorter with % of a negative value (-) of the modifier.",
                                tags$br(),
                                "For example, a value of 30 indicates a 30% longer half-life, whereas -30 means a 30% shorter half-life.",
                                tags$br(),
                                tags$strong("The minimum values of half-life modifier is -100%"),
                                ", since a 100% shorter half-life is not possible.",
                                tags$br(),
                                "Note: Half-life modifier directly influences the influences sputum culture conversion in 
                                both the PK-efficacy model and long-term outcome model",
                                style = "font-size: 14px; padding-left: 5px; line-height: 1.9;")
                        )
                    ),
                    
                    # Concomitant Medication Card
                    card(
                        card_header("Concomitant Medication", style = "font-size: 16px; background-color: #E8ECEE;"),
                        card_body(
                            tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                                tags$li(
                                    tags$strong("Add concomnitant medications having drug-drug interaction effects on PK or QT prolongation for each regimen:"),
                                    tags$ul(
                                        tags$li("PK effects (None, Efavirenz, Lopinavir/r, Nevirapine, Rifampicin, Rifapentine)"),
                                        tags$li("QT effects (None, Clofazimine, Moxifloxacin, Both)")
                                    )
                                )
                            )
                        )
                    )
                ),
                
                # Simulation Section
                card(
                    card_header("3. Simulation Tab", style = "font-size: 20px; background-color: #CDD8DA;"),
                    card_body(
                        tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                            tags$li("Set numbers of MGIT culture replicates per sampling timepoint. The default value is 1 per sampling timepoint, maximum of 3"),
                            tags$li("Specify numbers of individuals per regimen for simulation. Same population will be applied for all regimens"),
                            tags$li("Set simulation time for PK, efficacy, and safety in weeks"),
                            tags$li("Set simulation time for long-term outcome in weeks"),
                            tags$li("Include interindividual variability (ON/OFF)"),
                            tags$li("Click \"Start simulation\" to begin")
                        )
                    )
                ),
                
                # Results Section
                card(
                    card_header("4. Results Tab", style = "font-size: 20px; background-color: #CDD8DA;"),
                    card_body(
                        layout_columns(
                            # Left Column (Overview)
                            card(
                                card_header("Overview", style = "font-size: 16px; background-color: #E8ECEE;"),
                                card_body(
                                    tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                                        tags$li(tags$strong("Pharmacokinetics:"),
                                                tags$br(),
                                                "users can visualize the concentration profiles of bedaquiline and its metabolite M2 under different settings",
                                                tags$ul(
                                                    tags$li("Full concentration profile"),
                                                    tags$li("Daily average concentraion"),
                                                    tags$li("Weekly average concentration")
                                                )
                                        ),
                                        tags$li(tags$strong("Efficacy"),
                                                "(% of negative culture samples in MGIT culture)"),
                                        tags$li(tags$strong("Safety"),
                                                "(QTcF interval)"),
                                        tags$li(tags$strong("Long-term outcome:"),
                                                tags$br(),
                                                "individual trajectory of outcomes will be shown if users choose to simulate in an individual level.",
                                                tags$br(),
                                                "If population mode is chosen, outcome will be presented in the form of proportions of patients in each state")
                                    )
                                )
                            ),
                            
                            # Right Column (Patient Characteristics and Output Files)
                            div(
                                # Patient Characteristics Card
                                card(
                                    card_header("Patient Characteristics", style = "font-size: 16px; background-color: #E8ECEE;"),
                                    card_body(
                                        tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                                            tags$li("Summary table and graphs for patient characterisics of the sampled virtual population or defined
                                            individual are illustrated in this subtab"),
                                            tags$li("Boxplots representing the covariate distribution the large dataset of virtual population")
                                        )
                                    )
                                ),
                                # Output Files Card
                                card(
                                    card_header("Output Files and Specifications", style = "font-size: 16px; background-color: #E8ECEE;"),
                                    card_body(
                                        tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.9;",
                                            tags$li("Pharmacokinetics data (PK_output.csv)"),
                                            tags$li("Efficacy data - Time to positivity signal (TTP_output.csv)"),
                                            tags$li("Safety data - QT (QT_output.csv)"),
                                            tags$li("Long-term outcome data (longTermOutcome_output.csv)"),
                                            tags$li("Virtual individual or population data (virtual_individual_or_population.csv)")
                                        )
                                    )
                                )
                            ),
                            col_widths = c(6, 6)
                        )
                    )
                ),
                col_widths = c(12)
            )
        )
    ),
    conditionalPanel(
        condition = "input.aboutTab == 'Source Code'",
        page_fillable(
            layout_columns(
                card(
                    card_header("Source Code", style = "font-size: 20px; background-color: #CDD8DA;"),
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
                                    tags$li("BDQ_Shiny_UI_About.R")
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
                                      tags$li("Virtual Population Template.csv File"),
                                      tags$li("Simulated Virtual Population.csv File")
                                    )
                            )
                        )
                    )
                ),
                card(
                    card_header("Instructions", style = "font-size: 20px; background-color: #CDD8DA;"),
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
)
