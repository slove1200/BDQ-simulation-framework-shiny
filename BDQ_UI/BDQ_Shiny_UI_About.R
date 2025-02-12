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
                        layout_columns(
                          # Application Purpose
                          card(
                            card_header("Application Purpose", style = "font-size: 20px; background-color: #E8ECEE;"),
                            card_body(
                              div(style = "font-size: 14px; padding-left: 5px; line-height: 1.5;",
                                  "This interactive application is aimed for interested users to visualize and explore 
                                   different dosing strategies of bedaquiline in an integrated pharmacokinetic, pharmacodynamic and 
                                   long-term outcome modelling framework using patient characteristics, concomitant medications in flexible simulation settings."
                              )
                            )
                          ),
                          
                          # Navigation Structure
                          card(
                            card_header("Navigation Structure", style = "font-size: 20px; background-color: #E8ECEE;"),
                            card_body(
                              tags$span(tags$strong("The application consists of five main tabs:", style = "font-size: 14px;")),
                              div(style = "font-size: 14px; padding-left: 5px; line-height: 1.5;",
                                  tags$ul(
                                    tags$li("Dosing"),
                                    tags$li("Population"),
                                    tags$li("Simulation"),
                                    tags$li("Results"),
                                    tags$li("About")
                                  )
                              )
                            )
                          ),
                          col_widths = c(6, 6)
                        )
                    )
                ),
                
                # Dosing Card
                card(
                    card_header("1. Dosing Tab", style = "font-size: 20px; background-color: #CDD8DA;"),
                    card_body(
                        tags$span(tags$strong("Adding Multiple Regimens:", style = "font-size: 14px;")),
                        tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.5;",
                            tags$li("By default, Regimen 1 is always shown. The default values for regimens are the approved bedaquiline dosing (400 mg daily for 2 weeks, followed by 200 mg thrice weekly for 22 weeks)"),
                            tags$li("Use checkboxes to add Regimen 2 and Regimen 3. Note: Regimen 3 can only be added if Regimen 2 is active")
                        ),
                        tags$span(tags$strong("For Each Regimen:", style = "font-size: 14px;")),
                        tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.5;",
                            tags$li("Loading dose is optional. If no loading dose is added, maintainence dose will start from time 0"),
                            tags$li(
                                "Set loading dose and maintenance dose:",
                                tags$ul(
                                    tags$li("Dose amount (mg)"),
                                    tags$li("Duration (days or weeks)"),
                                    tags$li("Dosing frequency (twice daily, once daily, three times weekly, once weekly)")
                                )
                            )
                        )
                    )
                ),
                
                # Population Section
                card(
                    card_header("2. Population Tab", style = "font-size: 20px; background-color: #CDD8DA;"),
                    card_body(
                        tags$span("Choose to simulate in population level or individial level. In population mode, a group of subjects will be sampled from a large virtual population, 
                                              in which the covariate distribution was simulated using conditional distribution modelling (Smania and Johnsson, CPT: PSP, 2021). 
                                              Numbers of subjects can be set by users in the 3. Simulation tab.", style = "font-size: 14px;")
                        ),
                        layout_columns(
                            # Individual Mode Card
                            card(
                                card_header("Individual Mode", style = "font-size: 16px; background-color: #E8ECEE;"),
                                card_body(
                                    tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.5;",
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
                                                tags$li("Race (Non-Black/Black)"),
                                                tags$li("Drug Resistance (MDR-TB, pre-XDR-TB, XDR-TB)")
                                            )
                                        )
                                    )
                                )
                            ),
                            
                            # Population Mode Card
                            card(
                                card_header("Population Mode", style = "font-size: 16px; background-color: #E8ECEE;"),
                                card_body(
                                    tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.5;",
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
                                          tags$strong("If the default virtual population is used, range and proportion of population parameters can be set:"),
                                            tags$ul(
                                                tags$li("Drug resistance profile percentages"),
                                                tags$li("Proportion of black race"),
                                                tags$li("Population details (ranges for continuous covariates). Min/Max of each parameter in the virtual population are present as default, and can be reset to the default value")
                                            )
                                        )
                                    )
                                )
                            ),
                            col_widths = c(6,6)
                        ),
                        
                        # Concomitant Medication Card
                        card(
                            card_header("Concomitant Medication", style = "font-size: 16px; background-color: #E8ECEE;"),
                            card_body(
                                tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.5;",
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
                        layout_columns(
                            # Advanced Settings Card
                            card(
                                card_header("Advanced Settings", style = "font-size: 16px; background-color: #E8ECEE;"),
                                card_body(
                                    tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.5;",
                                        tags$li(
                                            "Select PK-TTP model based on the patient's treatment history:",
                                            tags$ul(
                                                tags$li(tags$strong("Treatment-Naive:"), " For patients with no prior TB treatment before starting bedaquiline."),
                                                tags$li(tags$strong("Treatment-Experienced:"), " For patients with prior TB treatment before initiating bedaquiline.")
                                            )
                                        ),
                                        tags$li("Set numbers of MGIT culture replicates at each visit"),
                                        tags$li("Set the value of bacterial clearance modifier in % faster/slower (Adjust this parameter to modify the speed of bacterial clearance. The following graph illustrates how the percentage change in this parameter affects the simulated Month 2 and Month 6 conversion rates based on the PK-TTP model)"),
                                        tags$ul(
                                            tags$li("Bacterial clearance is increased by a positive value (+) and decreased by a negative value (-) of the modifier. For example, a value of 30 indicates a 30% faster clearance, whereas -30 means a 30% slower clearance"),
                                            tags$li("Note that bacterial clearance directly influences sputum culture conversion in both the PK-efficacy model and long-term outcome model")
                                        )
                                    )
                                )
                            ),
                            
                            # Simulation Settings Card
                            card(
                                card_header("Simulation Settings", style = "font-size: 16px; background-color: #E8ECEE;"),
                                card_body(
                                    tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.5;",
                                        tags$li("Specify numbers of individuals per regimen for simulation. Same population will be applied for all regimens as default"),
                                        tags$li("Set simulation time for PK, efficacy, and safety in weeks"),
                                        tags$li("Set simulation time for long-term outcome in weeks"),
                                        tags$li("Include interindividual variability (ON/OFF)"),
                                        tags$li("Click \"Start simulation\" to begin")
                                    )
                                )
                            ),
                            col_widths = c(6,6)
                        )
                    )
                ),
                
                # Results Section
                card(
                    card_header("4. Results Tab", style = "font-size: 20px; background-color: #CDD8DA;"),
                    card_body(
                        layout_columns(
                            # Overview Card
                            card(
                                card_header("Overview", style = "font-size: 16px; background-color: #E8ECEE;"),
                                card_body(
                                    tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.5;",
                                        tags$li("Pharmacokinetics visualization options: users can visualize the concentration profiles of bedaquiline and its metabolite M2 under different settings including 1) full concentration profile, 2) daily average concentraion, and 3) weekly average concentration"),
                                        tags$li("Efficacy (% of negative culture samples in MGIT culture)"),
                                        tags$li("Safety (QTcF interval)"),
                                        tags$li("Long-term outcome:individual trajectory of outcomes will be shown if users choose to simulate in an individual level. If population mode is chosen (in the Population tab), outcome will be presented in the form of proportions of patients in each state")
                                      )
                                  )
                                ),
                            
                            # Patient Characteristics Card
                            card(
                                card_header("Patient Characteristics", style = "font-size: 16px; background-color: #E8ECEE;"),
                                card_body(
                                    tags$ul(style = "font-size: 14px; margin-left: 0; padding-left: 20px; line-height: 1.5;",
                                        tags$li("Summary table and graphs for patient characterisics of the virtual population or individual are illustrated in this subtab"),
                                        tags$li("Summary table of population characteristics"),
                                        tags$li("Visualization of continuous variables distribution")
                                    )
                                )
                            ),
                            col_widths = c(6,6)
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
                                      tags$li("Virtual Population Template .csv File"),
                                      tags$li("Simulated Virtual Population .csv File")
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
