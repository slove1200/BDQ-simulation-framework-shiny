# Bedaquiline Dose-PK-Efficacy/Safety-Outcome Modelling Framework

## Overview
This repository contains a Shiny application for Bedaquiline (BDQ) dosing optimization and simulation. The application provides an integrated framework for pharmacokinetics, efficacy (time-to-positivity, TTP), safety (QT interval), and long-term treatment outcomes of bedaquiline to help explore and visualize different bedaquiline dosing options.

The application incorporates several established models:
- Pharmacokinetic (PK) model developed by Svensson et al. (CPT: PSP, 2016)
- PK-efficacy model based on models developed by Svensson and Karlsson (J Antimicrob Chemother, 2017) and Tanneau et al. (Br J Clin Pharmacol, 2020)
- PK-safety model developed by Tanneau et al. (CPT: PSP, 2021)
- Long-term outcome model developed by Lin et al. (J Antimicrob Chemother, 2024)

Additionally, the application includes a dedicated TTP simulation module that allows users to generate individual time-to-positivity profiles without considering individual drug exposure but focusing on the overall combination therapy effect, as described by the half-life of mycobacterial load modifier. This module enables further analysis of TTP data and visualization of time-to-sputum culture conversion (TSCC).

## Repository Structure

The repository is organized into two main components:

### BDQ_Server
Contains the R scripts that handle the computational backend of the application:
- Pharmacokinetic (PK) modeling
- Time-to-positivity (TTP) simulations
- QT interval analysis
- Long-term outcome modeling (multi-state model)
- Virtual population generation
- Population summary statistics and visualization

### BDQ_UI
Contains the Shiny UI components and server logic:
- Main server script (`BDQ_Shiny_UI_Server.R`)
- UI modules for different sections of the application including:
  - User manual
  - Dosing interface
  - Individual or population characteristics and concomitant medication
  - Simulation configuration
  - Results visualization and output
  - Source code

## Features

- **Dosing Optimization**: Configure and optimize BDQ dosing regimens
  - Add loading dose (optional)
  - Set maintenance dose parameters: dose amount, duration, frequency
  - Add second maintenance dose (optional)
  - Support BDQ dose interruption and re-initiation
  - Compare up to three different regimens simultaneously
  - Default regimen follows the approved bedaquiline dosing (400 mg daily for 2 weeks, followed by 200 mg thrice weekly for 22 weeks)

- **Population Configuration**:
  - Choose between individual or population-level simulation
  - Individual mode: Set specific patient characteristics at baseline including age, weight, albumin concentration, calcium level, potassium level, TTP, sex, and race
  - Population mode: Use default virtual population based on three clinical trials (data available at the TB-PACTS platform) or import population data by users
  - Specify patient treatment history (treatment-naïve or treatment-experienced)
  - Set half-life modifier to adjust the elimination rate of mycobacterial load
  - Add concomitant medications with drug-drug interaction effects:
    - PK effects: None, Efavirenz, Lopinavir/r, Nevirapine, Rifampicin, Rifapentine
    - QT effects: None, Clofazimine, Moxifloxacin, or both

- **Simulation Settings**:
  - Specify number of individuals per regimen
  - Set number of MGIT culture replicates per sampling timepoint (1-3)
  - Configure simulation time for PK/efficacy/safety and long-term outcomes
  - Choose to simulate with or without interindividual variability

- **PK Modeling**: Simulate pharmacokinetic profiles
  - Visualize full, daily average or weekly average concentrations of bedaquiline and its metabolite M2
  - Account for drug-drug interactions with concomitant medications

- **TTP Simulation**: Model individual TTP profiles over time after start of treatment
  - Visualize proportions of negative cultures over time

- **QT Interval Analysis**: Assess cardiac safety of BDQ regimens
  - Predict QTcF interval over time
  - Account for effects of concomitant medications on QT interval

- **Long-term Outcome Modeling**: Analyze long-term treatment outcomes
  - Individual trajectory in different states (outcomes) visualization in individual mode
  - Proportions of patients in each state (outcome) in population mode

- **Visualization and Data Export**:
  - Comprehensive visualization of all simulation results
  - Patient characteristics summary tables and graphs
  - Export simulation data for further analysis:
    - Pharmacokinetics data
    - Efficacy data (TTP)
    - Safety data (QT)
    - Long-term outcome data
    - Virtual individual or population data

## Requirements
- R (version 4.4.1 or higher)
- Required R packages:
  - **Modeling and Simulation**: mrgsolve (PK/PD modeling and simulation), survival
  - **Data Manipulation**: dplyr, tidyr, purrr, stringr, zoo
  - **Visualization**: ggplot2, ggpubr, ggh4x, grid, gridExtra, patchwork
  - **Web Application**: shiny, bslib, bsicons, shinyjs, jquerylib, dipsaus, DT

## Usage

### Installation

1. Clone the repository:
   ```
   git clone https://github.com/slove1200/**bdq-optimization-framework**.git
   ```

2. Install required R packages:
   ```R
   # Run this in R or RStudio
   install.packages(c("mrgsolve", "dplyr", "tidyr", "purrr", "zoo", "ggplot2", 
                     "shiny", "grid", "ggpubr", "DT", "bslib", "dipsaus", 
                     "stringr", "bsicons", "ggh4x", "gridExtra", "patchwork", 
                     "shinyjs", "jquerylib", "survival"))
   ```

### Running the Application
1. Open the script in RStudio:
   - Open RStudio
   - Select File > Open file
   - Navigate to the cloned repository and open the `BDQ_Shiny_UI_Server.R` file
   - Run the scripts in `BDQ_Shiny_UI_Server.R` to launch the Shiny application

2. The application should open in your default web browser. If it doesn't, navigate to the URL displayed in the R console (typically http://127.0.0.1:xxxx).

### Using the Application
1. Check the user manual in the About tab which provides step-by-step guide of using the application
2. Set up the following information
   - dosing information of bedaquiline
   - individual or population characteristics and concomitant medications
   - simulation settings
4. Click the `Start simulation` button
5. Visualize the results of PK, efficacy, safety and long-term outcome in patients under different bedaquiline dosing strategies in the Results tab
6. Users can download the simulation outputs for further data analysis

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact
For questions, feedback, or collaboration inquiries:

- **Developer:** Yu-Jou Lin
- **Email:** yu-jou.lin@uu.se
- **Institution:** Department of Pharmacy, Uppsala University, Sweden
- **GitHub:** slove1200 (https://github.com/slove1200)

For issues related to this application, please use the [GitHub Issues](https://github.com/slove1200/**bdq-optimization-framework**/issues) page. 
