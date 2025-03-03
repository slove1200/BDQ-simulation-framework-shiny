# Bedaquiline Dose-PK-Efficacy/Safety-Outcome Modelling Framework

## Overview
This repository contains a Shiny application for **the evaluation of Bedaquiline dosing modifications**. 
The application provides tools for simulation of pharmacokinetics (PK) of bedaquiline and bedaquiline's main metabolite M2, time-to-positivity (TTP) simulation, QT interval analysis, 
and multi-state modeling describing long-term treatment outcomes. In addition, individual TTP can be simulated and outputed for further data analysis...

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
- **BDQ Dosing Selection**: Select BDQ dosing regimens of interest for simulation
- **PK Modeling**: Simulate pharmacokinetic profiles of BDQ and M2
- **TTP Simulation**: Model time-to-positivity for treatment monitoring
- **QT Interval Analysis**: Assess cardiac safety
- **Multi-State Modeling**: Analyze disease progression and treatment outcomes
- **Virtual Population**: Generate and analyze virtual patient populations
- **Interactive Visualization**: Explore results through interactive plots and tables

## Requirements
- R (version 4.4.1 or higher)
- Required R packages:
  - **Modeling and Simulation**: mrgsolve (PK/PD modeling and simulation), survival
  - **Data Manipulation**: dplyr, tidyr, purrr, stringr, zoo
  - **Visualization**: ggplot2, ggpubr, ggh4x, grid, gridExtra, patchwork
  - **Web Application**: shiny, bslib, bsicons, shinyjs, jquerylib, dipsaus, DT

## Usage
### Installation
1. Clone the repository to your local path:
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
   - Simulation settings
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
