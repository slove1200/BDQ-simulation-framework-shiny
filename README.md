# Bedaquiline Dose-Pharmacokinetic-Pharmacodynamic-Outcome Modelling Framework

## Overview
This repository contains a Shiny application for **the evaluation of Bedaquiline (BDQ) dosing modifications**. 
The application provides tools for simulation of pharmacokinetics of BDQ and BDQ's main metabolite M2, time-to-positivity (TTP) simulation, QT interval analysis, 
and multi-state modeling describing long-term treatment outcomes. In addition, individual TTP can be simulated and outputed for further data analysis...

## Repository Structure

The repository is organized into two main components:

### BDQ_Server
Contains the R scripts that handle the computational backend of the application:
- Pharmacokinetic (PK) modeling
- Time-to-positivity (TTP) simulations
- QT interval analysis
- Long-term outcome modeling (MSM)
- Virtual population generation
- Population summary statistics and visualization

### BDQ_UI
Contains the Shiny UI components and server logic:
- Main server script (`BDQ_Shiny_UI_Server.R`)
- UI modules for different sections of the application including:
  - User manual and Source code
  - Dosing interface
  - Simulation configuration
  - Results visualization and output
  - Individual/Population summary tools

## Features

- **BDQ Dosing Selection**: Select BDQ dosing regimens of interest for simulation
- **PK Modeling**: Simulate pharmacokinetic and pharmacodynamic profiles
- **TTP Simulation**: Model time-to-positivity for treatment monitoring
- **QT Interval Analysis**: Assess cardiac safety of BDQ regimens
- **Multi-State Modeling**: Analyze disease progression and treatment outcomes
- **Virtual Population**: Generate and analyze virtual patient populations
- **Interactive Visualization**: Explore results through interactive plots and tables

## Requirements

- R (version 4.4.1 or higher)
- Required R packages:
  - shiny
  - dplyr
  - tidyr
  - ggplot2
  - **mrgsolve**
  - DT
  - and other dependencies specified in the code

## Usage

1. Clone the repository
2. Open the project in RStudio
3. Install required packages
4. Run the Shiny application by executing the main server script

## Development

This application is designed with a modular architecture to facilitate maintenance and extension. The server-side computations are separated from the UI components to improve performance and maintainability.

## License

[Specify your license information here]

## Contact

[Your contact information or organization details]
