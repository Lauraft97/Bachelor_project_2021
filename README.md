# Bachelor_project_2021
This GitHub contains the scripts related to the bachelor project "Transmision of fasciolosis on Danish dairy farms" made in August to December 2021. 
The aim of the project was to build a model which could simulate the transmission of fasciolosis on a single farm. 

The Git repository contains 3 folders:

- data: Contains farm data and weather data

- R: Contains all scripts related to the project

- results: Contains figures and RData files with result data

The R folder:

- 00_sim_function.R: The simulation model of this project as a function

- 00A_run_simulation.R: A scripts that runs the model and saves the outputs

- 00B_farm_info.R: Functions that specifies farm specific parameters (used as an input in the model)

- 01_load_clean_farm_data.R: Loads and cleans farm data

- 02_transmission_probability: Script that calculates the transmission probabilities for each farm at each visit (Table 2 in the report)

- 02A_transmission_prob_plot.R: Creates figure 11

- 03_egg_excretion_NB_fit.R: Creates figure 3

- 04_weather_preparation.R: Prepares the weather data for the model

- 04_weather_plots.R: Creates figure 5 and 6 as well as table 3 

- 05_ODE_rates_scalar_plots.R: Creates figure 4, 7 and 8

- 06_sensitivity_analysis.R: Performs the sensitivity analysis and creates figure 14 and 15. Also creates table in appendix D

- 07_plot_functions.R: Functions to create all result figures for IBM and validation

- 07_IBM+validation_results_plot.R: Prepares data from simulations and creates figure 16-18 and 27-30

- 07_ODE_data_preparation.R: Prepares ODE data from simulations and saves RData files

- 07_ODE_results_plots.R: Creates figure 19-26 

- 08_appendix_plots.R: Creates the plots for appendices E, H, I and J

- 08_egg_death_rate_simulation.R: Creates plots for appendix G

- 08_stable_population.R: Creates plot for appendix C

- 99_functions.R: Contains functions used in the different scripts. 

- Folder Data_exploration: Contains draft scripts for weather and farm data exploration

- Folder functions: Contains 2 functions. They are also present in the R file 00_sim_function.R





