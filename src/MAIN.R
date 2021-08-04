###########################################################
##################### MAIN ################################
###########################################################

# load Project------------------------------------------------------------------
# Set the dir variable as the path to where the project folder is stored
dir <- "C:/Users/pablo/Desktop/"
setwd(paste0(dir,"Tesis_PLL"))
library(ProjectTemplate)
load.project()
set.seed(33)

# Install package nbastatR from Github:-----------------------------------------
# devtools::install_github("abresler/nbastatR")

# Download data ----------------------------------------------------------------
#' This script is left merely as reference.
#' The wrangled data is located at the data directory of this project
#' and loads automatically when the project is loaded.
#source("src/01_DataDownload.R")

# Define functions for computing lineup transition matrices
source("src/02_TransitionMatrices.R")

# Define functions for running regressions
source("src/03_Regressions.R")

# Experiments
source("src/04_Experiments.R")

