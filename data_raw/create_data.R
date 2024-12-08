# How pulses21.csv data was created

# This script contains information and code for loading the pulses21 data as csv files
# Only rerun this, if you messed up
# Packages and libraries
library(readxl)
library(tidyverse)
library(here)
library(vroom)
library(foreign)
library(dplyr)
library(magrittr)
library(readr)


#Pulses21 data
Pulses21_Raw_data_and_Questionnaire <- read_excel("data_raw/Pulses21_Raw data and Questionnaire.xlsx",
                                                  sheet = "Raw data and Questionna")
View(Pulses21_Raw_data_and_Questionnaire)

#Create a .csv file to work from
readr::write_csv(Pulses21_Raw_data_and_Questionnaire, here::here("data_raw/pulses21_raw_data.csv"))



# Remodel raw data -----------------------------------------------------------
raw_data <- read_csv(here::here("data_raw/pulses21_raw_data.csv"))

# Change column names to not be the question text but question number
new_column_names <- raw_data[1,] # remove the first row with column names to have the column question number as the variable name
new_column_names <- as.character(new_column_names) # Convert the row to character to avoid issues with factors
colnames(raw_data) <- new_column_names # Set the extracted row as column names
raw_data <- raw_data[-1,] # Remove the first row from the dataframe
raw_data <- raw_data[,-42] # Remove duplicate names in column 42 (same as column 41)

# Save with a new name in data folder
readr::write_csv(raw_data, here::here("data/pulses21.csv"))



# The column names are unfit for coding, let's change them to snake case
pulses21_file <- here::here("data/pulses21.csv")
pulses21_file_prep <- vroom::vroom(
  pulses21_file,
  col_select = 1:153,
  col_types = cols(),
  .name_repair = snakecase::to_snake_case
)
# Save data with snake case in the ~/data folder to work with in the analyses
readr::write_csv(pulses21_file_prep, here::here("data/pulses21.csv"))

