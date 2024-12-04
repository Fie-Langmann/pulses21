# Data management
# This script contains data management code for changing and creating variables
# You only have to run this once, unless you make mistakes/changes to your raw
# data and need to integrate that in your data frames

# Packages
library(readxl)
library(tidyverse)
library(here)
library(vroom)
library(foreign)
library(dplyr)
library(magrittr)
library(readr)

# Load data
pulses21 <- read_csv(here::here("data/pulses21.csv"))

# convert variables to integers
# Extract column names that start with "q1", "q2", and "q3"
char_var_names <- colnames(pulses21)[
  grepl("^q_1|^q_2|^q_3|^q_4|^q_5|^q_6|^q_7|^q_8|^q_9", colnames(pulses21))]
# Convert to integers
pulses21 <- pulses21 %>%
  mutate(across(all_of(char_var_names), as.integer))

# Consumption of legumes in the future
# Recode scoring number across columns to create composite measure of future consumption
# q28_1 until q28_7; 99 should be coded as NA and NA should be removed when creating binary score (na.rm = TRUE)
for (col_name in c("q_28_1", "q_28_2", "q_28_3", "q_28_4", "q_28_5", "q_28_6", "q_28_7")) {
  pulses21[[col_name]] <- ifelse(pulses21[[col_name]] == 99, NA, pulses21[[col_name]])}


# Create combined score with binary cut-off
pulses21 <- pulses21 %>% mutate(
    # average intended future consumption
    future_consumption = rowMeans(select(., starts_with("q_28_")), na.rm = TRUE),
    # binary cut-offs; <= 3 = eat less or same [coded 0]; > 3 = eat more [coded 1]
    future_cons_binary = (ifelse(future_consumption > 3, 1, 0)))

# rename variables
pulses21 <- pulses21 %>%
  rename(income = q_7,
         cohabitation = q_5,
         children = q_6,
         city_type = q_8,
         education = q_9,
         employment = q_10,
         region = q_4,
         gender = q_2,
         age = q_3)


# recoding variables for clarity following the semantics in
# "Pulses21_Raw data and Questionnaire.xlsx"
pulses21 <- pulses21 %>% mutate(
  q_1_1 = case_when(
    q_1_1 == 1 ~ "Heavy consumer",
    q_1_1 == 2 ~ "Light consumer"
  ),
  gender = case_when(
    gender == 1 ~ "man",
    TRUE ~ "woman"),
  gender = as.factor(gender),
  income = case_when(
    stringr::str_detect(income, "\\b(1|2)\\b") ~ "<200,000",
    stringr::str_detect(income, "\\b(3|4)\\b") ~ "200,000-400,000",
    stringr::str_detect(income, "\\b(5|6|7|8|9|10|11|12|13|14)\\b") ~ ">400,000",
    income == 16 ~ "Don't know",
    TRUE ~ "Don't wish to tell"
  ),
  income = as.factor(income),
  cohabitation = case_when(
    cohabitation == 1 ~ "Alone",
    cohabitation == 2 ~ "With partner",
    cohabitation == 3 ~ "Family with child(ren)",
    TRUE ~ "Shared home/other"
  ),
  cohabitation = as.factor(cohabitation),
  education = case_when(
    education == 1 | education == 2 ~ "Long 5+ years",
    stringr::str_detect(education, "3|5") ~ "Bachelor or professional bachelor",
    stringr::str_detect(education, "4|7") ~ "Vocational or short",
    stringr::str_detect(education, "6|8") ~ "Elementary or high school",
    TRUE ~ "Other"
  ),
  education = as.factor(education),
  employment = case_when(
    employment == 1 | employment == 4 ~ "Fulltime/self-employed",
    stringr::str_detect(employment, "2|3") ~ "Parttime/freelance",
    stringr::str_detect(employment, "5|6|8") ~ "Outside the labour market",
    stringr::str_detect(employment, "7") ~ "Student",
    TRUE ~ "Other"
  ),
  employment = as.factor(employment),
  region = case_when(
    region == 1 ~"Capital",
    region == 2 ~ "Zealand",
    region == 3 ~ "Southern Denmark",
    region == 4 ~ "Central Jutland",
    region == 5 ~ "Northern Jutland"
  ),
  region = as.factor(region),
  children = case_when(
    children == 0 ~ "0",
    children == 1 ~ "1",
    TRUE ~ "2+"
  ),
  children = as.factor(children)
)


# create subset of heavy and light consumers of pulses
# 2021 data
heavy <- filter(pulses21, q_1_1== 1 | !q_1_1==2)
light <- filter(pulses21, q_1_1==2 | !q_1_1==1)

# Save data frames in ~/data folder for simple use later
readr::write_csv(pulses21, here::here("data/pulses21.csv"))
readr::write_csv(heavy, here::here("data/heavy.csv"))
readr::write_csv(light, here::here("data/light.csv"))
