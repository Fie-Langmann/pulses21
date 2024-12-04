# 2021 analysis updated December 4, 2024
# Load packages
library(tidyverse)
library(magrittr)
library(dplyr)
library(readr)
library(writexl)
library(openxlsx)
library(flextable)
library(gtsummary)
library(broom)
library(here)
library(knitr)
library(kableExtra)

# Load data ---------------------------------------------------------------
pulses21 <- readr::read_csv("data/pulses21.csv")
heavy <- readr::read_csv("data/heavy.csv")
light <- readr::read_csv("data/light.csv")

# Table 1 -----------------------------------------------------------------
# Table 1. Baseline characteristics
table1 <- pulses21 %>%
  select(q_1_1, age, gender, income, education, cohabitation, employment, children, region) %>%
  tbl_summary(by = q_1_1,
              statistic = list(all_continuous() ~  "{median} ({p10}, {p90})",
                               all_categorical() ~ "{p} %"),
              digits = all_continuous() ~ 1,
              missing_text = "n missing") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Table 1. Baseline characteristics of participants") %>%
  as_flex_table()

flextable::save_as_html(table1, path = here("doc/table1.html"))

# Distribution of drivers and barriers -----------------------
# Prevalence of "yes" for each driver and barrier in each subset data frame
table_prevalences <- pulses21 %>%
  select(q_1_1, starts_with("q_29_"), starts_with("q_30_")) %>%
  tbl_summary(by = q_1_1,
              statistic = list(all_continuous() ~ "{mean}",
                               all_categorical() ~ "{p} %"),
              digits = all_continuous() ~ 1,
              missing_text = "n missing") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Table 2. Distribution of barriers and drivers") %>%
  as_flex_table()

flextable::save_as_html(table_prevalences, path = here("doc/table_prevalences.html"))

# OR for intention ------------------------------------------
# I will create a loop to conduct most of my logistic regression
# The results output below both contains OR and 95%CI for each barrier and
# driver exposure
logistic_regression <- function(df) {
  results <- list()
  exposure_var <- grep("^(q_29|q_30)", names(df), value = TRUE)
  for (var in exposure_var) {
    formula <- as.formula(paste("future_cons_binary ~", var))
    model <- glm(formula, family = binomial(link = "logit"), data = df)
    tidy_model <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
    results[[var]] <- tidy_model
  }
  return(results)
}

all_OR <- logistic_regression(pulses21)
heavy_OR <- logistic_regression(heavy)
light_OR <- logistic_regression(light)

# Creating a data frame to combine the results
table_OR <- data.frame(
  var = character(),
  all_OR = character(),
  heavy_OR = character(),
  light_OR = character(),
  stringsAsFactors = FALSE)

# extract the estimate and CI for each variable
extract_estimate_conf <- function(data_list, var_name) {
  row <- data_list[[var_name]][2, ]  # Extract the second row for the variable of interest
  sprintf("%.2f (%.2f; %.2f)", row$estimate, row$conf.low, row$conf.high)
}

# Get the list of variable names from the "all" dataset as the reference
var_names <- names(all_OR)

# Loop over each variable name to populate the combined table
for (var_name in var_names) {
  # Extract and format estimates from each list
  all_estimate <- extract_estimate_conf(all_OR, var_name)
  heavy_estimate <- extract_estimate_conf(heavy_OR, var_name)
  light_estimate <- extract_estimate_conf(light_OR, var_name)

  # Append the result to the combined table
  table_OR <- rbind(
    table_OR,
    data.frame(
      var = var_name,
      all_OR = all_estimate,
      heavy_OR = heavy_estimate,
      light_OR = light_estimate,
      stringsAsFactors = FALSE
    )
  )
}

# save results with kable
html_content <- kable(table_OR, format = "html", table.attr = "style='width:50%;'")
write(html_content, file = "doc/table_OR.html")
