# Restriction of anonymized data to reduce identifiability.

# Packages ----------------------------------------------------------------
library(here)
library(dplyr)
library(magrittr)
library(readr)

# Load and select data ---------------------------------------------------------------
restricted21 <- readr::read_csv("data/pulses21.csv") %>%
  dplyr::select("q_1_1",
                starts_with("q_29_"),
                starts_with("q_30_"),
                income,
                cohabitation,
                children,
                city_type,
                education,
                employment,
                gender,
                id,
                age,
                region,
                future_consumption,
                future_cons_binary)

# Save data frames in ~/data folder for simple use later
readr::write_csv(restricted21, here::here("data/restricted21.csv"))
