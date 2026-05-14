# ============================================================
# Script 03: LMDI Decomposition
# Project: Decoupling or Illusion? India Emissions Analysis
# ============================================================

# Load packages
library(tidyverse)
library(janitor)

# ============================================================
# Group EDGAR sectors into broad categories
# ============================================================

edgar_sector_grouped <- edgar_sector_india %>%
  mutate(
    sector_group = case_when(
      ipcc_code_2006_for_standard_report_name == 
        "Main Activity Electricity and Heat Production" ~ "Power",
      ipcc_code_2006_for_standard_report_name %in% c(
        "Manufacturing Industries and Construction",
        "Petroleum Refining - Manufacture of Solid Fuels and Other Energy Industries",
        "Cement production", "Lime production", "Glass Production",
        "Other Process Uses of Carbonates", "Chemical Industry",
        "Metal Industry") ~ "Industry",
      ipcc_code_2006_for_standard_report_name %in% c(
        "Civil Aviation", "Road Transportation no resuspension",
        "Railways", "Water-borne Navigation", "Other Transportation") ~ "Transport",
      ipcc_code_2006_for_standard_report_name == 
        "Residential and other sectors" ~ "Residential",
      ipcc_code_2006_for_standard_report_name %in% c(
        "Solid Fuels", "Oil and Natural Gas", 
        "Fossil fuel fires") ~ "Fugitive",
      ipcc_code_2006_for_standard_report_name %in% c(
        "Liming", "Urea application") ~ "Agriculture",
      TRUE ~ "Other"
    )
  )

# ============================================================
# Reshape to long format and aggregate by sector group
# ============================================================

edgar_sector_long <- edgar_sector_grouped %>%
  pivot_longer(cols = y_2000:y_2022,
               names_to = "year",
               values_to = "co2") %>%
  mutate(year = as.integer(gsub("y_", "", year))) %>%
  group_by(year, sector_group) %>%
  summarise(co2 = sum(co2, na.rm = TRUE), .groups = "drop")

# Verify sector totals match overall total
edgar_sector_long %>%
  group_by(year) %>%
  summarise(co2_sector_total = sum(co2)) %>%
  left_join(panel_data %>% select(year, co2_total), by = "year") %>%
  mutate(difference = co2_sector_total - co2_total) %>%
  print(n = 23)

# Save sectoral long format data
write_csv(edgar_sector_long, "data/processed/edgar_sector_long.csv")