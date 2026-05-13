# ============================================================
# Script 01: Data Cleaning
# Project: Decoupling or Illusion? India Emissions Analysis
# ============================================================

# Install packages if not already installed
if (!require(readxl)) install.packages("readxl")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(janitor)) install.packages("janitor")

# Load packages
library(readxl)
library(tidyverse)
library(janitor)


# ============================================================
# Load EDGAR CO2 Data
# ============================================================

# Read the EDGAR excel file - "totals by country" sheet
edgar_raw <- read_excel("data/raw/IEA_EDGAR_CO2_1970_2023.xlsx", 
                        sheet = "TOTALS BY COUNTRY",
                        skip = 9)


# ============================================================
# Filter for India and study period (2000-2022)
# ============================================================

# Check what India's country code is
edgar_raw %>% 
  filter(Name == "India") %>%
  select(Country_code_A3) %>%
  distinct()

# Filter for India only - total emissions
edgar_india <- edgar_raw %>%
  filter(Country_code_A3 == "IND") %>%
  clean_names() %>%
  select(ipcc_annex,
         c_group_im24_sh,
         country_code_a3,
         name,
         substance,
         y_2000:y_2022)


# ============================================================
# Load EDGAR Sectoral CO2 Data (for LMDI decomposition)
# ============================================================

# Load IPCC 2006 sectoral sheet
edgar_sector_raw <- read_excel("data/raw/IEA_EDGAR_CO2_1970_2023.xlsx", 
                               sheet = "IPCC 2006",
                               skip = 9)

# Filter sectoral data for India only
edgar_sector_india <- edgar_sector_raw %>%
  filter(Country_code_A3 == "IND") %>%
  clean_names() %>%
  select(ipcc_code_2006_for_standard_report,
         ipcc_code_2006_for_standard_report_name,
         substance,
         fossil_bio,
         y_2000:y_2022)


# ============================================================
# Save cleaned data to processed folder
# ============================================================

write_csv(edgar_india, "data/processed/edgar_india_total.csv")
write_csv(edgar_sector_india, "data/processed/edgar_sector_india.csv")


# ============================================================
# Load World Bank GDP Data
# ============================================================

library(readxl)
library(tidyverse)
library(janitor)

gdp_raw <- read_csv("data/raw/worldbank_gdp_india.csv", skip = 4)

# Filter for India and select study period
gdp_india <- gdp_raw %>%
  filter(`Country Code` == "IND") %>%
  select(`Country Name`, `Country Code`, `1960`:`2023`) %>%
  clean_names()

# Reshape from wide to long format
gdp_india_long <- gdp_india %>%
  pivot_longer(cols = x2000:x2022,
               names_to = "year",
               values_to = "gdp") %>%
  mutate(year = as.integer(gsub("x", "", year))) %>%
  select(year, gdp)

# Save cleaned GDP data
write_csv(gdp_india_long, "data/processed/gdp_india_clean.csv")


# ============================================================
# Merge EDGAR and GDP data into unified panel
# ============================================================

# First reshape edgar_india to long format
edgar_india_long <- edgar_india %>%
  pivot_longer(cols = y_2000:y_2022,
               names_to = "year",
               values_to = "co2") %>%
  mutate(year = as.integer(gsub("y_", "", year))) %>%
  group_by(year) %>%
  summarise(co2_total = sum(co2, na.rm = TRUE))

# Merge with GDP data
panel_data <- edgar_india_long %>%
  left_join(gdp_india_long, by = "year")

# Save unified panel dataset
write_csv(panel_data, "data/processed/panel_data.csv")