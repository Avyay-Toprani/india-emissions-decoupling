# ============================================================
# Script 05: Spatial Analysis
# Project: Decoupling or Illusion? India Emissions Analysis
# ============================================================

# Load packages
library(sf)
library(tmap)
library(rmapshaper)
library(tidyverse)

# ============================================================
# Load India State Boundary Shapefile
# ============================================================

india_states <- st_read("data/raw/shapefile/gadm41_IND_1.shp")

# Check what it looks like
glimpse(india_states)

# Quick plot to verify shapefile
plot(st_geometry(india_states))

# ============================================================
# Load Ember State Level Electricity Data
# ============================================================

ember_raw <- read_csv("data/raw/ember_india_states.csv")

# Check structure
glimpse(ember_raw)

# See what variables are available
ember_raw %>%
  distinct(Category, Subcategory, Variable) %>%
  print(n = 50)

# ============================================================
# Filter for State Level CO2 Intensity (2022)
# ============================================================

state_intensity <- ember_raw %>%
  filter(
    Category == "Power sector emissions",
    Subcategory == "CO2 intensity",
    Variable == "CO2 intensity",
    Year == 2022
  ) %>%
  select(State, Year, Value, Unit) %>%
  rename(state = State,
         year = Year,
         co2_intensity = Value,
         unit = Unit) %>%
  filter(!state %in% c("India", "India Total"))

# Check results
print(state_intensity)

# ============================================================
# Fix State Name Mismatches
# ============================================================

state_intensity_clean <- state_intensity %>%
  mutate(state = case_when(
    state == "Delhi" ~ "NCT of Delhi",
    state == "Dadra and Nagar Haveli and Daman and Diu" ~ "Dadra and Nagar Haveli",
    TRUE ~ state
  )) %>%
  filter(state != "Ladakh")  # Not in shapefile

# ============================================================
# Merge Shapefile with Emission Intensity Data
# ============================================================

india_map_data <- india_states %>%
  left_join(state_intensity_clean, by = c("NAME_1" = "state"))

# Check how many states matched
sum(!is.na(india_map_data$co2_intensity))

# ============================================================
# Create Choropleth Map
# ============================================================

tmap_mode("plot")

india_map <- tm_shape(india_map_data) +
  tm_polygons(
    col = "co2_intensity",
    title = "CO2 Intensity\n(gCO2/kWh)",
    palette = "YlOrRd",
    style = "quantile",
    n = 5,
    border.col = "white",
    border.alpha = 0.5,
    textNA = "No Data",
    colorNA = "gray90"
  ) +
  tm_layout(
    main.title = "Power Sector CO2 Intensity by State, India (2022)",
    main.title.size = 1,
    main.title.fontface = "bold",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE
  ) +
  tm_compass(position = c("left", "bottom")) +
  tm_scalebar(position = c("left", "bottom"))

india_map

# Save map
tmap_save(india_map, 
          filename = "outputs/figures/spatial_map.png",
          width = 10,
          height = 8,
          dpi = 300)

# ============================================================
# Save processed spatial data
# ============================================================

state_intensity_clean %>%
  write_csv("data/processed/state_co2_intensity.csv")