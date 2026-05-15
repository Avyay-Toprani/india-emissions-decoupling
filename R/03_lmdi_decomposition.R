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


# ============================================================
# LMDI Decomposition Calculations
# ============================================================

# Step 1: Define the log mean function
# Updated log mean function that handles NA values
log_mean <- function(a, b) {
  if (is.na(a) | is.na(b)) return(NA)
  if (a == b) return(a)
  if (a <= 0 | b <= 0) return(0)
  (a - b) / (log(a) - log(b))
}

log_mean_vec <- Vectorize(log_mean)

# Vectorise so it works on entire columns at once
log_mean_vec <- Vectorize(log_mean)

# Step 2: Prepare data for decomposition
# Join sectoral emissions with GDP data
lmdi_data <- edgar_sector_long %>%
  left_join(panel_data %>% select(year, gdp, co2_total), by = "year") %>%
  group_by(year) %>%
  mutate(
    # Share of each sector in total emissions
    sector_share = co2 / co2_total,
    # Emission intensity of each sector (emissions per unit GDP)
    emission_intensity = co2 / gdp
  ) %>%
  ungroup() %>%
  arrange(sector_group, year)

# Step 3: Calculate LMDI effects year by year
lmdi_results <- lmdi_data %>%
  group_by(sector_group) %>%
  mutate(
    # Log mean weight for each sector
    lm_weight = log_mean_vec(co2, lag(co2)),
    
    # Activity effect: change due to overall GDP growth
    activity_effect = lm_weight * log(gdp / lag(gdp)),
    
    # Structure effect: change due to shifts in sectoral composition
    structure_effect = lm_weight * log(sector_share / lag(sector_share)),
    
    # Intensity effect: change due to emission intensity changes
    intensity_effect = lm_weight * log(emission_intensity / lag(emission_intensity))
  ) %>%
  ungroup() %>%
  filter(!is.na(activity_effect)) %>%
  group_by(year) %>%
  summarise(
    activity_effect  = sum(activity_effect, na.rm = TRUE),
    structure_effect = sum(structure_effect, na.rm = TRUE),
    intensity_effect = sum(intensity_effect, na.rm = TRUE)
  ) %>%
  left_join(
    panel_data %>%
      arrange(year) %>%
      mutate(actual_change = co2_total - lag(co2_total)) %>%
      select(year, actual_change),
    by = "year"
  ) %>%
  mutate(
    sum_of_effects = activity_effect + structure_effect + intensity_effect
  )

# Verify decomposition adds up correctly
lmdi_results %>%
  select(year, actual_change, sum_of_effects) %>%
  mutate(difference = sum_of_effects - actual_change) %>%
  print(n = 22)

lmdi_results %>%
  select(year, activity_effect, structure_effect, intensity_effect, actual_change) %>%
  mutate(across(where(is.numeric), ~ round(., 0))) %>%
  print(n = 22)

# Save LMDI results
write_csv(lmdi_results, "outputs/tables/lmdi_results.csv")


# ============================================================
# Figure 2: LMDI Overview Chart
# ============================================================

# Add actual change line data
lmdi_summary <- lmdi_results %>%
  select(year, activity_effect, structure_effect, 
         intensity_effect, actual_change)

# Reshape effects to long format for stacked bars
lmdi_long <- lmdi_summary %>%
  select(year, activity_effect, structure_effect, intensity_effect) %>%
  pivot_longer(cols = c(activity_effect, structure_effect, intensity_effect),
               names_to = "effect",
               values_to = "value") %>%
  mutate(effect = case_when(
    effect == "activity_effect"  ~ "Activity Effect",
    effect == "structure_effect" ~ "Structure Effect",
    effect == "intensity_effect" ~ "Intensity Effect"
  ))

# Define colours
effect_colours <- c(
  "Activity Effect"  = "#e74c3c",
  "Structure Effect" = "#f39c12",
  "Intensity Effect" = "#2ecc71"
)

# Figure 2
lmdi_plot_overview <- ggplot() +
  geom_col(data = lmdi_long, 
           aes(x = year, y = value, fill = effect),
           position = "stack") +
  geom_line(data = lmdi_summary,
            aes(x = year, y = actual_change),
            color = "black", linewidth = 1, linetype = "solid") +
  geom_point(data = lmdi_summary,
             aes(x = year, y = actual_change),
             color = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = effect_colours) +
  scale_x_continuous(breaks = seq(2001, 2022, by = 2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "LMDI Decomposition of CO2 Emission Changes in India (2001 to 2022)",
    subtitle = "Black line shows actual emission change",
    x = "Year",
    y = "Emission Change (Gg CO2)",
    fill = "Effect",
    caption = "Source: EDGAR, World Bank WDI. Authors own calculations."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = "right",
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

lmdi_plot_overview

# Save Figure 2
ggsave("outputs/figures/lmdi_overview.png",
       plot = lmdi_plot_overview,
       width = 10,
       height = 6,
       dpi = 300)


# ============================================================
# Figure 3: Three Panel LMDI Chart
# ============================================================

# Activity Effect panel
p_activity <- ggplot(lmdi_results, aes(x = year, y = activity_effect)) +
  geom_col(fill = "#e74c3c") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = seq(2001, 2022, by = 3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Activity Effect",
       x = "Year",
       y = "Emission Change (Gg CO2)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Structure Effect panel
p_structure <- ggplot(lmdi_results, aes(x = year, y = structure_effect)) +
  geom_col(fill = "#f39c12") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = seq(2001, 2022, by = 3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Structure Effect",
       x = "Year",
       y = "Emission Change (Gg CO2)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Intensity Effect panel
p_intensity <- ggplot(lmdi_results, aes(x = year, y = intensity_effect)) +
  geom_col(fill = "#2ecc71") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = seq(2001, 2022, by = 3)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Intensity Effect",
       x = "Year",
       y = "Emission Change (Gg CO2)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combine three panels using patchwork
library(patchwork)

lmdi_plot_panels <- p_activity + p_structure + p_intensity +
  plot_annotation(
    title = "LMDI Decomposition Effects for India (2001 to 2022)",
    caption = "Source: EDGAR, World Bank WDI. Authors own calculations.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13)
    )
  )

lmdi_plot_panels

# Save Figure 3
ggsave("outputs/figures/lmdi_panels.png",
       plot = lmdi_plot_panels,
       width = 14,
       height = 5,
       dpi = 300)