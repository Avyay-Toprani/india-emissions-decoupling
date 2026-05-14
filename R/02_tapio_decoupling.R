# ============================================================
# Script 02: Tapio Decoupling Analysis
# Project: Decoupling or Illusion? India Emissions Analysis
# ============================================================

# Load packages
library(tidyverse)

# ============================================================
# Calculate Tapio Decoupling Index
# ============================================================

tapio_data <- panel_data %>%
  arrange(year) %>%
  mutate(
    # Percentage change in CO2 from previous year
    pct_change_co2 = (co2_total - lag(co2_total)) / lag(co2_total),
    
    # Percentage change in GDP from previous year
    pct_change_gdp = (gdp - lag(gdp)) / gdp,
    
    # Tapio index = % change CO2 / % change GDP
    tapio_index = pct_change_co2 / pct_change_gdp
  ) %>%
  filter(!is.na(tapio_index))


# ============================================================
# Classify Decoupling States
# ============================================================

tapio_data <- tapio_data %>%
  mutate(
    decoupling_state = case_when(
      # GDP growing (normal economic conditions)
      pct_change_gdp > 0 & tapio_index < 0       ~ "Strong Decoupling",
      pct_change_gdp > 0 & tapio_index >= 0 & tapio_index < 0.8  ~ "Weak Decoupling",
      pct_change_gdp > 0 & tapio_index >= 0.8 & tapio_index < 1.2 ~ "Expansive Coupling",
      pct_change_gdp > 0 & tapio_index >= 1.2    ~ "Expansive Negative Coupling",
      
      # GDP shrinking (recession conditions)
      pct_change_gdp < 0 & tapio_index > 0       ~ "Recessive Coupling",
      pct_change_gdp < 0 & tapio_index < 0       ~ "Recessive Decoupling",
      pct_change_gdp < 0 & tapio_index >= 0 & tapio_index < 0.8  ~ "Weak Negative Decoupling",
      
      TRUE ~ "Undefined"
    )
  )


# ============================================================
# View Results
# ============================================================

tapio_data %>%
  select(year, tapio_index, decoupling_state) %>%
  print(n = 22)


# ============================================================
# Visualise Tapio Decoupling Results
# ============================================================

# Define colours for each decoupling state
state_colours <- c(
  "Strong Decoupling"             = "#2ecc71",
  "Weak Decoupling"               = "#a8d8a8",
  "Expansive Coupling"            = "#f39c12",
  "Expansive Negative Coupling"   = "#e74c3c",
  "Recessive Coupling"            = "#9b59b6"
)

# Create the plot
tapio_plot <- ggplot(tapio_data, aes(x = year, y = tapio_index, fill = decoupling_state)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "gray20", linewidth = 0.7) +
  geom_hline(yintercept = 1.2, linetype = "dashed", color = "gray20", linewidth = 0.7) +
  scale_fill_manual(values = state_colours) +
  scale_x_continuous(breaks = seq(2001, 2022, by = 2)) +
  labs(
    title = "Tapio Decoupling Index for India (2001 to 2022)",
    x = "Year",
    y = "Tapio Decoupling Index",
    fill = "Decoupling State",
    caption = "Source: EDGAR, World Bank WDI. Authors own calculations."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display the plot
tapio_plot

# Save plot to outputs folder
ggsave("outputs/figures/tapio_plot.png", 
       plot = tapio_plot,
       width = 10, 
       height = 6, 
       dpi = 300)