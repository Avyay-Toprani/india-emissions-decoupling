# ============================================================
# Script 04: EKC Regression Analysis
# Project: Decoupling or Illusion? India Emissions Analysis
# ============================================================

# Load packages
library(tidyverse)
library(lmtest)
library(sandwich)

# ============================================================
# Prepare EKC Data
# ============================================================

ekc_data <- panel_data %>%
  mutate(
    # Per capita CO2 emissions (Gg CO2 per person)
    co2_pc = co2_total / population,
    
    # GDP per capita (constant 2015 USD per person)
    gdp_pc = gdp / population,
    
    # GDP per capita squared (for inverted U test)
    gdp_pc_sq = gdp_pc^2
  ) %>%
  select(year, co2_pc, gdp_pc, gdp_pc_sq,
         renewables_share_energy, energy_per_gdp)

# ============================================================
# Run EKC Regression
# ============================================================

# Base EKC model - quadratic regression
ekc_model <- lm(co2_pc ~ gdp_pc + gdp_pc_sq + 
                  renewables_share_energy + energy_per_gdp,
                data = ekc_data)

# View results
summary(ekc_model)

# ============================================================
# Calculate EKC Turning Point
# ============================================================

beta1 <- coef(ekc_model)["gdp_pc"]
beta2 <- coef(ekc_model)["gdp_pc_sq"]

turning_point <- -beta1 / (2 * beta2)
turning_point

# ============================================================
# Compare turning point to actual GDP per capita
# ============================================================

ekc_data %>%
  select(year, gdp_pc) %>%
  mutate(gdp_pc = round(gdp_pc, 2)) %>%
  print(n = 23)

# ============================================================
# Save regression summary and turning point
# ============================================================

# Save turning point value
ekc_summary <- data.frame(
  parameter = c("beta1_gdp_pc", "beta2_gdp_pc_sq", 
                "turning_point_usd", "r_squared"),
  value = c(beta1, beta2, turning_point, 
            summary(ekc_model)$r.squared)
)

write_csv(ekc_summary, "outputs/tables/ekc_summary.csv")

# ============================================================
# Visualise EKC Results
# ============================================================

# Extended GDP range including projection beyond 2022
gdp_range <- data.frame(
  gdp_pc = seq(min(ekc_data$gdp_pc), turning_point * 1.05, length.out = 300),
  renewables_share_energy = mean(ekc_data$renewables_share_energy, na.rm = TRUE),
  energy_per_gdp = mean(ekc_data$energy_per_gdp, na.rm = TRUE)
) %>%
  mutate(gdp_pc_sq = gdp_pc^2)

gdp_range$co2_pc_fitted <- predict(ekc_model, newdata = gdp_range)

# GDP per capita at 2032
gdp_2032 <- last_gdp_pc * (1 + recent_growth)^(2032 - 2022)

ekc_plot <- ggplot() +
  # Fitted curve - observed period
  geom_line(data = gdp_range %>% filter(gdp_pc <= last_gdp_pc),
            aes(x = gdp_pc, y = co2_pc_fitted),
            color = "#2c3e50", linewidth = 1.2) +
  # Fitted curve - projected period (dashed)
  geom_line(data = gdp_range %>% filter(gdp_pc >= last_gdp_pc),
            aes(x = gdp_pc, y = co2_pc_fitted),
            color = "#2c3e50", linewidth = 1.2, linetype = "dashed") +
  # Actual observed points
  geom_point(data = ekc_data,
             aes(x = gdp_pc, y = co2_pc),
             color = "#e74c3c", size = 2.5) +
  # Year labels on selected points
  geom_text(data = ekc_data %>% 
              filter(year %in% c(2000, 2005, 2010, 2015, 2019, 2022)),
            aes(x = gdp_pc, y = co2_pc, label = year),
            vjust = -0.8, size = 3, color = "#e74c3c") +
  # Turning point vertical line
  geom_vline(xintercept = turning_point,
             linetype = "dashed", color = "#f39c12", linewidth = 0.8) +
  # Turning point annotation
  annotate("text", x = turning_point + 20,
           y = max(gdp_range$co2_pc_fitted) * 0.92,
           label = paste0("Turning point\nUSD ", round(turning_point, 0),
                          "\n(~", turning_year, ")"),
           hjust = 0, size = 3.2, color = "#f39c12") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Environmental Kuznets Curve for India (2000 to 2022) with Projection",
    subtitle = "Solid line: observed period. Dashed line: projected trajectory. Red points: actual observations.",
    x = "GDP per Capita (constant 2015 USD)",
    y = "CO2 Emissions per Capita (Gg CO2)",
    caption = "Source: EDGAR, World Bank WDI. Authors own calculations."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40")
  )

ekc_plot

# ============================================================
# Project GDP per capita to find approximate turning point year
# ============================================================

# Calculate average annual growth rate of GDP per capita (2015-2022)
recent_growth <- ekc_data %>%
  filter(year >= 2015) %>%
  summarise(
    avg_growth = mean((gdp_pc - lag(gdp_pc)) / lag(gdp_pc), na.rm = TRUE)
  ) %>%
  pull(avg_growth)

# Project forward until turning point is reached
last_gdp_pc <- ekc_data %>% filter(year == 2022) %>% pull(gdp_pc)

projected <- data.frame(year = 2023:2045) %>%
  mutate(
    gdp_pc = last_gdp_pc * (1 + recent_growth)^(year - 2022)
  ) %>%
  filter(gdp_pc <= turning_point * 1.1)

# Approximate year when turning point is reached
turning_year <- projected %>%
  filter(gdp_pc >= turning_point) %>%
  slice(1) %>%
  pull(year)

turning_year

#===================
# Save Figure 4
#===================
ggsave("outputs/figures/ekc_plot.png",
       plot = ekc_plot,
       width = 10,
       height = 6,
       dpi = 300)