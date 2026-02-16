# Population estimation for Gaza Strip
# Monthly estimates by age, sex and governorate

library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(here)

# Load helper functions
devtools::load_all(here::here())

# Constants
CENSUS_RAW_PATH <- here::here("data/raw-data/sensitive/2017-census.csv")
POP_EST_RAW_PATH <- here::here(
  "data/raw-data/sensitive/population_by_governorate.csv"
)
POP_EST_DERIVED_PATH <- here::here("data/derived-data/population.rds")

START_DATE <- as.Date("2016-01-01")
END_DATE <- as.Date("2025-12-31") # End of 2025
GOVERNORATES <- GAZA_GOVERNORATES

# 1. Clean census data to get proportions
message("Cleaning census data...")
census_age_sex_props <- clean_census_data(CENSUS_RAW_PATH)

if (nrow(census_age_sex_props) == 0) {
  cli::cli_abort("Census proportions data is empty.")
}

# 2. Load population totals from CSV
message("Loading population totals...")
observed_pop_totals <- load_governorate_population_csv(POP_EST_RAW_PATH)

# 3. Create monthly timeline and interpolate totals
message("Interpolating and averaging monthly totals...")
interpolated_pop_monthly <- interpolate_population_monthly(
  observed_pop_totals,
  START_DATE,
  END_DATE,
  GOVERNORATES
)

if (any(is.na(interpolated_pop_monthly$total_population))) {
  cli::cli_abort("Found NA values in interpolated population totals.")
}

# 4. Apply census proportions to totals
message("Applying age/sex proportions...")

# We need to ensure the output format matches:
# date (Date), governorate (chr), sex (Named chr), age_group (Factor), count (num)

final_population_estimates <- interpolated_pop_monthly |>
  inner_join(
    census_age_sex_props,
    by = join_by(governorate),
    relationship = "many-to-many"
  ) |>
  mutate(count = total_population * prop) |>
  mutate(
    sex_label = unlist(list(Males = "Male", Females = "Female")[sex])
  ) |>
  select(date, governorate, sex_raw = sex, sex = sex_label, age_group, count)

# The original output had 'sex' as a named character vector where names were "Males"/"Females"
# and values were "Male"/"Female".
final_population_estimates <- final_population_estimates |>
  mutate(sex = setNames(sex, sex_raw)) |>
  select(date, governorate, sex, age_group, count)

# 5. Save results
message(paste("Saving results to", POP_EST_DERIVED_PATH))
derived_dir <- dirname(POP_EST_DERIVED_PATH)
if (!dir.exists(derived_dir)) {
  dir.create(derived_dir, recursive = TRUE)
}
saveRDS(final_population_estimates, POP_EST_DERIVED_PATH)

# 6. Exploratory Plots
message("Generating exploratory plots...")
PLOT_DIR <- here::here("figures/exploration/population")
if (!dir.exists(PLOT_DIR)) {
  dir.create(PLOT_DIR, recursive = TRUE)
}

# Total Gaza Strip population over time
p_total_gaza <- final_population_estimates |>
  group_by(date) |>
  summarise(total_pop = sum(count), .groups = "drop") |>
  ggplot(aes(x = date, y = total_pop)) +
  geom_line(linewidth = 1, color = "steelblue") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Population Estimates for Gaza Strip",
    subtitle = "2016 - 2025",
    y = "Total Population",
    x = "Date"
  )
ggsave(
  file.path(PLOT_DIR, "total_gaza_population.png"),
  p_total_gaza,
  width = 10,
  height = 6
)

# Total population over time by governorate
p_total <- final_population_estimates |>
  group_by(date, governorate) |>
  summarise(total_pop = sum(count), .groups = "drop") |>
  ggplot(aes(x = date, y = total_pop, color = governorate)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Total Population Estimates by Governorate",
    subtitle = "2016 - 2025",
    y = "Total Population",
    x = "Date"
  )
ggsave(
  file.path(PLOT_DIR, "total_population.png"),
  p_total,
  width = 10,
  height = 6
)

# Sex distribution over time
p_sex <- final_population_estimates |>
  group_by(date, governorate, sex) |>
  summarise(total_pop = sum(count), .groups = "drop") |>
  ggplot(aes(x = date, y = total_pop, color = sex)) +
  geom_line(linewidth = 1) +
  facet_wrap(vars(governorate), scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Population Count by Sex and Governorate Over Time",
    y = "Population Count",
    x = "Date"
  )
ggsave(
  file.path(PLOT_DIR, "sex_distribution.png"),
  p_sex,
  width = 12,
  height = 8
)

# Age distribution over time
p_age <- final_population_estimates |>
  group_by(date, governorate, age_group) |>
  summarise(count = sum(count), .groups = "drop") |>
  ggplot(aes(x = date, y = count, color = age_group, group = age_group)) +
  geom_line() +
  facet_wrap(vars(governorate), scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 6)) +
  labs(
    title = "Age Group Population Trends by Governorate",
    y = "Population Count",
    x = "Date"
  )
ggsave(
  file.path(PLOT_DIR, "age_distribution.png"),
  p_age,
  width = 14,
  height = 10
)

message("Done!")
