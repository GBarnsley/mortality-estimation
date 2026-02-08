# Population estimation for Gaza Strip
# Monthly estimates by age, sex and governorate

library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(readxl)
library(here)

# Load helper functions
devtools::load_all(here::here())

# Constants
CENSUS_RAW_PATH <- here::here("data/raw-data/2017-census.csv")
POP_EST_RAW_PATH <- here::here("data/raw-data/GAZA STRIP - OPT Pop Est.xlsx")
POP_EST_DERIVED_PATH <- here::here("data/derived-data/population.rds")

START_DATE <- as.Date("2023-09-01")
END_DATE <- as.Date("2025-12-01")
GOVERNORATES <- GAZA_GOVERNORATES

# 1. Clean census data to get proportions
message("Cleaning census data...")
census_age_sex_props <- clean_census_data(CENSUS_RAW_PATH)

if (nrow(census_age_sex_props) == 0) {
  cli::cli_abort("Census proportions data is empty.")
}

# 2. Load population totals from Excel
message("Loading population totals...")
observed_pop_totals <- load_population_totals(POP_EST_RAW_PATH)

# 3. Create monthly timeline and interpolate totals
message("Interpolating monthly totals...")
target_months <- seq(START_DATE, END_DATE, by = "month")

monthly_template <- expand.grid(
  date = target_months,
  governorate = GOVERNORATES,
  stringsAsFactors = FALSE
)

# Merge available data into template and interpolate
interpolated_pop_monthly <- monthly_template |>
  left_join(observed_pop_totals, by = join_by(date, governorate)) |>
  group_by(governorate) |>
  # Add the observed data points that might not fall on the 1st of the month
  bind_rows(observed_pop_totals |> filter(!(date %in% target_months))) |>
  arrange(governorate, date) |>
  mutate(total_population = na.approx(total_population, x = date, rule = 2)) |>
  filter(date %in% target_months) |>
  ungroup()

if (any(is.na(interpolated_pop_monthly$total_population))) {
  cli::cli_abort("Found NA values in interpolated population totals.")
}

# 4. Apply census proportions to totals
message("Applying age/sex proportions...")
final_population_estimates <- interpolated_pop_monthly |>
  inner_join(
    census_age_sex_props,
    by = join_by(governorate),
    relationship = "many-to-many"
  ) |>
  mutate(count = total_population * prop) |>
  select(date, governorate, sex, age_group, count) |>
  mutate(
    sex = unlist(list(Males = "Male", Females = "Female")[sex])
  )

# 5. Save results
message(paste("Saving results to", POP_EST_DERIVED_PATH))
derived_dir <- dirname(POP_EST_DERIVED_PATH)
if (!dir.exists(derived_dir)) {
  dir.create(derived_dir, recursive = TRUE)
}
saveRDS(final_population_estimates, POP_EST_DERIVED_PATH)

# 6. Exploratory Plots
message("Generating exploratory plots...")
library(ggplot2)
PLOT_DIR <- here::here("figures/exploration/population")
if (!dir.exists(PLOT_DIR)) {
  dir.create(PLOT_DIR, recursive = TRUE)
}

# Total population over time
p_total <- final_population_estimates |>
  group_by(date, governorate) |>
  summarise(total_pop = sum(count), .groups = "drop") |>
  ggplot(aes(x = date, y = total_pop, color = governorate)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Total Population Estimates by Governorate",
    subtitle = "September 2023 - December 2025",
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
