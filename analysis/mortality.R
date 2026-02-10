# Mortality estimation for Gaza Strip
# Monthly estimates by cause, age, sex and governorate

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(here)

# Load helper functions
devtools::load_all(here::here())

# Constants
MORTALITY_RAW_PATH <- here::here(
  "data/raw-data/sensitive/Mortality_with_ICD10.csv"
)
MORTALITY_DERIVED_PATH <- here::here("data/derived-data/mortality.rds")
PLOT_DIR <- here::here("figures/exploration/mortality")

GOVERNORATE_FACILITY_LOOKUP <- read.csv(
  here::here("data/raw-data/facility_governorate_lookup.csv"),
  stringsAsFactors = FALSE
) |>
  mutate(
    Facility_Name = standardize_string(Facility_Name)
  ) |>
  pull(Governorate, name = Facility_Name)

START_DATE <- as.Date("2015-01-01")
END_DATE <- as.Date("2025-12-01")

# 1. Load data
message("Loading mortality data...")
if (!file.exists(MORTALITY_RAW_PATH)) {
  cli::cli_abort("Mortality data not found at {.path {MORTALITY_RAW_PATH}}")
}
raw_mortality <- read.csv(MORTALITY_RAW_PATH, stringsAsFactors = FALSE)

# Input validation
required_cols <- c(
  "Date_of_Death",
  "Date_of_Birth",
  "Gender",
  "Region",
  "ICD1_Code",
  "Hospital_Name"
)
missing_cols <- setdiff(required_cols, names(raw_mortality))
if (length(missing_cols) > 0) {
  cli::cli_abort(
    "Missing required columns in mortality data: {.val {missing_cols}}"
  )
}

# 2. Clean and process data
message("Processing mortality data...")
processed_mortality <- raw_mortality |>
  # Parse dates (MM/YYYY)
  mutate(
    date_death = as.Date(paste0("01/", Date_of_Death), format = "%d/%m/%Y"),
    date_birth = as.Date(paste0("01/", Date_of_Birth), format = "%d/%m/%Y")
  ) |>
  # Calculate age in years using lubridate period logic
  mutate(
    age_years = calculate_age(date_birth, date_death)
  ) |>
  # Categorize age
  mutate(
    age_group = categorize_age(age_years)
  ) |>
  # Standardize governorate
  mutate(
    governorate = standardize_governorate(Region)
  ) |>
  mutate(
    governorate = case_when(
      governorate %in% c("", "?????") ~ "Unknown",
      TRUE ~ governorate
    )
  ) |>
  # Impute Governorate based on Hospital_Name
  mutate(
    governorate = case_when(
      !(governorate %in% GAZA_GOVERNORATES) &
        standardize_string(Hospital_Name) %in%
          names(GOVERNORATE_FACILITY_LOOKUP) ~ GOVERNORATE_FACILITY_LOOKUP[
        standardize_string(Hospital_Name)
      ],
      TRUE ~ governorate
    )
  ) |>
  filter(governorate %in% GAZA_GOVERNORATES | governorate == "Unknown") |>
  # Determine cause (Trauma vs Non-Trauma)
  # Only for dates after conflict started
  mutate(
    cause = case_when(
      date_death >= CONFLICT_START_DATE & is_trauma(ICD1_Code) ~ "Trauma",
      TRUE ~ "Non-Trauma"
    )
  ) |>
  filter(date_death >= START_DATE, date_death <= END_DATE)

# 3. Aggregate counts
message("Aggregating monthly counts...")
mortality_counts <- processed_mortality |>
  group_by(
    date = floor_date(date_death, "month"),
    governorate,
    age_group,
    sex = Gender,
    cause
  ) |>
  summarise(deaths = n(), .groups = "drop")

# 4. Save results
message(paste("Saving results to", MORTALITY_DERIVED_PATH))
derived_dir <- dirname(MORTALITY_DERIVED_PATH)
if (!dir.exists(derived_dir)) {
  dir.create(derived_dir, recursive = TRUE)
}
saveRDS(mortality_counts, MORTALITY_DERIVED_PATH)

# 5. Exploratory Plots
message("Generating exploratory plots...")
if (!dir.exists(PLOT_DIR)) {
  dir.create(PLOT_DIR, recursive = TRUE)
}

# Deaths over time by cause
p_cause <- mortality_counts |>
  group_by(date, cause) |>
  summarise(deaths = sum(deaths), .groups = "drop") |>
  ggplot(aes(x = date, y = deaths, color = cause)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Monthly Deaths by Cause in Gaza Strip",
    subtitle = paste(
      "2015 - 2025 (Conflict Threshold:",
      CONFLICT_START_DATE,
      ")"
    ),
    x = "Date",
    y = "Number of Deaths"
  )

ggsave(
  file.path(PLOT_DIR, "deaths_by_cause.png"),
  p_cause,
  width = 10,
  height = 6
)

# Deaths by governorate
p_gov <- mortality_counts |>
  group_by(date, governorate) |>
  summarise(deaths = sum(deaths), .groups = "drop") |>
  ggplot(aes(x = date, y = deaths, color = governorate)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Monthly Deaths by Governorate",
    x = "Date",
    y = "Number of Deaths"
  )

ggsave(
  file.path(PLOT_DIR, "deaths_by_governorate.png"),
  p_gov,
  width = 10,
  height = 6
)

# Prop unknown
p_prop_unknown <- mortality_counts |>
  group_by(date) |>
  summarise(
    deaths = sum(deaths[governorate == "Unknown"]) / sum(deaths),
    .groups = "drop"
  ) |>
  ggplot(aes(x = date, y = deaths)) +
  geom_col() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_vline(
    xintercept = CONFLICT_START_DATE,
    linetype = "dashed"
  ) +
  labs(
    x = "Date",
    y = "% of Monthly deaths that have unknown governorate"
  )

ggsave(
  file.path(PLOT_DIR, "prop_deaths_unknown.png"),
  p_prop_unknown,
  width = 10,
  height = 6
)

message("Done!")
