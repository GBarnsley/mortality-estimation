# Conflict events estimation for Gaza Strip
# Monthly counts by governorate

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(here)

# Load helper functions
devtools::load_all(here::here())

# Constants
CONFLICT_RAW_PATH <- here::here(
  "data/raw-data/sensitive/israel_palestine_full_data_up_to-2025-11-28.csv"
)
CONFLICT_DERIVED_PATH <- here::here("data/derived-data/conflict.rds")

START_DATE <- as.Date("2023-10-01")
END_DATE <- as.Date("2025-12-01")
GOVERNORATES <- GAZA_GOVERNORATES

# 1. Load and clean data
message("Loading conflict data...")
conflict_events <- load_conflict_data(CONFLICT_RAW_PATH)

# 2. Aggregate monthly
message("Aggregating monthly counts...")

# Create template for all months and governorates
target_months <- seq(START_DATE, END_DATE, by = "month")

monthly_template <- expand.grid(
  date = target_months,
  governorate = GOVERNORATES,
  stringsAsFactors = FALSE
)

monthly_conflict <- conflict_events |>
  mutate(month_date = floor_date(date, "month")) |>
  group_by(month_date, governorate) |>
  summarise(
    num_events = n(),
    num_fatalities = sum(fatalities, na.rm = TRUE),
    .groups = "drop"
  ) |>
  right_join(
    monthly_template,
    by = join_by(month_date == date, governorate)
  ) |>
  mutate(
    num_events = replace_na(num_events, 0),
    num_fatalities = replace_na(num_fatalities, 0)
  ) |>
  rename(date = month_date) |>
  arrange(date, governorate)

# 3. Save results
message(paste("Saving results to", CONFLICT_DERIVED_PATH))
derived_dir <- dirname(CONFLICT_DERIVED_PATH)
if (!dir.exists(derived_dir)) {
  dir.create(derived_dir, recursive = TRUE)
}
saveRDS(monthly_conflict, CONFLICT_DERIVED_PATH)

# 4. Plot
message("Generating exploratory plots...")
PLOT_DIR <- here::here("figures/exploration/conflict")
if (!dir.exists(PLOT_DIR)) {
  dir.create(PLOT_DIR, recursive = TRUE)
}

p_events <- monthly_conflict |>
  ggplot(aes(x = date, y = num_events, color = governorate)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Monthly Conflict Events by Governorate",
    subtitle = "October 2023 - December 2025",
    y = "Number of Events",
    x = "Month"
  )

ggsave(
  file.path(PLOT_DIR, "monthly_events.png"),
  p_events,
  width = 10,
  height = 6
)

p_fatalities <- monthly_conflict |>
  ggplot(aes(x = date, y = num_fatalities, color = governorate)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Monthly Fatalities by Governorate",
    subtitle = "October 2023 - December 2025",
    y = "Number of Fatalities",
    x = "Month"
  )

ggsave(
  file.path(PLOT_DIR, "monthly_fatalities.png"),
  p_fatalities,
  width = 10,
  height = 6
)

message("Done!")
