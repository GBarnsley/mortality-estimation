#' Standardize Governorate Names
#'
#' @param x A character vector of governorate names
#' @return A character vector with standardized names
#' @export
standardize_governorate <- function(x) {
  standardized <- dplyr::case_match(
    x,
    c("Khan Zunis", "Khan Yunis") ~ "Khan Yunis",
    c(
      "Deir al Balah",
      "Dier al Balah",
      "Middle Area",
      "Deir al-Balah",
      "Deir El Balah"
    ) ~ "Deir al-Balah",
    c("Gaza City", "Gaza") ~ "Gaza",
    c("Jabalia", "North Gaza") ~ "North Gaza",
    .default = x
  )

  # Check if any non-NA names are not in the canonical list
  unknown <- setdiff(na.omit(standardized), GAZA_GOVERNORATES)
  if (length(unknown) > 0) {
    cli::cli_warn("Unknown governorate names detected: {.val {unknown}}")
  }

  return(standardized)
}

#' Clean 2017 Census Data for Gaza
#'
#' @param path Path to the census CSV file
#' @return A long data frame with proportions by governorate, age_group, and sex
#' @export
clean_census_data <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("Census file not found at {.path {path}}")
  }

  raw_data <- read.csv(path, header = FALSE, stringsAsFactors = FALSE)

  if (ncol(raw_data) < 12) {
    cli::cli_abort(
      "Census data format unexpected: expected at least 12 columns, found {ncol(raw_data)}."
    )
  }

  # Find the row where Gaza governorates are listed as headers to target the correct table
  gaza_header_row <- which(apply(raw_data, 1, function(x) {
    any(x == "North Gaza")
  }))[1]
  if (is.na(gaza_header_row)) {
    cli::cli_abort("Could not find 'North Gaza' header in census data.")
  }

  extract_sex_data <- function(sex_label) {
    # Find rows for this sex AFTER the Gaza header row
    sex_start_idx <- which(
      raw_data[, 12] == sex_label & seq_len(nrow(raw_data)) > gaza_header_row
    )[1]
    if (is.na(sex_start_idx)) {
      cli::cli_abort(
        "Sex label {.val {sex_label}} not found after Gaza header in column 12."
      )
    }

    # Identify rows containing age group data
    # Patterns: "0-4" (or "4 - 0") and "+ 95"
    age_group_pattern <- "^[0-9]+ - [0-9]+|^\\+ [0-9]+"
    age_rows <- which(grepl(age_group_pattern, raw_data[, 12]))

    cols_to_extract <- c(
      "North Gaza" = 7,
      "Gaza" = 8,
      "Dier al Balah" = 9,
      "Khan Yunis" = 10,
      "Rafah" = 11
    )

    # We only want rows between the sex label and the next "Total"
    next_total_idx <- which(
      raw_data[, 12] == "Total" & seq_len(nrow(raw_data)) > sex_start_idx
    )[1]
    if (is.na(next_total_idx)) {
      cli::cli_abort(
        "Could not find 'Total' row after sex label {.val {sex_label}}."
      )
    }

    relevant_age_rows <- age_rows[
      age_rows > sex_start_idx & age_rows < next_total_idx
    ]

    # Filter out summary rows
    summary_groups <- c("14 - 0", "64 - 15", "+ 65")

    data_list <- lapply(names(cols_to_extract), function(gov_name) {
      col_idx <- cols_to_extract[[gov_name]]

      age_labels_raw <- raw_data[relevant_age_rows, 12]
      count_vals_raw <- raw_data[relevant_age_rows, col_idx] |>
        gsub(pattern = ",", replacement = "") |>
        as.numeric()

      df <- data.frame(
        governorate = standardize_governorate(gov_name),
        sex = sex_label,
        age_group_raw = age_labels_raw,
        census_count = count_vals_raw,
        stringsAsFactors = FALSE
      )

      # Filter out summaries
      df <- df[!(df$age_group_raw %in% summary_groups), ]

      # Correct labels: "4 - 0" -> "0-4", "9 - 5" -> "5-9", "+ 95" -> "95+"
      df$age_group <- purrr::map_vec(
        df$age_group_raw,
        function(x) {
          # Parse numeric start of age group
          if (grepl("^[0-9]+ - [0-9]+$", x)) {
            # Handles "4 - 0", "9 - 5", etc.
            parts <- strsplit(x, " - ")[[1]]
            age <- as.numeric(parts[2])
            return(categorize_age(age))
          }
          if (grepl("^\\+ [0-9]+$", x)) {
            # Handles "+ 95"
            age <- as.numeric(gsub("^\\+ ", "", x))
            return(categorize_age(age))
          }
          return(x)
        }
      )

      df$age_group_raw <- NULL
      df
    })

    do.call(rbind, data_list)
  }

  census_males <- extract_sex_data("Males")
  census_females <- extract_sex_data("Females")

  census_long <- rbind(census_males, census_females)

  # Calculate proportions by governorate
  census_props <- census_long |>
    dplyr::group_by(governorate) |>
    dplyr::mutate(prop = census_count / sum(census_count)) |>
    dplyr::ungroup()

  return(census_props)
}

#' Load and combine population estimates from CSV
#'
#' @param path Path to the CSV file
#' @return A data frame with date, governorate, and total_population
#' @export
load_governorate_population_csv <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("CSV file not found at {.path {path}}")
  }

  raw_data <- read.csv(path, stringsAsFactors = FALSE)

  clean_data <- raw_data |>
    dplyr::mutate(
      date = as.Date(date),
      governorate = standardize_governorate(governorate)
    ) |>
    dplyr::rename(total_population = pop_estimate) |>
    dplyr::filter(governorate %in% GAZA_GOVERNORATES) |>
    # Ensure every date present in the input has all governorates (fill with 0)
    # This prevents interpolation from bridging over implied zeros
    tidyr::complete(
      date,
      governorate = GAZA_GOVERNORATES,
      fill = list(total_population = 0)
    ) |>
    dplyr::arrange(governorate, date)

  return(clean_data)
}

#' Interpolate Population to Monthly Estimates
#'
#' @param observed_pop A data frame with date, governorate, and total_population
#' @param start_date Date to start estimates
#' @param end_date Date to end estimates
#' @param governorates Character vector of governorates to include
#' @return A data frame with date (1st of month) and total_population
#' @export
interpolate_population_monthly <- function(
  observed_pop,
  start_date,
  end_date,
  governorates
) {
  # Create a daily sequence to interpolate
  daily_template <- expand.grid(
    date = seq(min(observed_pop$date), max(observed_pop$date), by = "day"),
    governorate = governorates,
    stringsAsFactors = FALSE
  )

  # Merge available data into template and interpolate daily
  interpolated_pop_daily <- daily_template |>
    dplyr::left_join(observed_pop, by = dplyr::join_by(date, governorate)) |>
    dplyr::group_by(governorate) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      total_population = zoo::na.approx(total_population, x = date, rule = 2)
    ) |>
    dplyr::ungroup()

  # Average daily estimates into monthly values and rescale to ensure steady total
  interpolated_pop_monthly <- interpolated_pop_daily |>
    dplyr::mutate(month_date = lubridate::floor_date(date, "month")) |>
    dplyr::filter(
      month_date >= start_date,
      month_date <= lubridate::floor_date(end_date, "month")
    ) |>
    dplyr::group_by(month_date, governorate) |>
    dplyr::summarise(
      total_population = mean(total_population),
      .groups = "drop"
    ) |>
    dplyr::rename(date = month_date) |>
    # Rescale step: Ensure total across governorates matches expected_total
    dplyr::group_by(date) |>
    dplyr::ungroup()

  return(interpolated_pop_monthly)
}
