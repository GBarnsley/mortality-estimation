#' Standardize Governorate Names
#'
#' @param x A character vector of governorate names
#' @return A character vector with standardized names
#' @export
standardize_governorate <- function(x) {
  dplyr::case_when(
    x %in% c("Khan Zunis", "Khan Yunis") ~ "Khan Yunis",
    x %in%
      c(
        "Deir al Balah",
        "Dier al Balah",
        "Middle Area",
        "Deir al-Balah"
      ) ~ "Deir al-Balah",
    TRUE ~ x
  )
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
      df$age_group <- vapply(
        df$age_group_raw,
        function(x) {
          if (grepl("^[0-9]+ - [0-9]+$", x)) {
            parts <- strsplit(x, " - ")[[1]]
            return(paste(parts[2], parts[1], sep = "-"))
          }
          if (grepl("^\\+ [0-9]+$", x)) {
            return(paste0(gsub("^\\+ ", "", x), "+"))
          }
          return(x)
        },
        FUN.VALUE = character(1)
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

#' Load and combine population estimates from Excel
#'
#' @param path Path to the Excel file
#' @return A data frame with date, governorate, and total_population
#' @export
load_population_totals <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("Excel file not found at {.path {path}}")
  }

  sheets <- readxl::excel_sheets(path)
  required_sheets <- c("Other sources", "GAZA STRIP - OPT Pop Est")
  missing_sheets <- setdiff(required_sheets, sheets)
  if (length(missing_sheets) > 0) {
    cli::cli_abort(
      "Missing required sheets in Excel file: {.val {missing_sheets}}."
    )
  }

  # 1. Baseline from "Other sources"
  other_sources <- readxl::read_excel(path, sheet = "Other sources")

  baseline_pop <- other_sources |>
    dplyr::filter(is.na(Date), !is.na(`Population baseline`)) |>
    dplyr::select(
      governorate = Governorate,
      total_population = `Population baseline`
    ) |>
    dplyr::mutate(
      date = as.Date("2023-09-01"),
      governorate = standardize_governorate(governorate)
    )

  # 2. 2024 data from "Other sources"
  estimates_2024 <- other_sources |>
    dplyr::filter(!is.na(Date)) |>
    dplyr::mutate(
      date_num = as.numeric(Date),
      date = as.Date(date_num, origin = "1899-12-30"),
      governorate = standardize_governorate(Governorate)
    ) |>
    dplyr::mutate(
      total_population = dplyr::coalesce(
        Estimate,
        (`Lower bound` + `Upper bound`) / 2
      )
    ) |>
    dplyr::filter(!is.na(total_population)) |>
    dplyr::select(date, governorate, total_population)

  # 3. 2025 data from "GAZA STRIP - OPT Pop Est"
  opt_pop_est_raw <- readxl::read_excel(
    path,
    sheet = "GAZA STRIP - OPT Pop Est",
    skip = 3
  )

  estimates_2025 <- opt_pop_est_raw |>
    dplyr::rename(
      date = Date,
      governorate = Governorate,
      total_population = `Population estimate`
    ) |>
    dplyr::mutate(
      date = as.Date(date),
      governorate = standardize_governorate(governorate)
    ) |>
    dplyr::filter(
      governorate %in%
        c("North Gaza", "Gaza", "Deir al-Balah", "Khan Yunis", "Rafah")
    )

  combined_estimates <- dplyr::bind_rows(
    baseline_pop,
    estimates_2024,
    estimates_2025
  ) |>
    dplyr::group_by(governorate, date) |>
    dplyr::summarise(
      total_population = mean(total_population, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(governorate, date)

  if (nrow(combined_estimates) == 0) {
    cli::cli_abort(
      "No population estimates could be loaded from {.path {path}}."
    )
  }

  return(combined_estimates)
}
