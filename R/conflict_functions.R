#' Load and Clean Conflict Data
#'
#' @param path Path to the ACLED CSV file
#' @return A data frame with cleaned conflict events for Gaza Strip
#' @export
load_conflict_data <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("Conflict data file not found at {.path {path}}")
  }

  raw_data <- read.csv(path, stringsAsFactors = FALSE)

  # Column verification
  required_cols <- c(
    "EVENT_DATE",
    "ADMIN1",
    "ADMIN2",
    "EVENT_TYPE",
    "SUB_EVENT_TYPE",
    "FATALITIES"
  )
  missing_cols <- setdiff(required_cols, names(raw_data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Conflict data missing required columns: {.val {missing_cols}}"
    )
  }

  cleaned_data <- raw_data |>
    dplyr::filter(ADMIN1 == "Gaza Strip") |>
    dplyr::mutate(
      date = lubridate::dmy(EVENT_DATE),
      governorate = standardize_governorate(ADMIN2)
    ) |>
    dplyr::filter(governorate %in% GAZA_GOVERNORATES) |>
    dplyr::select(
      date,
      governorate,
      event_type = EVENT_TYPE,
      sub_event_type = SUB_EVENT_TYPE,
      fatalities = FATALITIES
    )

  return(cleaned_data)
}
