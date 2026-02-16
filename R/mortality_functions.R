#' Standardize String
#'
#' Removes everything but letters and converts to uppercase for consistent matching
#' @param x A character vector
#' @return A standardized character vector
#' @export
standardize_string <- function(x) {
  stringr::str_to_upper(x) |>
    stringr::str_extract_all("[A-Z]+") |>
    purrr::map_chr(stringr::str_c, collapse = "")
}

#' Calculate age in years from two dates
#'
#' @param start_date Date of birth
#' @param end_date Date of death
#' @return Age in years
#' @export
calculate_age <- function(start_date, end_date) {
  floor(as.numeric(
    lubridate::as.period(lubridate::interval(start_date, end_date)),
    "years"
  ))
}

#' Determine if cause is trauma (V-Y codes)
#'
#' @param icd_code ICD-10 code
#' @return Logical indicating if it's trauma
#' @export
is_trauma <- function(icd_code) {
  # External causes of morbidity and mortality (V01-Y98)
  grepl("^[V-Y]", icd_code)
}
