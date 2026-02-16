#' Extract unique values from a df
#' @export
get_var_mapping <- function(df, name, value) {
  dplyr::select(df, all_of(c(value, name))) |>
    unique() |>
    dplyr::pull(.data[[name]], .data[[value]])
}

#' Categorize Age into Standard Groups
#'
#' @param age Numeric age in years
#' @return A character vector of age groups
#' @export
categorize_age <- function(age) {
  # Census groups: 0-4, 5-9, ..., 90-94, 95+
  breaks <- AGE_BREAKS
  labels <- c(paste(breaks[-length(breaks)], breaks[-1] - 1, sep = "-"), "95+")

  cut(
    age,
    breaks = c(breaks, Inf),
    labels = labels,
    right = FALSE,
    include.lowest = TRUE
  ) |>
    as.factor()
}
