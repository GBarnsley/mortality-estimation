#' Extract unique values from a df
#' @export
get_var_mapping <- function(df, name, value) {
    dplyr::select(df, all_of(c(value, name))) |>
        unique() |>
        dplyr::pull(.data[[name]], .data[[value]])
}
