# Estimate mortality

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(here)
library(purrr)
library(forcats)
library(stringr)

# Load helper functions
devtools::load_all(here::here())

#load data
conflict <- readRDS("data/derived-data/conflict.rds")
mortality <- readRDS("data/derived-data/mortality.rds")
population <- readRDS("data/derived-data/population.rds")

#merge
all_data <- mortality |>
    filter(date >= ANALYSIS_START_DATE) |>
    filter(date <= max(population$date)) |>
    filter(sex != "Unknown") |>
    filter(!is.na(age_group)) |>
    complete(
        date,
        governorate,
        age_group,
        sex,
        cause,
        fill = list(deaths = 0)
    ) |>
    full_join(
        mutate(
            population,
            age_group = factor(age_group, levels = levels(mortality$age_group))
        ),
        by = join_by(date, age_group, sex, governorate),
        relationship = "many-to-one"
    ) |>
    left_join(
        select(conflict, -num_fatalities) |>
            filter(date <= max(population$date)),
        by = join_by(date, governorate),
        relationship = "many-to-one"
    ) |>
    filter(!is.na(cause), !is.na(sex), !is.na(age_group), !is.na(date)) |>
    mutate(
        region = if_else(
            governorate == "Unknown",
            NA,
            match(governorate, GAZA_GOVERNORATES)
        ),
        year = year(date) - year(ANALYSIS_START_DATE) + 1,
        month = month(date),
        time = ((year - 1) * 12) + month,
        age = match(age_group, levels(mortality$age_group)),
        Sex = sex,
        sex = if_else(sex == "Male", 1, 2),
        Cause = cause,
        cause = if_else(cause == "Trauma", 1, 2)
    ) |>
    select(-c(month, year))

var_mapping <- list(
    sex = "Sex",
    cause = "Cause",
    region = "governorate",
    age = "age_group",
    time = "date"
) |>
    purrr::imap(\(name, value) get_var_mapping(all_data, name, value))

all_data <- all_data |>
    mutate(
        region = region,
        time = time,
        age = age,
        sex = sex,
        cause = cause,
        post = date >= CONFLICT_START_DATE,
        conflict = if_else(post, num_events, 0),
        post = as.numeric(post),
        exposure = count,
        y = deaths,
        .keep = "none"
    ) |>
    arrange(
        time,
        region,
        sex,
        cause,
        age
    ) |>
    complete(
        sex,
        cause,
        region,
        time,
        age,
        fill = list(y = 0)
    ) |>
    mutate(
        post = unique(na.omit(post)),
        .by = c(time)
    ) |>
    mutate(
        conflict = ifelse(is.na(region), 0, unique(na.omit(conflict))),
        .by = c(time, region)
    ) |>
    filter(!is.na(age)) |>
    group_by(sex, region, age) |>
    fill(exposure, .direction = "up") |>
    ungroup() |> #dummy data for missing regions
    mutate(
        exposure = if_else(is.na(region), 1, exposure)
    )

t0 <- filter(all_data, post == 1) |> pull(time) |> min()

#regroup ages to 8 groups of equal size at conflict start
n_age <- 8
new_age_mapping <- all_data |>
    filter(time == t0 - 1, cause == 1) |>
    summarise(
        pop = sum(exposure),
        .by = age
    ) |>
    mutate(
        cum_pop = cumsum(pop),
        new_target_size = max(cum_pop) / n_age,
        new_age_group = round(cum_pop / new_target_size)
    ) |>
    pull(new_age_group, age)

all_data <- all_data |>
    mutate(
        age = new_age_mapping[age]
    ) |>
    summarise(
        exposure = sum(exposure),
        y = sum(y),
        .by = c(sex, cause, region, time, age, post, conflict)
    )

var_mapping$age <- tibble(
    name = var_mapping$age,
    new_age = new_age_mapping[names(var_mapping$age)]
) |>
    mutate(
        lower = str_split_i(name, "(\\-|\\+)", 1),
        upper = str_split_i(name, "(\\-|\\+)", 2),
        upper = if_else(upper == "", "+", upper)
    ) |>
    summarise(
        name = str_c(head(lower, 1), tail(upper, 1), sep = "-"),
        .by = new_age
    ) |>
    mutate(
        name = str_replace(name, "\\-\\+$", "+"),
        name = fct_relevel(ordered(name), name)
    ) |>
    pull(name, new_age)

#identical pop across cause
problematic <- all_data |>
    filter(n_distinct(exposure) > 1, .by = c(sex, region, time, age))

if (nrow(problematic) > 0) {
    warning(
        "There are some combinations of sex, region, time, and age where the exposure differs across causes."
    )
}

saveRDS(all_data, "data/derived-data/fitting_data.rds")

saveRDS(var_mapping, "data/derived-data/var_mapping.rds")
