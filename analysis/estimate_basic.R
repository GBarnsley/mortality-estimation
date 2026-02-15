# Estimate mortality

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(here)
library(vrcmort)

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
        sex = if_else(sex == "Male", 1, 2),
        cause = if_else(cause == "Trauma", 1, 2),
        post = date >= CONFLICT_START_DATE,
        conflict = if_else(post, num_events, 0),
        post = as.numeric(post),
        exposure = count,
        y = deaths,
        .keep = "none"
    ) |>
    select(-c(month, year)) |>
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
        conflict = ifelse(is.na(region), NA, unique(na.omit(conflict))),
        .by = c(time, region)
    ) |>
    filter(!is.na(age)) |>
    group_by(sex, region, age) |>
    fill(exposure, .direction = "up") |>
    ungroup() |> #dummy data for missing regions
    mutate(
        exposure = if_else(is.na(region), 1, exposure),
        conflict = if_else(is.na(region), 0, conflict)
    )

t0 <- filter(all_data, post == 1) |> pull(time) |> min()

### Fit Base Model

#drop deaths where no exposure (handle later)

dropped_deaths <- all_data |>
    filter(exposure == 0, y > 0) |>
    pull(y) |>
    sum()

message(
    "Dropping {.val {dropped_deaths}} deaths with zero exposure. ",
    "These will be handled in a later iteration."
)

all_data <- all_data |>
    mutate(
        exposure = if_else(exposure == 0, 1, exposure),
        y = if_else(y > exposure, exposure, y)
    )

fit0 <- vrcm(
    mortality = vrc_mortality(~1),
    reporting = vrc_reporting(~1),
    data = all_data,
    t0 = t0,
    chains = 2,
    cores = 2,
    iter = 1000,
    seed = 1
)

saveRDS(fit0, "data/derived-date/model_fit_basic.rds")

plot(fit0, type = "reporting")
plot(fit0, type = "mortality", value = "true_deaths")
