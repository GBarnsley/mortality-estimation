# Estimate mortality
devtools::install_github(
    "ojwatson/vrcmort",
    ref = "feature/fast-vignette"
)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggh4x)
library(here)
library(vrcmort)

# Load helper functions
devtools::load_all(here::here())

#load data
all_data <- readRDS("data/derived-data/fitting_data.rds") |>
    filter_out(is.na(region))

t0 <- filter(all_data, post == 1) |> pull(time) |> min()

all_data <- all_data |>
    filter(cause == 2) |>
    mutate(cause = 1)

### Fit Base Model

fit0 <- vrcm(
    mortality = vrc_mortality(~1),
    reporting = vrc_reporting(~1),
    #use_mar_labels = TRUE,
    data = all_data,
    t0 = t0,
    chains = 4,
    cores = 4,
    iter = 1000,
    seed = 1,
    stan_model = "vr_reporting_model_rho1_pre"
)

saveRDS(fit0, "data/derived-data/model_fit_basic.rds")

fit0 <- readRDS("data/derived-data/model_fit_basic.rds")

var_mapping <- readRDS("data/derived-data/var_mapping.rds")

subtitle <- "Modelling excluding data missing labels,\nusing ACLED data to inform conflict intensity,\nassuming complete reporting pre-conflict"

#true deaths
df_mortality <- posterior_mortality(
    fit0,
    draws = FALSE
) |>
    select(sex, region, time, age, exposure, y, lambda_mean) |>
    mutate(
        sex = var_mapping$sex[sex],
        region = var_mapping$region[region],
        age = var_mapping$age[age],
        time = var_mapping$time[time]
    )

df_reporting <- posterior_reporting(
    fit0,
    draws = FALSE
) |>
    select(sex, region, time, age, exposure, rho_mean) |>
    mutate(
        sex = var_mapping$sex[sex],
        region = var_mapping$region[region],
        age = var_mapping$age[age],
        time = var_mapping$time[time]
    )

df_combined <- df_mortality |>
    left_join(
        df_reporting,
        by = c("sex", "region", "time", "age", "exposure")
    ) |>
    rename(y_obs = y) |>
    mutate(
        y_unobs = exposure * lambda_mean * (1 - rho_mean),
        y_true = y_obs + y_unobs,
        preconflict = time < CONFLICT_START_DATE
    )

df_age <- df_combined |>
    mutate(year = year(time)) |>
    summarise(
        y_obs = sum(y_obs),
        y_unobs = sum(y_unobs),
        y_true = sum(y_true),
        years = n_distinct(time) / 12,
        .by = c(age, year, preconflict)
    ) |>
    summarise(
        y_obs = sum(y_obs) / sum(years),
        y_unobs = sum(y_unobs) / sum(years),
        y_true = sum(y_true) / sum(years),
        .by = c(age, preconflict)
    )

p <- df_age |>
    select(-y_unobs) |>
    pivot_longer(
        c(y_obs, y_true),
        names_to = "type",
        values_to = "deaths"
    ) |>
    filter_out(type == "y_true", preconflict) |>
    mutate(
        type = recode(type, y_obs = "Observed", y_true = "Estimated"),
        preconflict = if_else(preconflict, "Pre-conflict", "Conflict") |>
            ordered(levels = c("Pre-conflict", "Conflict"))
    ) |>
    ggplot(aes(x = age, y = deaths, fill = type)) +
    geom_col(position = "dodge") +
    facet_wrap(~preconflict, nrow = 1) +
    theme_minimal() +
    labs(
        x = "Age group",
        fill = "Count type",
        y = "Average annual non-trauma deaths",
        subtitle = subtitle
    ) +
    theme(
        legend.position = "bottom"
    )

ggsave("figures/basic/deaths_by_age.png", p, width = 8, height = 4)

#by region

df_region <- df_combined |>
    mutate(year = year(time)) |>
    summarise(
        y_obs = sum(y_obs),
        y_unobs = sum(y_unobs),
        y_true = sum(y_true),
        years = n_distinct(time) / 12,
        .by = c(region, year, preconflict)
    ) |>
    summarise(
        y_obs = sum(y_obs) / sum(years),
        y_unobs = sum(y_unobs) / sum(years),
        y_true = sum(y_true) / sum(years),
        .by = c(region, preconflict)
    )

p <- df_region |>
    select(-y_unobs) |>
    pivot_longer(
        c(y_obs, y_true),
        names_to = "type",
        values_to = "deaths"
    ) |>
    filter_out(type == "y_true", preconflict) |>
    mutate(
        type = recode(type, y_obs = "Observed", y_true = "Estimated"),
        preconflict = if_else(preconflict, "Pre-conflict", "Conflict") |>
            ordered(levels = c("Pre-conflict", "Conflict"))
    ) |>
    ggplot(aes(x = region, y = deaths, fill = type)) +
    geom_col(position = "dodge") +
    facet_wrap(~preconflict, nrow = 1) +
    theme_minimal() +
    labs(
        x = "Governorate",
        fill = "Count type",
        y = "Average annual non-trauma deaths",
        subtitle = subtitle
    ) +
    theme(
        legend.position = "bottom"
    )

ggsave("figures/basic/deaths_by_region.png", p, width = 8, height = 4)

total_over_time <- df_combined |>
    summarise(
        y_obs = sum(y_obs),
        y_unobs = sum(y_unobs),
        y_true = sum(y_true),
        .by = c(time, preconflict)
    ) |>
    filter_out(preconflict)

p <- ggplot(total_over_time, aes(x = time)) +
    geom_line(aes(y = y_obs, color = "Observed")) +
    geom_line(aes(y = y_unobs, color = "Unobserved")) +
    theme_minimal() +
    labs(
        x = "Time",
        y = "Average monthly non-trauma deaths",
        color = "Count type",
        subtitle = subtitle
    ) +
    theme(
        legend.position = "bottom"
    )

ggsave("figures/basic/deaths_over_time.png", p, width = 8, height = 4)

#reporting summary

p <- df_combined |>
    filter_out(preconflict) |>
    select(region, time, rho_mean) |>
    unique() |>
    ggplot(aes(x = time, y = rho_mean)) +
    geom_line() +
    facet_wrap(vars(region)) +
    labs(x = "Time", y = "Estimated average reporting rate") +
    scale_y_continuous(labels = scales::percent_format())

ggsave("figures/basic/reporting_over_time.png", p, width = 8, height = 4)

#match to observed deaths

posterior_predictions <- posterior_predict(
    fit0
) |>
    mutate(
        sex = var_mapping$sex[sex],
        region = var_mapping$region[region],
        age = var_mapping$age[age],
        time = var_mapping$time[time]
    ) |>
    select(c(sex, region, time, age, y, yrep_q01, yrep_q05, yrep_q09)) |>
    mutate(
        preconflict = time < CONFLICT_START_DATE
    ) |>
    mutate(
        sex_age = paste(sex, age)
    )

p <- ggplot(posterior_predictions, aes(x = time)) +
    geom_ribbon(
        aes(ymin = yrep_q01, ymax = yrep_q09),
        fill = "blue",
        alpha = 0.2
    ) +
    geom_line(aes(y = yrep_q05, color = "Predicted"), linetype = "dashed") +
    geom_point(aes(y = y, color = "Observed")) +
    facet_grid2(sex_age ~ region, scales = "free_y", independent = "y") +
    labs(
        x = "Time",
        y = "Monthly non-trauma deaths",
        color = "",
        subtitle = subtitle
    ) +
    scale_color_manual(values = c("Observed" = "black", "Predicted" = "blue")) +
    theme_minimal() +
    theme(
        legend.position = "bottom"
    )

ggsave("figures/basic/predicted_vs_observed.png", p, width = 20, height = 20)
