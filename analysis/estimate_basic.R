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
all_data <- readRDS("data/derived-data/fitting_data.rds")

t0 <- filter(all_data, post == 1) |> pull(time) |> min()

### Fit Base Model

#drop deaths where no exposure (handle later)

all_data <- all_data |>
    mutate(
        exposure = if_else(exposure == 0, 1, exposure),
        y = if_else(y > exposure, exposure, y)
    )

print(
    "Some data has no exposure but includes deaths, setting exposure to deaths"
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

saveRDS(fit0, "data/derived-data/model_fit_basic.rds")

plot(fit0, type = "reporting")
plot(fit0, type = "mortality", value = "true_deaths")
