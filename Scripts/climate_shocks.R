# Description
# Shock calculations - Xolani Sibande 18 March 2025

# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(broom)
library(glue)
library(scales)
library(kableExtra)
library(pins)
library(timetk)
library(uniqtag)
library(quantmod)

# graphs
library(PNWColors)
library(patchwork)

# eda
library(psych)
library(DataExplorer)
library(skimr)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)

options(scipen = 999)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
developed_markets_temp_tbl <- read_rds(here("Outputs", "artifacts_climate_data.rds")) |>
  pluck(1) |>
  dplyr::select(-year, -temp2)

developed_markets_precip_tbl <- read_rds(here("Outputs", "artifacts_climate_data.rds")) |>
  pluck(2) |>
  dplyr::select(-year, -precip2)

# Anomalies ---------------------------------------------------------------

## Rolling mean ---------------------------------------------------------
rolling_mean = slidify(
  .f = mean,
  .period = 365*10,
  .align = "right",
  .partial = FALSE
)

temp_rolling_mean_anomaly_tbl <-
  developed_markets_temp_tbl |>
  group_by(country) |>
  mutate(temp_mean = rolling_mean(temp)) |>
  mutate(anomaly = temp - temp_mean)

temp_rolling_mean_anomaly_tbl |>
  fx_plot(
    col = "country",
    value = "anomaly",
    facet_var = "country"
  ) +
  labs(title = "Temperature Anomalies",
       x = "Date",
       y = "Anomaly")

precip_rolling_mean_anomaly_tbl <-
  developed_markets_precip_tbl |>
  group_by(country) |>
  mutate(precip_mean = rolling_mean(precip)) |>
  mutate(anomaly = precip - precip_mean)

precip_rolling_mean_anomaly_tbl |>
  fx_plot(
    col = "country",
    value = "anomaly",
    facet_var = "country"
  ) +
  labs(title = "Precipitation Anomalies",
       x = "Date",
       y = "Anomaly")

## Using timetk anomalise function -------------------------------------
temp_anomalise_tbl <-
  developed_markets_temp_tbl |>
  group_by(country) |>
  anomalize(date, temp)

temp_anomalise_gg <-
  temp_anomalise_tbl |>
  # filter(anomaly == "Yes") |>
  fx_plot(
    date = "date",
    col = "country",
    value = "remainder",
    facet_var = "country"
  )


precip_anomalise_tbl <-
  developed_markets_precip_tbl |>
  group_by(country) |>
  anomalize(date, precip)

precip_anomalise_gg <-
  precip_anomalise_tbl |>
  # filter(anomaly == "Yes") |>
  fx_plot(
    date = "date",
    col = "country",
    value = "remainder",
    facet_var = "country"
  )


## Using extreme values for temperature -----------------------------------------------
country_extreme_function <- function(data, variable, extreme_level) {
  data |>
    group_by(country) |>
    mutate("{{variable}}_extreme_p_{extreme_level}" :=
             ifelse({{variable}} >= quantile({{variable}}, extreme_level), "Yes", "No")) |>
    mutate("{{variable}}_extreme_p_{1 - extreme_level}" :=
             ifelse({{variable}} <= quantile({{variable}}, 1 - extreme_level), "Yes", "No")) |>
    ungroup()
}

country_extreme_gg_function <- function(data, variable = "temp", extreme_variable) {
  data |>
    group_by(country) |>
    filter({{extreme_variable}} == "Yes") |>
    fx_plot(
      col = "country",
      value = variable,
      facet_var = "country"
    )
}


temp_extreme_tbl <-
  developed_markets_temp_tbl |>
  country_extreme_function(temp, 0.90) |>
  country_extreme_function(temp, 0.95) |>
  country_extreme_function(temp, 0.99)

temp_p90_gg <-
  temp_extreme_tbl |>
  country_extreme_gg_function(extreme_variable = temp_extreme_p_0.9)
temp_p10_gg <-
  temp_extreme_tbl |>
  country_extreme_gg_function(extreme_variable = temp_extreme_p_0.1)
temp_p95_gg <-
  temp_extreme_tbl |>
  country_extreme_gg_function(extreme_variable = temp_extreme_p_0.95)
temp_p5_gg <-
  temp_extreme_tbl |>
  country_extreme_gg_function(extreme_variable = temp_extreme_p_0.05)
temp_p99_gg <-
  temp_extreme_tbl |>
  country_extreme_gg_function(extreme_variable = temp_extreme_p_0.99)
temp_p1_gg <-
  temp_extreme_tbl |>
  country_extreme_gg_function(extreme_variable = temp_extreme_p_0.01)

temp_p90_gg/ temp_p10_gg
temp_p95_gg / temp_p5_gg
temp_p99_gg / temp_p1_gg

## Using extreme values for precipitation -----------------------------------------------
precip_extreme_tbl <-
  developed_markets_precip_tbl |>
  country_extreme_function(precip, 0.90) |>
  country_extreme_function(precip, 0.95) |>
  country_extreme_function(precip, 0.99)

precip_p90_gg <-
  precip_extreme_tbl |>
  country_extreme_gg_function(variable = "precip", extreme_variable = precip_extreme_p_0.9)
precip_p10_gg <-
  precip_extreme_tbl |>
  country_extreme_gg_function(variable = "precip", extreme_variable = precip_extreme_p_0.1)
precip_p95_gg <-
  precip_extreme_tbl |>
  country_extreme_gg_function(variable = "precip", extreme_variable = precip_extreme_p_0.95)
precip_p5_gg <-
  precip_extreme_tbl |>
  country_extreme_gg_function(variable = "precip", extreme_variable = precip_extreme_p_0.05)
precip_p99_gg <-
  precip_extreme_tbl |>
  country_extreme_gg_function(variable = "precip", extreme_variable = precip_extreme_p_0.99)
precip_p1_gg <-
  precip_extreme_tbl |>
  country_extreme_gg_function(variable = "precip", extreme_variable = precip_extreme_p_0.01)

precip_p90_gg/ precip_p10_gg
precip_p95_gg / precip_p5_gg
precip_p99_gg / precip_p1_gg

# Export ---------------------------------------------------------------
artifacts_climate_shocks <- list(
  data = list(
    temp_rolling_mean_anomaly_tbl = temp_rolling_mean_anomaly_tbl,
    precip_rolling_mean_anomaly_tbl = precip_rolling_mean_anomaly_tbl,
    temp_anomalise_tbl = temp_anomalise_tbl,
    precip_anomalise_tbl = precip_anomalise_tbl,
    temp_extreme_tbl = temp_extreme_tbl
  ),
  plots = list(
    temp_anomalise_gg = temp_anomalise_gg,
    precip_anomalise_gg = precip_anomalise_gg,
    temp_p90_gg = temp_p90_gg,
    temp_p10_gg = temp_p10_gg,
    temp_p95_gg = temp_p95_gg,
    temp_p5_gg = temp_p5_gg,
    temp_p99_gg = temp_p99_gg,
    temp_p1_gg = temp_p1_gg
    )
)

write_rds(artifacts_climate_shocks, file = here("Outputs", "artifacts_climate_shocks.rds"))


