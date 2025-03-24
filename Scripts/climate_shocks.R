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


## Using extreme values -----------------------------------------------
country_extreme_function <- function(data, variable, extreme_level) {
  data |>
    group_by(country) |>
    mutate("{{variable}}_extreme_p_{extreme_level}" :=
             ifelse(temp >= quantile({{variable}}, extreme_level), "Yes", "No")) |>
    mutate("{{variable}}_extreme_p_{1 - extreme_level}" :=
             ifelse(temp <= quantile({{variable}}, 1 - extreme_level), "Yes", "No")) |>
    ungroup()
}

country_extreme_gg_function <- function(data, extreme_variable) {
  data |>
    group_by(country) |>
    filter({{ extreme_variable }} == "Yes") |>
    fx_plot(
      col = "country",
      value = "temp",
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
  country_extreme_gg_function(temp_extreme_p_0.9)
temp_p10_gg <-
  temp_extreme_tbl |>
  country_extreme_gg_function(temp_extreme_p_0.1)
temp_p95_gg <-
  temp_extreme_tbl |>
  country_extreme_gg_function(temp_extreme_p_.095)
temp_p5_gg <-
  temp_extreme_tbl |>
  country_extreme_gg_function(temp_extreme_p_.05)
temp_p99_gg <-
  temp_extreme_tbl |>
  country_extreme_gg_function(temp_extreme_p_.099)
temp_p1_gg <-
  temp_extreme_tbl |>
  country_extreme_gg_function(temp_extreme_p_.01)

temp_p90_gg/ temp_p10_gg
temp_p95_gg / temp_p5_gg
temp_p99_gg / temp_p1_gg

# Export ---------------------------------------------------------------
artifacts_ <- list(

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


