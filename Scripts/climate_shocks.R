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
  .period = 120,
  .align = "right",
  .partial = TRUE
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
  filter(anomaly == "Yes") |>
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
  filter(anomaly == "Yes") |>
  fx_plot(
    date = "date",
    col = "country",
    value = "remainder",
    facet_var = "country"
  )


# Export ---------------------------------------------------------------
artifacts_ <- list(

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


