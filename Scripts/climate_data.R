# Description
# Importing daily climate data - 18 March 2025 Xolani Sibande

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
countries <- c("USA", "GBR", "DEU", "FRA", "CAN", "JPN", "CHE", "ITA")
developed_markets_temp_tbl <-
  read_rds(here("Data", "Temperature", "pop_weighted_temp_day.rds")) |>
  filter(country %in% countries)

developed_markets_precip_tbl <-
  read_rds(here("Data", "Precipitation", "pop_weighted_precip_day.rds")) |>
  filter(country %in% countries)

developed_markets_temp_tbl
developed_markets_precip_tbl

# EDA ---------------------------------------------------------------
developed_markets_temp_desc_tbl <-
  developed_markets_temp_tbl |>
  group_by(country) |>
  summarise(
    min_temp = min(temp),
    max_temp = max(temp),
    mean_temp = mean(temp),
    sd_temp = sd(temp),
    obs = n()
  ) |>
  kable() |>
  kable_styling()

developed_markets_precip_desc_tbl <-
  developed_markets_precip_tbl |>
  group_by(country) |>
  summarise(
    min_precip = min(precip),
    max_precip = max(precip),
    mean_precip = mean(precip),
    sd_precip = sd(precip),
    obs = n()
  ) |>
  kable() |>
  kable_styling()

# Graphing ---------------------------------------------------------------
developed_markets_temp_gg <-
  developed_markets_temp_tbl |>
  fx_plot(
    date = "date",
    col = "country",
    value = "temp",
    facet_var = "country"
  ) +
  labs(
    title = "Daily Temperature in Developed Markets",
    x = "Time",
    y = "Temperature (°C)",
    color = "Country"
  )

developed_markets_precip_gg <-
  developed_markets_precip_tbl |>
  fx_plot(
    date = "date",
    value = "precip",
    col = "country",
    facet_var = "country"
  ) +
  labs(
    title = "Daily Precipitation in Developed Markets",
    x = "Time",
    y = "Precipitation (mm)",
    color = "Country"
  )


# Export ---------------------------------------------------------------
artifacts_climate_data <- list (
  developed_markets_temp_tbl = developed_markets_temp_tbl,
  developed_markets_precip_tbl = developed_markets_precip_tbl,
  developed_markets_temp_desc_tbl = developed_markets_temp_desc_tbl,
  developed_markets_precip_desc_tbl = developed_markets_precip_desc_tbl,
  developed_markets_temp_gg = developed_markets_temp_gg,
  developed_markets_precip_gg = developed_markets_precip_gg
)

write_rds(artifacts_climate_data, file = here("Outputs", "artifacts_climate_data.rds"))


