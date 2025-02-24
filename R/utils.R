###########################################################################
# Script name: Utilities
#
# Purpose: Contains custom functions and data processing code
#
# Author: Clare Gibson
# 
# Date created: 2025-02-24
# 
# Copyright (c) Clare Gibson, 2025
# Email: clare@datatranslator.co.uk
###########################################################################

# Load packages -----------------------------------------------------------
library(here)
library(tidyverse)
library(janitor)

# Read data ---------------------------------------------------------------
df <- read_csv(
  file = here("data/src/all_perth_310121.csv"),
  col_types = "ccnnnnnnnncnccnncnn"
)

# Process data ------------------------------------------------------------
house_prices <- 
  df |> 
  # clean column headings
  clean_names()

# Feature engineering -----------------------------------------------------
house_prices <- 
  house_prices |> 
  mutate(
    rounded_price = round(price/1000),
    sale_year = as.numeric(str_sub(date_sold, -4)),
    age = sale_year - build_year
  )

# Model 1 -----------------------------------------------------------------
# In this model we use 3 features to predict house price: 
# size (floor area), age and number of bedrooms

model_1 <- 
  house_prices |> 
  # select and rename the required columns
  select(
    price = rounded_price,
    size = floor_area,
    bedrooms,
    age
  ) |> 
  # remove any examples where age is negative (build year > sale year
  # doesn't make sense)
  filter(age >= 0) |> 
  # discard NA values
  na.omit()

write_csv(model_1, here("data/cln/model_1.csv"))