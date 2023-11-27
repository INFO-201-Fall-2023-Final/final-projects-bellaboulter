# Aaron, Bella, Dagmawit
# Final Project -- Data Wrangling

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)

# LOAD DATASETS
Agrofood_co2_emission_df <- read.csv("Agrofood_co2_emission.csv") 
crop_yield_changes_df <- read.csv("crops-yield-changes-CSV.csv") 

# JOIN AND CLEAN
# Find the common countries between the data frames
common_countries <- intersect(Agrofood_co2_emission_df$Area, crop_yield_changes_df$BLS_2_Countries_.SRES._ABBREVNAME)

# Filter both data frames to include only common countries
Agrofood_co2_emission_df_filtered <- filter(Agrofood_co2_emission_df, Area %in% common_countries)
crop_yield_changes_df_filtered <- filter(crop_yield_changes_df, BLS_2_Countries_.SRES._ABBREVNAME %in% common_countries)

# Only keep columns of interest
crop_yield_changes_df_filtered <- select(crop_yield_changes_df_filtered, BLS_2_Countries_.SRES._ABBREVNAME, WH_2000, RI_2000, MZ_2000)

# Join the filtered data frames
combined_df <- left_join(Agrofood_co2_emission_df_filtered, crop_yield_changes_df_filtered, by = c("Area" = "BLS_2_Countries_.SRES._ABBREVNAME"))

# Remove this column with N/A values
combined_df <- subset(combined_df, select = -On.farm.energy.use)

# NEW CATEGORICAL COLUMN
# Whether or not a country's average household consumption is below or above average of all countries combined 
average_consumption <- summarise(combined_df, avg_consumption = mean(Food.Household.Consumption, na.rm = TRUE))
average_consumption <- pull(average_consumption)
combined_df <- mutate(combined_df, above_or_below_avg = ifelse(Food.Household.Consumption > average_consumption, "Above", "Below"))
  
# NEW NUMERICAL COLUMN
# There is data from 1990-2020 for each country. 
# This new column, total_emissions_over_thirty_years, is the sum of all 30 years of emmisions
temp_df <- group_by(combined_df, Area)
combined_df <- mutate(temp_df, total_emissions_over_thirty_years = sum(total_emission, na.rm = TRUE))

# SUMMARIZED DATA FRAME
temp_df <- group_by(combined_df, Area)
summarised_df <- summarise(temp_df, 
                           'Avg.Country.Emissions' = round(mean(total_emission), 3),
                           'Avg.Forest.Land' = round(mean(Forestland), 3),
                           'Avg.Forest.Fires' = round(mean(Forest.fires), 3))