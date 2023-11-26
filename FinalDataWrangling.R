# Aaron, Bella, Dagmawit
# Final Project -- Data Wrangling

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)

# Datasets
Agrofood_co2_emission_df <- read.csv("Agrofood_co2_emission.csv") 
crop_yield_changes_df <- read.csv("crops-yield-changes-CSV.csv") 

# Join by country name
combined_df <- left_join(Agrofood_co2_emission_df, crop_yield_changes_df, by = c("Area" = "BLS_2_Countries_.SRES._ABBREVNAME"))
# Agrofood_co2_emission_df has a ton of rows for one country (1990-2020), 
# so I think we need to create new rows based on combining/averaging/summarizing
# all of that together. 

combined_df %>%
  
  #drop_na deletes all objects including 'na' in only variables "area", "total_emission", "Forestland", and "ForestFires"
  drop_na(Area, total_emission, Forestland, Forest.fires) %>%
  
  #grouping by variable "area" so summarize function will calculate each country.
    group_by(Area) %>%
    summarise('Avg.Country.Emissions' = round(mean(total_emission), 3),
              'Avg.Forest.Land' = round(mean(Forestland), 3),
              'Avg.Forest.Fires' = round(mean(Forest.fires), 3)) %>%
    view()    

# AARON: Clean dataset and create the new categorical variable and new continuous/numerical variable
# BELLA: Join dataframes and create at least one summarization data frame
#DAGMAWIT: Data Nutrition Label
  
  
  