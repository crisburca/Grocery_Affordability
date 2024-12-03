#### Preamble ####
# Purpose: Save linear model
# Author: Cristina Burca
# Date: 02 December 2024
# Contact: cristina.burca@mail.utoronto.ca
# Pre-requisites: 02-download_data.R, 03-clean_data.R


#### Workspace setup ####
library(tidyverse)

#### Read data ####
grocery_data <- read_csv("./data/02-analysis_data/grocery_data.csv")


#### Format Data ####
grocery_data <- grocery_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         Average_Price = rowSums(select(., 
                                        ground_beef_per_kilogram_4, chicken_breasts_per_kilogram_4,
                                        butter_454_grams_4, milk_1_litre_4, yogurt_500_grams_5,
                                        block_cheese_500_grams_5, eggs_1_dozen_4, apples_per_kilogram_4,
                                        oranges_per_kilogram_4, bananas_per_kilogram_4, potatoes_4_54_kilograms_4,
                                        tomatoes_per_kilogram_4, carrots_1_36_kilograms_5, onions_per_kilogram_4,
                                        celery_unit_4, romaine_lettuce_unit_4, peppers_per_kilogram_4,
                                        frozen_mixed_vegetables_750_grams_5, frozen_pizza_390_grams_5,
                                        white_bread_675_grams_5, dry_or_fresh_pasta_500_grams_5,
                                        white_rice_2_kilograms_5, orange_juice_2_litres_5,
                                        roasted_or_ground_coffee_340_grams_5, olive_oil_1_litre_5,
                                        toothpaste_100_millilitres_5, laundry_detergent_4_43_litres_5), na.rm = TRUE))


grocery_data <- grocery_data %>%
  mutate(
    Time = as.numeric(Date - min(Date)),
    Affordability = Wage/Average_Price,
    CPI = as.numeric(CPI),
    Wage = as.numeric(Wage))

model_data <- grocery_data


### Model data ####
model_data <- model_data %>% filter(Time > 0)
elasticity_model <- lm(log(Affordability) ~ log(CPI) + log(Average_Price) + log(Time), data = model_data)


#### Save model ####
saveRDS(
  elasticity_model,
  file = "models/elasticity_model.rds")


