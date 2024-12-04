#### Preamble ####
# Purpose: Save linear model .rds file
# Author: Cristina Burca
# Date: 02 December 2024
# Contact: cristina.burca@mail.utoronto.ca
# Pre-requisites: 02-download_data.R, 03-clean_data.R


#### Workspace setup ####
library(tidyverse)

#### Read data ####
grocery_data <- read_csv("./data/02-analysis_data/grocery_data.csv")


#### Format Data ####
model_data <- grocery_data


### Model data ####
model_data <- model_data %>% filter(Time > 0)
elasticity_model <- lm(log(Affordability) ~ log(CPI) + log(Average_Price) + log(Time), data = model_data)


#### Save model ####
saveRDS(
  elasticity_model,
  file = "models/elasticity_model.rds")


