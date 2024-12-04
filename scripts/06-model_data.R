#### Preamble ####
# Purpose: Save bayesian model
# Author: Cristina Burca
# Date: 02 December 2024
# Contact: cristina.burca@mail.utoronto.ca
# Pre-requisites: 02-download_data.R, 03-clean_data.R


#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
grocery_data <- read_csv("./data/02-analysis_data/grocery_data.csv")


#### Format Data ####
model_data <- grocery_data

### Model data ####
priors <- normal(0, 2.5, autoscale = TRUE)

bay_model <- stan_glm(
  formula = model,
  data = model_data,
  family = gaussian(),
  prior = priors,
  prior_intercept = priors,
  seed = 123)


#### Save model ####
saveRDS(
  bay_model,
  file = "models/bay_model.rds")


