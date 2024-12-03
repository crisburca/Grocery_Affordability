#### Preamble ####
# Purpose: Download and save data
# Author: Cristina Burca
# Date: 02 December 2024
# Contact: cristina.burca@mail.utoronto.ca
# Pre-requisites: -

#### Workspace setup ####
library(tidyverse)

#### Download data ####
grocery_data <- read.csv("./data/01-raw_data/grocery_prices.csv", skip = 7, header = TRUE)
inflation_data <- read.csv("./data/01-raw_data/cpi_inflation.csv",  skip = 7, header = TRUE)
avg_wage_data <- read.csv("./data/01-raw_data/wages_Year.csv", skip= 16, header = FALSE)
old_wage_data <- read.csv("./data/01-raw_data/old_wages_Year.csv", skip= 16, header = FALSE)
old_inflation_data <- read.csv("./data/01-raw_data/old_cpi_inflation.csv", skip= 7, header = TRUE)

         
