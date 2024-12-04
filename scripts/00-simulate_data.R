#### Preamble ####
# Purpose: Test Simulate data
# Author: Cristina Burca
# Date: 02 December 2024
# Contact: cristina.burca@mail.utoronto.ca
# Pre-requisites: 00-simulate_data.R


#### Workspace setup ####
library(dplyr)
library(lubridate)

set.seed(123) 

#### Simulate Data ####

dates <- seq(as.Date("2017-01-01"), as.Date("2023-12-01"), by = "month")

n <- length(dates)

simulate_prices <- function(n, mean, sd) {
  rnorm(n, mean = mean, sd = sd)}

simulated_data <- data.frame(
  Date = dates,
  ground_beef_per_kilogram_4 = simulate_prices(n, mean = 10, sd = 2),
  chicken_breasts_per_kilogram_4 = simulate_prices(n, mean = 8, sd = 1.5),
  butter_454_grams_4 = simulate_prices(n, mean = 5, sd = 0.5),
  milk_1_litre_4 = simulate_prices(n, mean = 1.5, sd = 0.2),
  yogurt_500_grams_5 = simulate_prices(n, mean = 3, sd = 0.5),
  block_cheese_500_grams_5 = simulate_prices(n, mean = 6, sd = 0.8),
  eggs_1_dozen_4 = simulate_prices(n, mean = 3.5, sd = 0.3),
  apples_per_kilogram_4 = simulate_prices(n, mean = 2, sd = 0.3),
  oranges_per_kilogram_4 = simulate_prices(n, mean = 2.5, sd = 0.4),
  bananas_per_kilogram_4 = simulate_prices(n, mean = 1, sd = 0.2),
  potatoes_4_54_kilograms_4 = simulate_prices(n, mean = 5, sd = 0.7),
  tomatoes_per_kilogram_4 = simulate_prices(n, mean = 3, sd = 0.5))


simulated_data <- simulated_data %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Average_Price = rowSums(select(., ground_beef_per_kilogram_4, 
                                   chicken_breasts_per_kilogram_4,
                                   butter_454_grams_4, milk_1_litre_4, yogurt_500_grams_5,
                                   block_cheese_500_grams_5, eggs_1_dozen_4, apples_per_kilogram_4,
                                   oranges_per_kilogram_4, bananas_per_kilogram_4, potatoes_4_54_kilograms_4,
                                   tomatoes_per_kilogram_4)),
    CPI = 100 + cumsum(rnorm(n, mean = 0.5, sd = 0.2)),
    Wage = 500 + 5 * (1:n) + rnorm(n, mean = 0, sd = 20),
    Affordability = Wage / Average_Price)

simulated_data <- simulated_data %>%
  select(Year, Month, everything(), -Date)  

write_csv(simulated_data, "./data/00-simulated_data/simulated_data.csv")

