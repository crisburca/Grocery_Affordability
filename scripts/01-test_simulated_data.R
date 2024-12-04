#### Preamble ####
# Purpose: Test simulated data
# Author: Cristina Burca
# Date: 02 December 2024
# Contact: cristina.burca@mail.utoronto.ca
# Pre-requisites: 00-simulated_data.R


#### Workspace setup ####
library(tidyverse)

analysis_data <- read_csv("data/00-simulated_data/simulated_data.csv")

#### Test data ####
test_that("The column has 7 unique values", {
  expect_equal(length(unique(analysis_data$Year)), 7)
})

test_that("The column has 7 unique values", {
  expect_equal(length(unique(analysis_data$Month)), 12)
})

test_that("'CPI' is double", {
  expect_type(analysis_data$CPI, "double")
})

