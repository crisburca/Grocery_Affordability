#### Preamble ####
# Purpose: Test data to ensure cleaned data accuracy
# Author: Cristina Burca
# Date: 02 December 2024
# Contact: cristina.burca@mail.utoronto.ca
# Pre-requisites: Cleaned variables loaded from 03-clean_data.R


#### Workspace setup ####
library(tidyverse)
library(testthat)


#### Load Data ####
grocery_data <- read_csv("./data/02-analysis_data/grocery_data.csv")
inflation_data <- read_csv("./data/02-analysis_data/inflation_data.csv")
inflation_wage_data <- read_csv("./data/02-analysis_data/inflation_wage_data.csv")
old_inflation_wage_data <- read_csv("./data/02-analysis_data/old_inflation_wage_data.csv")
avg_wage_data <- read_csv("./data/02-analysis_data/avg_wage_data.csv")

#### Test data ####

## Test that `Year` and `Month` have correct number of columns
test_that("The column has 7 unique values", {
  expect_equal(length(unique(grocery_data$Wage)), 7)
})

test_that("The column has 7 unique values", {
  expect_equal(length(unique(grocery_data$Year)), 7)
})

test_that("The column has 7 unique values", {
  expect_equal(length(unique(grocery_data$Month)), 12)
})

test_that("The column has 7 unique values", {
  expect_equal(length(unique(inflation_data$Year)), 7)
})


## Testing classes
test_that("'products' is character", {
  expect_type(grocery_data$products, "character")
})

test_that("'CPI' is double", {
  expect_type(grocery_data$CPI, "double")
})

test_that("'CPI' is double", {
  expect_type(inflation_data$CPI, "double")
})

## Test that there are no missing values in the dataset
test_that("no missing values in dataset", {
  expect_true(all(!is.na(grocery_data)))
})

test_that("no missing values in dataset", {
  expect_true(all(!is.na(inflation_data)))
})

## Test Date format
test_that("The column is a Date in Y-M-D format", {
  expect_s3_class(grocery_data$Date, "Date")
})

