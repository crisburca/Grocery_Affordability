#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace setup ####
library(tidyverse)

#### Clean data ####

grocery_data <- read.csv("./data/01-raw_data/grocery_prices.csv", skip = 7, header = TRUE)
inflation_data <- read.csv("./data/01-raw_data/cpi_inflation.csv",  skip = 7, header = TRUE)
avg_wage_data <- read.csv("./data/01-raw_data/wages_year.csv", skip= 16, header = FALSE)




# Remove unnecessary rows and format grocery data
colnames(grocery_data) <- grocery_data[1, ] 
grocery_data <- grocery_data[-c(1, 2), ]
colnames(grocery_data) <- gsub(" ", "_", colnames(grocery_data))
grocery_data <- grocery_data[-c(1), ]
grocery_data <- head(grocery_data, -17)



# Remove unnecessary rows and format inflation data
colnames(inflation_data) <- inflation_data[1, ] 
inflation_data <- inflation_data[-c(1,3), ]   
inflation_data <- inflation_data[-1, ]
inflation_data <- head(inflation_data, -15)
colnames(inflation_data) <- c("Date", "CPI")



# Remove unnecessary rows and format avg wage data
avg_wage_data <- avg_wage_data[1:7, ]
avg_wage_data <- avg_wage_data[, 1:2]

colnames(avg_wage_data) <- c("Date", "Wage")

avg_wage_data$Date <- as.numeric(avg_wage_data$Date)
avg_wage_data$Wage <- as.numeric(avg_wage_data$Wage)




# Extract month
grocery_data <- grocery_data %>%
  mutate(year = year(dmy(paste("01", Products))), 
         month = month(dmy(paste("01", Products))))

# Extract month and year
inflation_data <- inflation_data %>%
  mutate(year = year(dmy(paste("01", Date))),
         month = month(dmy(paste("01", Date)))) 

# Merge CPI data with grocery data by year and month
merged_data <- grocery_data %>%
  left_join(inflation_data, by = c("year", "month"))

# Merge wage data by year
avg_wage_data <- avg_wage_data %>%
  rename(year = Date) # Rename to match `grocery_data`

final_data <- merged_data %>%
  left_join(avg_wage_data, by = "year")

# Create CPI percentage column
final_data <- final_data %>%
  mutate(
    CPI_Percentage = (CPI - CPI[Date == "January 2017"]) / CPI[Date == "January 2017"] * 100
  )

# Merging inflation and wage in separate variable
inflation_wage_data <- inflation_data %>%
  mutate(Wage = case_when(
    year == 2017 ~ 26.82,
    year == 2018 ~ 27.57,
    year == 2019 ~ 28.32,
    year == 2020 ~ 30.03,
    year == 2021 ~ 30.67,
    year == 2022 ~ 31.96,
    year == 2023 ~ 33.55
  ))


#### Save data ####
write_csv(final_data, "./data/02-analysis_data/grocery_data.csv")
write_csv(inflation_data, "./data/02-analysis_data/inflation_data.csv")
write_csv(avg_wage_data, "./data/02-analysis_data/avg_wage_data.csv")
write_csv(inflation_wage_data, "./data/02-analysis_data/inflation_wage_data.csv")
