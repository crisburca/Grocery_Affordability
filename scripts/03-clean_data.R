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
library(janitor)


#### Clean data ####

# Remove unnecessary rows and format grocery data
colnames(grocery_data) <- grocery_data[1, ] 
grocery_data <- grocery_data[-c(1, 2), ]
colnames(grocery_data) <- gsub(" ", "_", colnames(grocery_data))
grocery_data <- grocery_data[-c(1), ]
grocery_data <- head(grocery_data, -17)

grocery_data <- grocery_data %>%
  clean_names()

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



# Same for old avg wage data
old_wage_data <- old_wage_data[1:24, ]
old_wage_data <- old_wage_data[, 1:2]
colnames(old_wage_data) <- c("Year", "Wage")

old_wage_data$Year <- as.numeric(old_wage_data$Year)
old_wage_data$Wage <- as.numeric(old_wage_data$Wage)


# Same for old inflation data
colnames(old_inflation_data) <- old_inflation_data[1, ] 
old_inflation_data <- old_inflation_data[-c(1,3), ]   
old_inflation_data <- old_inflation_data[-1, ]
old_inflation_data <- head(old_inflation_data, -5)
colnames(old_inflation_data) <- c("Date", "CPI")

old_inflation_data <- old_inflation_data %>%
  mutate(Year = year(dmy(paste("01", Date))),
         Month = month(dmy(paste("01", Date)))) 



# Extract month
grocery_data <- grocery_data %>%
  mutate(Year = year(dmy(paste("01", products))), 
         Month = month(dmy(paste("01", products))))

# Extract month and Year
inflation_data <- inflation_data %>%
  mutate(Year = year(dmy(paste("01", Date))),
         Month = month(dmy(paste("01", Date)))) 

# Merge CPI data with grocery data by Year and month
grocery_data <- grocery_data %>%
  left_join(inflation_data, by = c("Year", "Month"))

# Merge wage data by Year
avg_wage_data <- avg_wage_data %>%
  rename(Year = Date) 

grocery_data <- grocery_data %>%
  left_join(avg_wage_data, by = "Year")

grocery_data <- grocery_data %>%
  mutate(CPI = as.numeric(CPI))

# Extract reference CPI
ref_cpi <- grocery_data %>%
  filter(Date == "January 2017") %>%
  pull(CPI)

# Add CPI percentage column
grocery_data <- grocery_data %>%
  mutate(
    CPI_Percentage = (CPI - ref_cpi) / ref_cpi * 100)

# Create Date column for plotting
grocery_data <- grocery_data %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))


# Merging inflation and wage in separate variable
inflation_wage_data <- inflation_data %>%
  mutate(Wage = case_when(
    Year == 2017 ~ 26.82,
    Year == 2018 ~ 27.57,
    Year == 2019 ~ 28.32,
    Year == 2020 ~ 30.03,
    Year == 2021 ~ 30.67,
    Year == 2022 ~ 31.96,
    Year == 2023 ~ 33.55))

# Merging old inflation and wage in separate variable
old_inflation_wage_data <- old_inflation_data %>%
  mutate(Wage = case_when(
    Year == 2000 ~ 16.66,
    Year == 2001 ~ 17.22,
    Year == 2002 ~ 17.66,
    Year == 2003 ~ 18.05,
    Year == 2004 ~ 18.5,
    Year == 2005 ~ 19.09,
    Year == 2006 ~ 20.16,
    Year == 2007 ~ 20.99,
    Year == 2008 ~ 21.85,
    Year == 2009 ~ 22.63,
    Year == 2010 ~ 23.09,
    Year == 2011 ~ 23.6,
    Year == 2012 ~ 24.23,
    Year == 2013 ~ 24.75,
    Year == 2014 ~ 25.18,
    Year == 2015 ~ 25.88,
    Year == 2016 ~ 26.41,
    Year == 2017 ~ 26.82,
    Year == 2018 ~ 27.57,
    Year == 2019 ~ 28.32,
    Year == 2020 ~ 30.03,
    Year == 2021 ~ 30.67,
    Year == 2022 ~ 31.96,
    Year == 2023 ~ 33.55))

old_inflation_wage_data <- old_inflation_wage_data %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))

merged_data <- merge(old_wage_data, old_inflation_data, by = "Year", all = TRUE)

old_inflation_wage_data <- old_inflation_wage_data %>%
  mutate(CPI = as.numeric(CPI)) %>%
  mutate(CPI_Percentage = (CPI - lag(CPI)) / lag(CPI) * 100)

wage_changes <- old_inflation_wage_data %>%
  group_by(Year) %>%
  summarise(Wage = first(Wage), .groups = "drop") %>% 
  mutate(Wage_Percentage = (Wage - lag(Wage)) / lag(Wage) * 100) 

old_inflation_wage_data <- old_inflation_wage_data %>%
  mutate(Year = year(as.Date(Date))) %>% 
  left_join(wage_changes %>% select(Year, Wage_Percentage), by = "Year")


#### Save data ####
write_csv(grocery_data, "./data/02-analysis_data/grocery_data.csv")
write_csv(inflation_data, "./data/02-analysis_data/inflation_data.csv")
write_csv(avg_wage_data, "./data/02-analysis_data/avg_wage_data.csv")
write_csv(inflation_wage_data, "./data/02-analysis_data/inflation_wage_data.csv")
write_csv(old_inflation_wage_data, "./data/02-analysis_data/old_inflation_wage_data.csv")

arrow::write_parquet(preddata, "./data/02-analysis_data/cleaned.parquet")
