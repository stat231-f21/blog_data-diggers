# TODO: ADD "ALL" column to the table 
library(tidyverse)

kickstarter_data <- read.csv("kickstarter_2020.csv")

# 1 if successful, 0 if failed
kickstarter_data <- kickstarter_data %>% mutate(binarySF = ifelse(outcome == "successful", 1, 0))

# showing that the original country column was messed up
kickstarter_data %>% distinct(country)
kickstarter_data %>% distinct(country_corrected)

# see success rates by category for all countries
world_success_by_cat <- kickstarter_data %>% 
  group_by(country_corrected, country_display_name_corrected, main_category) %>% 
  summarize(successRate = sum(binarySF)/n(),
            observations = n())
# see success rate by category for all states in the US
us_success_by_cat <- kickstarter_data %>% 
  filter(country_corrected == "US") %>% 
  group_by(country_corrected, country_display_name_corrected, state, main_category) %>% 
  summarize(successRate = sum(binarySF)/n(),
            observations = n())
us_success_by_cat

## write csv
# write.csv(us_success_by_cat, "success_rate_tbl.csv")

## quick analysis examples
# tech <- us_success_by_cat %>% filter(main_category == "Technology") %>% arrange(desc(successRate))
# ca <- us_success_by_cat %>% filter(state == "CA") %>% arrange(desc(successRate))