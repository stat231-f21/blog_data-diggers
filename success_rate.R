# TODO: ADD "ALL" column to the table 
library(tidyverse)

kickstarter_data <- read.csv("kickstarter_2020.csv")
stateAbbrv <- read.csv("stateAbbrv.csv") %>% select(-Abbrev)

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

# create table for all states - for the SHINY TABLE
all_states <- kickstarter_data %>% 
  filter(country_corrected == "US") %>% 
  group_by(country_corrected, country_display_name_corrected ,main_category) %>% 
  summarize(successRate = sum(binarySF)/n(),
            observations = n())
all_states$state <- "All" #creates state column with "All" in each row
all_states <- all_states %>% relocate(state, .after = country_display_name_corrected)


# create a table for with an "all" category - for the SHINY MAP
all_categories <- kickstarter_data %>% 
  filter(country_corrected == "US") %>% 
  group_by(state) %>% 
  summarize(successRate = sum(binarySF)/n(),
            observations = n())
all_categories$main_category <- "All"
all_categories$country_corrected <- "US"
all_categories$country_display_name_corrected <- "United States" 
all_categories <- all_categories %>% relocate(c(country_code, country), .before = state)
all_categories <- all_categories %>% relocate(main_category, .after = state)
map_tbl <- rbind(all_categories, us_success_by_cat)

# clean the SHINY MAP data
map_tbl <- left_join(map_tbl, stateAbbrv, by = c("state" = "Code")) %>% 
  rename(state_code = state, state = State,
         country_code = country_corrected, country = country_display_name_corrected,
         success_rate = successRate) %>% 
  relocate(state, .after = state_code) %>% 
  relocate(c(country_code, country), .before = state_code)
# write shiny map data to a csv
#write.csv(map_tbl, "map_tbl.csv")



# For the SHINY TABLE
# row bind the all table 
us_success_by_cat <- rbind(all_states, us_success_by_cat)

# join in state names and clean - for the table with an "all" state
us_success_by_cat <- left_join(us_success_by_cat, stateAbbrv, by = c("state" = "Code")) %>% 
  rename(state_code = state, state = State,
         country_code = country_corrected, country = country_display_name_corrected,
         success_rate = successRate) %>% 
  relocate(state, .after = state_code)

# fix NAs in state column for All
us_success_by_cat <- us_success_by_cat %>% 
  mutate(state = ifelse(state_code == "All", "All", state))


## write csv
#write.csv(us_success_by_cat, "success_rate_tbl.csv")

## quick analysis examples
# tech <- us_success_by_cat %>% filter(main_category == "Technology") %>% arrange(desc(successRate))
# ca <- us_success_by_cat %>% filter(state == "CA") %>% arrange(desc(successRate))