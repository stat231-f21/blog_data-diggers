# Text Analysis Wrangling
# Dan and Karla
library(tidyverse)

# import data
kickstarter <- read.csv("kickstarter_2020.csv")

# get text data
text <- kickstarter %>%
  select(blurb, name, outcome, main_category)

# subset of text data
small_data <- kickstarter %>%
  select(blurb, name)

# remove stop words 
data(stop_words)
wordcloud_data <- text %>%
  unnest_tokens(output = word, input = blurb) %>%
  anti_join(stop_words, by = "word")

# join in words
new_text <- inner_join(wordcloud_data, small_data, by = "name")

# write to a csv
write_csv(new_text, "WordCloudApp/word_cloud_data.csv")