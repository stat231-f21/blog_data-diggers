# load packages
library(tidyverse)
library(shiny)
library(wordcloud)
library(tidytext)
library(tm)
library(bslib)

# import data
kickstarter <- read.csv("kickstarter_2020.csv")
# wrangle
text <- kickstarter %>%
  select(blurb, name, outcome, main_category)

small_data <- kickstarter %>%
  select(blurb, name)
# filter for stop words 
data(stop_words)
wordcloud_data <- text %>%
  unnest_tokens(output = word, input = blurb) %>%
  anti_join(stop_words, by = "word")

new_text <- inner_join(wordcloud_data, small_data, by = "name")

main_cat <- unique(new_text$main_category)

############
#    ui    #
############
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty",
                   primary = "#05ce78",
                   secondary = "#05ce78"), 
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId = "cloudchoice",
        label = "Choose a Category",
        choices = main_cat,
        multiple = FALSE, 
      ),
      
      
      mainPanel(
        plotOutput("wordcloud")
      )
    )
  )
)  
############
#  server  #
############
# Define server logic required to make table
server <- function(input, output) {
  
  
  
  
  
}
  


####################
# call to shinyApp #
####################
# Run the application 
shinyApp(ui = ui, server = server)



  
  
  
  
  