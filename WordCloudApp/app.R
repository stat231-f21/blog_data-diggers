library(shiny)
library(tidyverse)
library(wordcloud)
library(reshape2) #for acast()
library(bslib)
#set seed
set.seed(189)

# import data
word_data <- read.csv("word_cloud_data.csv")

# get categories - type out b/c our data set is almost 1 million rows - more efficient
cat_choices <- c("Art", "Comics", "Crafts", "Dance", "Design", "Fashion", 
                 "Film & Video", "Food", "Games", "Journalism", "Music",
                 "Photography", "Publishing", "Technology", "Theater")


# Define UI for application that draws a histogram
ui <- fluidPage(
    # theme
    theme = bs_theme(bootswatch = "minty",
                     primary = "#05ce78",
                     secondary = "#05ce78"),

    # choice input selector
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "cat_choice",
                label = "Choose Main Category",
                multiple = FALSE,
                choices = cat_choices,
                selected = "Art")
        ),

        # Show the word cloud
        mainPanel(
           plotOutput("wordCloud")
        )
    )
)

# Define server logic required to draw a word cloud
server <- function(input, output) {
    output$wordCloud <- renderPlot({
        category_data <- word_data %>% 
            filter(main_category == input$cat_choice)
        category_data %>% 
            count(word, outcome, sort = T) %>% 
            acast(word ~outcome, value.var = "n", fill = 0) %>% 
            comparison.cloud(colors = c("#ed4752", "#05ce78"), max.words = 100,
                             title.colors = c("#ed4752", "#05ce78"), 
                             title.bg.colors=c("#3e4241","#3e4241"))
    }, height = 720, width = 550)
}

# Run the application 
shinyApp(ui = ui, server = server)
