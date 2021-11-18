
## loading packages
library(tidyverse)
library(shiny)
library(plotly)
library(sf)
library(shinyWidgets)
library(bslib)

# import data
kickstarter <- read.csv("map_tbl.csv")

# remove the state "all"
kickstarter <- kickstarter %>% 
  filter(state != "All") %>% 
  mutate(success_rate = round(success_rate, 4))
  
us_map <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  st_as_sf()
state_info <- data.frame(ID = tolower(state.name),
                         state = state.name)

# cuts it down from 51 states to 48
new_map <- inner_join(kickstarter, state_info, by = "state") 
new_map <- inner_join(new_map, us_map, by = "ID")
m <- st_as_sf(new_map)

# Extract Unique Elements
cat_choices <- unique(m$main_category)

m <- m %>%
  mutate(`Success Rate` = success_rate)

title <- tags$a(href = "https://www.kickstarter.com/",
                tags$image(src = "kickstarter.jpg", height = '22', width = '200'))

############
#    ui    #
############

ui <- navbarPage(
  theme = bs_theme(bootswatch = "minty",
                   primary = "#05ce78",
                   secondary = "#05ce78"),
  tags$head(
    tags$style(HTML("
      .selectize-input {
        height: 20px;
        width: 200px;
        font-size: 15pt;
        padding-top: 5px;
      }"))
  ),
  
  tabPanel(
    title = title,
   sidebarLayout(  
    sidebarPanel(
      
      selectInput(
        inputId = "widget",
        label = "Choose Main Category",
        multiple = FALSE,
        choices = cat_choices,
        selected = "All"),
      ),
      
      mainPanel(
        plotlyOutput(outputId = "map", 
                     width = 650, height = 650))
    
   )
  )
)

server <- function(input, output) {
  output$map <- renderPlotly({
    new_map <- m %>%
      filter(main_category == input$widget) 
    
    ggplot(new_map, aes(text = paste("State:", state,
                                     "</br>Observations:", observations))) + 
      geom_sf(aes(fill = `Success Rate`)) +
      labs(title = paste("Success Rate for", input$widget, "Kickstarters\nin Each State"),
           fill = "Success Rate") +
      # make a plain theme
      theme(axis.line = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks = element_blank(),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            panel.background = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            plot.background = element_blank(), plot.title = element_text(size = 20,
                                                                         face = "bold",
                                                                         hjust = 0.5),
            legend.title = element_text(family = "Frankurter",
                                        size = 13,
                                        face = "bold")) +
      scale_fill_distiller(palette = "Greens", direction = 1)
    
  })
}


shinyApp(ui = ui, server = server)




