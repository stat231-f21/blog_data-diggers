
## loading packages
library(tidyverse)
library(shiny)
library(maps)
library(plotly)
library(leaflet)
library(sf)

# import data
kickstarter <- read.csv("map_tbl.csv")

# remove the state "all"
kickstarter <- kickstarter %>% 
  filter(state != "All")

us_map <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  st_as_sf()

data(state)
state_info <- data.frame(ID = tolower(state.name),
                         state = state.name)

# cuts it down from 51 states to 48
new_map <- inner_join(kickstarter, state_info, by = "state") 
new_map <- inner_join(new_map, us_map, by = "ID")
m <- st_as_sf(new_map)

############
#    ui    #
############

cat_choices <- unique(m$main_category)

ui <- navbarPage(
  title = "Kickstarter",
  
  tabPanel(
    
    title = "Chloropleth Map",
    
    sidebarPanel(
      
      selectInput(
        inputId = "widget",
        label = "Choose Main Category",
        multiple = FALSE,
        choices = cat_choices,
        selected = "All"
      ),
      
      mainPanel(
        plotOutput(outputId = "map") 
        
      ) 
    )
  )
)

server <- function(input, output) {
  output$map <- renderPlot({
    new_map <- m %>%
      filter(main_category == input$widget) 
    
    ggplot(new_map) + geom_sf(aes(fill = success_rate)) + 
      # make a plain theme
      theme(axis.line = element_blank(),axis.text.x = element_blank(),
            axis.text.y = element_blank(),axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_blank(),panel.border = element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor = element_blank(),plot.background = element_blank()) 
    
  })
  
}


shinyApp(ui = ui, server = server)




