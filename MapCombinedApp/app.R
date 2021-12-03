library(DT)
library(tidyverse)
library(miniUI)
library(bslib)
library(sf)
library(plotly)
library(ggiraph)

# Map Shiny Gadget - Dan and Karla

# TABLE
# TABLE - import data
success_tbl <- read.csv("success_rate_tbl.csv")
# TABLE - get user input
state_choices <- unique(success_tbl$state)


# MAP
# import data
kickstarter <- read.csv("map_tbl.csv")
# remove the state "all"
kickstarter <- kickstarter %>% 
    filter(state != "All") %>% 
    mutate(success_rate = round(success_rate, 4))
# make map an sf object
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
# fix name
m <- m %>%
    mutate(`Success Rate` = success_rate)
# title <- tags$a(href = "https://www.kickstarter.com/",
#                 tags$image(src = "kickstarter.jpg", height = '22', width = '200'))

############
#    ui    #
############
ui <- miniPage(
    theme = bs_theme(bootswatch = "minty",
                     primary = "#05ce78",
                     secondary = "#05ce78"),
    # Application title - don't include for the blog
    #gadgetTitleBar("Success Rates by State", left = NULL, right = NULL),
    
    # Sidebar with a slider input for number of bins 
    miniTabstripPanel(
        miniTabPanel("Table", icon = icon("table"),
                     sidebarPanel(
                         selectizeInput(inputId = "state",
                                        label = "State",
                                        choices = state_choices,
                                        selected = "All",
                                        multiple = FALSE),
                     ),
                     
                     # data table
                     miniContentPanel(DT::dataTableOutput(outputId = "table")
                     )
        ),
        
        # MAP
        miniTabPanel("Map", icon = icon("map-o"),
                     sidebarPanel(
                         selectInput(
                             inputId = "widget",
                             label = "Main Category",
                             multiple = FALSE,
                             choices = cat_choices,
                             selected = "All"),
                     ),
                     miniContentPanel(
                         girafeOutput(outputId = "map", 
                         ))
        )
    )
)


############
#  server  #
############
# Define server logic required to make table
server <- function(input, output) {
    
    # TABLE
    # data for the table
    data_for_table <- reactive({
        #store user inputs
        state_input <- input$state
        # get table data
        data <- success_tbl %>% 
            filter(state %in% state_input) %>%
            arrange(desc(success_rate)) %>% 
            mutate(success_rate = round(success_rate, digits = 4)) %>% 
            select(-c(X, country_code, country, state_code, state)) %>% 
            rename(Category = main_category, 
                   "Success Rate" = success_rate,
                   Observations = observations)
    })
    
    # make the table output
    output$table <- DT::renderDataTable({
        datatable(data_for_table(), escape = FALSE, # escape = FALSE allows links to be clickable
                  extensions = c("Buttons"), 
                  options = list(dom = 't',
                                 pageLength = 14))
    })
    
    # MAP
    output$map <- renderGirafe({
        new_map <- m %>%
            filter(main_category == input$widget) 
        # round success rate
        new_map$success_rate <- round(new_map$success_rate, 4)
        # make ggplot
        gg <- ggplot(new_map, aes(text = paste("State:", state,
                                               "</br>Observations:", observations))) + 
            geom_sf_interactive(aes(fill = success_rate, data_id = state, tooltip = paste("State: ", state, "</br>Success Rate: ", success_rate, "</br>Observations:", observations))) +
            ggtitle(paste("Success Rate for", input$widget, "Kickstarters\nin Each State")) +
            labs(fill = "Success Rate") +
            # make a plain theme
            theme(axis.line = element_blank(), axis.text.x = element_blank(),
                  axis.text.y = element_blank(), axis.ticks = element_blank(),
                  axis.title.x = element_blank(), axis.title.y = element_blank(),
                  panel.background = element_blank(), panel.border = element_blank(), 
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                  plot.background = element_blank(), plot.title = element_text(size = 15,
                                                                               face = "bold",
                                                                               hjust = 0.5),
                  legend.title = element_text(family = "Frankurter",
                                              size = 13,
                                              face = "bold")) +
            scale_fill_distiller(palette = "Greens", direction = 1)
        # make interactive
        interactive_gg <- girafe(ggobj = gg) 
        # add zooming functionality
        interactive_gg <- girafe_options(interactive_gg, opts_zoom(min = 1, max = 2.5))
    })
}

####################
# call to shinyApp #
####################
# Run the application 
shinyApp(ui, server)