library(shiny)
library(DT)

# DAN - import data
success_tbl <- read.csv("success_rate_tbl.csv")

# DAN - For interactive state table

state_choices <- unique(success_tbl$state)

############
#    ui    #
############
ui <- fluidPage(
    
    # Application title
    titlePanel("Success Rates by Category"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "state",
                           label = "State",
                           choices = state_choices,
                           selected = "All",
                           multiple = FALSE),
        ),
        
        # data table
        mainPanel(DT::dataTableOutput(outputId = "table")
        )
    )
)

############
#  server  #
############
# Define server logic required to make table
server <- function(input, output) {
    
    # data for the table
    data_for_table <- reactive({
        #store user inputs
        state_input <- input$state
        # get table data
        data <- success_tbl %>% 
            filter(state %in% state_input) %>%
            arrange(desc(success_rate)) %>% 
            select(-c(X, country_code, country, state_code, state)) %>% 
            rename(Category = main_category, 
                   "Success Rate" = success_rate,
                   Observations = observations)
    })
    
    # make the table output
    output$table <- DT::renderDataTable({
        datatable(data_for_table(), escape = FALSE, # escape = FALSE allows links to be clickable
                  extensions = c("Buttons"), 
                  options = list(dom = 'Blrtip',
                                 buttons = c('csv', 'excel', 'print'),
                                 pageLength = 5,
                                 lengthMenu = c(5, 10, 15)))
    })
}

####################
# call to shinyApp #
####################
# Run the application 
shinyApp(ui = ui, server = server)
