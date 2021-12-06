# Logit Model App

# libraries
library(shiny)
library(bslib)

# reproducible randomness
set.seed(123)


# I do some wrangling in the app to avoid committing another large csv file to the repo
# read in csv
kickstarter <- read.csv("../../kickstarter_2020.csv")

# cast categorical predictors into factors, mutate outcome into 0 = failed, 1 = successful
kickstarter_init <- kickstarter %>% 
    mutate(outcome = ifelse(outcome == "successful", 1, 0),
           main_category = as.factor(main_category),
           sub_category = as.factor(sub_category),
           city = as.factor(city),
           state = as.factor(state),
           country_corrected = as.factor(country_corrected),
           launched_at = date(launched_at))

# subset data to relevant variables
kickstarter_subset <- kickstarter_init %>% 
    select(backers_count, country_display_name_corrected, launched_at, spotlight, staff_pick, outcome, goal_usd, pledged_usd, campaign_length, main_category, sub_category, city, state, proportion_funded, name, blurb) %>% 
    mutate(launch_month = month(launched_at),
           launch_day = wday(launched_at),
           name_length = str_length(name),
           blurb_length = str_length(blurb))

## Train-Test Split
sample <- sample(c(TRUE, FALSE), nrow(kickstarter_subset), 
                 replace = TRUE, prob = c(0.7,0.3))
train <- kickstarter_subset[sample, ]
test <- kickstarter_subset[!sample, ]

train <- train %>% mutate(sqrt_goal_usd = sqrt(goal_usd))
test <- test %>% mutate(sqrt_goal_usd = sqrt(goal_usd))

# fit model
mod.final <- glm(outcome ~ sqrt_goal_usd + name_length + 
                     campaign_length + staff_pick + launch_day + 
                     main_category, data = train, family = "binomial")


# category choices
cat_choices <- train %>% distinct(main_category) %>% arrange(main_category)
cat_choices <- unique(cat_choices$main_category)

# Define UI for application
ui <- fluidPage(
    # theme
    theme = bs_theme(bootswatch = "minty",
                     primary = "#05ce78",
                     secondary = "#05ce78"),

    # Application title
    titlePanel("Kickstarter Success Prediction"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # category input
            selectizeInput(inputId = "input_cat",
                        label = "Category",
                        choices = cat_choices,
                        selected = "Art",
                        multiple = FALSE),
            # campaign length input
            sliderInput(inputId = "input_length",
                        label = "Campaign Length",
                        min = 1,
                        max = 60,
                        value = 30),
            # name input
            textInput(inputId = "input_name",
                      label = "Company Name",
                      value = "-Company Name-"),
            # goal input
            numericInput(inputId = "input_goal",
                         label = "Campaign Goal (USD)",
                         value = 5000),
            # launch weekday
            selectizeInput(inputId = "input_day",
                           label = "Launch Weekday",
                           choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                           selected = "Monday",
                           multiple = FALSE),
            # staff pick
            radioButtons(inputId = "input_staff_pick",
                         label = "Kickstarter Staff Pick",
                         choices = c("True", "False")),
            # button
            actionButton(inputId = "go", "Go")
        ),

        # Show text
        mainPanel(
           textOutput("text")
        )
    )
)

# Define server logic 
server <- function(input, output) {

    observeEvent(input$go, {
    # cast staff_pick_input to logical
    staff_pick_refined <- input$input_staff_pick 
    staff_pick_refined <- as.logical(input$input_staff_pick)
    
    # convert day name input to numeric
    launch_day_init <- input$input_day
    launch_day_refined <- case_when(launch_day_init == "Sunday" ~ 1,
                                    launch_day_init == "Monday" ~ 2,
                                    launch_day_init == "Tuesday" ~ 3,
                                    launch_day_init == "Wednesday" ~ 4,
                                    launch_day_init == "Thursday" ~ 5,
                                    launch_day_init == "Friday" ~ 6,
                                    launch_day_init == "Saturday" ~ 7,
                                            TRUE ~ 0)
    
    # make tibble with user parameters
    campaign_params <- tibble(main_category = input$input_cat, sqrt_goal_usd = sqrt(input$input_goal), 
                              staff_pick = staff_pick_refined, campaign_length = input$input_length, 
                              launch_day = launch_day_refined, name_length = str_length(input$input_name))
    # make prediction
    prediction <- predict(mod.final, campaign_params, type = "response")
    pred_pct <- round(prediction * 100, 2)
    # get goal
    goal_init <- input$input_goal
    goal_refined <- formatC(goal_init, format="d", big.mark=",")
    # make text for output
    final_text <- paste0("There is a ", pred_pct, "% chance that ", input$input_name, " will reach their funding goal of $", goal_refined)
    # print text
    output$text <- renderText({
        final_text
    })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
