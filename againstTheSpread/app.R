library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(thematic)
library(showtext)

# Business Logic

## Read Data, Clean Data
data <- read_csv("../data/data.csv")
# cleaning done in final_cleaning.R file

## UI
ui <- fluidPage(

  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  titlePanel("Against the Spread"),
  tabsetPanel(
    tabPanel(
      "Introduction",
      textOutput("Add text that explains sports betting terms and the app's usage; see peer review of our group")
    ),
    
    tabPanel("Actual results v. Vegas Spread",
             sidebarLayout(
               sidebarPanel(
                 textOutput("This tab allows the user to filter by various characteristics to see how vegas spread compares to actual spread based on selected characteristics.")
                 
                 
                 # Add inputs for variables we choose
               ),
               mainPanel(
                 plotOutput("Vegas_plot"),
                 tableOutput("ttest_res")
               )
             )
    ),
    tabPanel("Spread v. User Selected Variables",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("X_var", "Report Year of Interest:", 
                                    choices = names(data),
                                    selected = "NA"),
                 varSelectInput("ActualY_var", "For the Multiple Regression Plot, select the variable for the Y axis", data = (NA))
               ),
               mainPanel(
                 plotOutput("MLR_plot"),
                 verbatimTextOutput("lm_sum")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {

    model_formula <- reactive({
      req(input$X_var)
      as.formula(paste0("spread" ~ paste(input$X_var, collapse = " + ")))
    })
    
    output$two_var_plot <- renderPlot({
    
}

# Run the application 
shinyApp(ui = ui, server = server)
