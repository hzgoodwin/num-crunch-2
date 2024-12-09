library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(thematic)
library(showtext)

# Business Logic

## Read Data, Clean Data



# Define UI for application that draws a histogram
ui <- fluidPage(

  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  titlePanel("Against the Spread"),
  tabsetPanel(
    tabPanel(
      "Introduction",
      textOutput("Add text that explains sports betting terms and the app's usage; see peer review of our group")
    ),
    tabPanel("Actual Spread v. Vegas Spread",
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
                 varSelectInput("ActualX_var", "For the Multiple Regression Plot, select the variable for the X axis", data = (NA)),
                 varSelectInput("ActualY_var", "For the Multiple Regression Plot, select the variable for the Y axis", data = (NA))
               ),
               mainPanel(
                 plotOutput("MLR_plot"),
                 verbatimTextOutput("lm_sum")
               )
             )
    ),
    tabPanel("Spread v. User Selected Variables",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("X_var", "Report Year of Interest:", 
                                    choices = names(dataset),
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
}

# Run the application 
shinyApp(ui = ui, server = server)
