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
                 verbatimTextOutput("results_text"),
                 plotOutput("spread_vs_actual"),
                 plotOutput("time_graph")
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
    
    output$spread_vs_actual <- renderPlot({
      ggplot(data, aes(spread_home, actual_result_home)) +
        geom_point(alpha = .4) +
        geom_abline(slope=1, color= "blue")
      })
    
    output$results_text <- renderText({
      paste0(
      "Percent of games in which the home team covered the spread: ",
      round(filter(data, vs_line_home == "covered") |> nrow()*100 /nrow(data), 2), "%\n",
      "Percent of games in which the away team covered the spread: ",
      round(filter(data, vs_line_away == "covered") |> nrow()*100 /nrow(data), 2), "%\n",
      "Percent of games in where the teams pushed: ",
      round(filter(data, vs_line_home == "push") |> nrow()*100 /nrow(data), 2), "%"
      )
    })
    
    output$time_graph <- renderPlot({
      data |>
        mutate(month_day = format(date, "%m-%d")) |>
        ggplot(aes(x = month_day, y = actual_result_home, color = as.factor(season))) +
        geom_point(alpha = 0.6) +
        scale_x_discrete(
          labels = scales::date_format("%m-%d")
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

}
        
# Run the application 
shinyApp(ui = ui, server = server)
