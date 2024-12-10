library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(thematic)
library(showtext)

# Business Logic

## Read Data, Clean Data
data <- read_csv("../data/data.csv")

teams <- c("--All--", str_sort(unique(data$home_team)))
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
                 
               # variable inputs
                 
               selectInput("home_team", "Select home team", choices = teams),
               selectInput("away_team", "Select away team", choices = teams),
               
               sliderInput("home_spread", "Home team spread", value = c(-20, 20), min = -20, max = 20),
               
               sliderInput("home_win_percentage", "Home team win percentage", value = c(0, 1), min = 0, max = 1),
               sliderInput("away_win_percentage", "Away team win percentage", value = c(0, 1), min = 0, max = 1),
               
               checkboxGroupInput("season", "Season",
                                  choices = c("2024 (thru 12/5)", "2023", "2022", "2021"),
                                  selected = c("2024 (thru 12/5)", "2023", "2022", "2021")),
               
               sliderInput("week", "Season week", value = c(1, 18), min = 1, max = 18),
               checkboxGroupInput("day", "Day of the week",
                                  choices = c("Mon", "Tue", "Thu", "Fri", "Sat", "Sun"),
                                  selected = c("Mon", "Tue", "Thu", "Fri", "Sat", "Sun")),
               checkboxGroupInput("game_time", "Game time",
                                  choices = c("early", "afternoon", "night"),
                                  selected = c("early", "afternoon", "night")),
               
               checkboxInput("divisional", "Divisional games only"),
               checkboxInput("after_bye_home", "Games with home team coming off bye only"),
               checkboxInput("after_bye_away", "Games with away team coming off bye only"),
               
               sliderInput("under_over", "Under/Over", value = c(30, 60), min = 30, max = 60),
               sliderInput("temperature", "Temperature (F)", value = c(0, 110), min = 0, max = 110)
       
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

    filtered_data <- reactive({
      filtered <- data
      
      if (input$home_team != "--All--"){
        filtered <- filtered |> filter(home_team == !!input$hometeam)
      }
      
      filtered
      
    })

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
      "Total games selected: ", nrow(filtered_data()), "\n",
      "Percent of games in which the home team covered the spread: ",
      round(filter(data, vs_line_home == "covered") |> nrow()*100 /nrow(data), 2), "%\n",
      "Percent of games in which the away team covered the spread: ",
      round(filter(data, vs_line_away == "covered") |> nrow()*100 /nrow(data), 2), "%\n",
      "Percent of games where the teams pushed: ",
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
