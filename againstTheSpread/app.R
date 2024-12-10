library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(thematic)
library(showtext)
library(plotly)

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
                                  choices = c("2024", "2023", "2022", "2021"),
                                  selected = c("2024", "2023", "2022", "2021")),
               
               sliderInput("week", "Week of the season", value = c(1, 18), min = 1, max = 18),
               checkboxGroupInput("day", "Day of the week",
                                  choices = c("Mon", "Tue", "Thu", "Fri", "Sat", "Sun"),
                                  selected = c("Mon", "Tue", "Thu", "Fri", "Sat", "Sun")),
               checkboxGroupInput("game_time", "Game time",
                                  choices = c("early", "afternoon", "night"),
                                  selected = c("early", "afternoon", "night")),
               
               checkboxInput("divisional", "Divisional games only"),
               checkboxInput("after_bye_home", "Games with home team coming off bye only"),
               checkboxInput("after_bye_away", "Games with away team coming off bye only"),
               
               sliderInput("over_under", "Over/Under", value = c(30, 60), min = 30, max = 60),
               sliderInput("temperature", "Temperature (F)", value = c(0, 110), min = 0, max = 110)
       
               ),
               mainPanel(
                 verbatimTextOutput("results_text"),
                 plotlyOutput("spread_vs_actual"),
                 plotlyOutput("time_graph")
               )
             )
    ),
    tabPanel("Spread v. User Selected Variables",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("X_var", "Report Year of Interest:", 
                                    choices = names(data)),
                 sliderInput("conflev", "Confidence Level", value = 0.95, min = 0, max = 1),
                 varSelectInput("plotpred", "Residual Plot for Predictor", data = data)
               ),
               mainPanel(
                 plotOutput("MLR_plot"),
                 verbatimTextOutput("lm_sum"),
                 verbatimTextOutput("confint"),
                 plotOutput("resplotfit"),
                 plotOutput("qqplot"),
                 plotOutput("resplotpred")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {

    filtered_data <- reactive({
      data |>
        filter(
          input$home_team == "--All--" | home_team == input$home_team,
          input$away_team == "--All--" | away_team == input$away_team,
          spread_home >= input$home_spread[1] & 
            spread_home <= input$home_spread[2],
          win_percentage_home >= input$home_win_percentage[1] & 
            win_percentage_home <= input$home_win_percentage[2],
          win_percentage_away >= input$away_win_percentage[1] & 
            win_percentage_away <= input$away_win_percentage[2],
          season %in% input$season,
          week >= input$week[1] & week <= input$week[2],
          day %in% input$day,
          game_time %in% input$game_time,
          input$divisional == FALSE | divisional == TRUE,
          input$after_bye_home == FALSE | after_bye_home == TRUE,
          input$after_bye_away == FALSE | after_bye_away == TRUE,
          over_under >= input$over_under[1] & over_under <= input$over_under[2],
          temperature >= input$temperature[1] & temperature <= input$temperature[2]
        )
    })
  

    model <- reactive({
      validate(
        need(length(input$X_var) > 0, message = FALSE)
      )
      lm(as.formula(paste0("spread_home ~ ", paste(input$X_var, collapse = "+"))),
         data = data)
    })
    
    output$lm_sum <- renderPrint({
      validate(
        need(length(input$X_var) > 0, "Select at least one predictor to display linear model")
      )
      print(summary(model()))
    })
    
    output$confint <- renderPrint({
      confint(model(), level = input$conflev)
    })
    
    output$resplotfit <- renderPlot({
      ggplot(mapping = aes(x = fitted(model()), y = resid(model()))) +
        geom_point() +
        geom_hline(yintercept = 0)
    })
    
    output$qqplot <- renderPlot({
      ggplot(mapping = aes(sample = resid(model()))) +
        geom_qq() +
        geom_qq_line()
    })
    
    observe({
      updateVarSelectInput(inputId = "plotpred", data = data |> dplyr::select(input$X_var))
    }) |> 
      bindEvent(model())
    
    output$resplotpred <- renderPlot({
      validate(
        need(length(input$X_var) > 0, message = FALSE)
      )
      ggplot(data = data, mapping = aes(x = !!(input$plotpred), y = resid(model()))) +
        geom_point() +
        geom_hline(yintercept = 0)
    })
    
    output$spread_vs_actual <- renderPlotly({
      p1 <- ggplot(filtered_data(), aes(spread_home, actual_result_home, 
                                  text = paste0(
                                    season, " week ", week, "<br>",
                                    home_team, " ", str_extract(result_home, "\\d+-\\d+"),
                                    " ", away_team, "<br>",
                                    "Home team spread: ", spread_home, "<br>",
                                    "Home team result: ", actual_result_home))) +
        geom_point(alpha = .4) +
        geom_abline(slope=1, color= "blue") +
        annotate("text", x = 12, y = -30, label = "Home team covered", 
                 hjust = 1, vjust = -1, size = 5, color = "blue", alpha = .5) +
        annotate("text", x = -15, y = 25, label = "Away team covered", 
                 hjust = 0, vjust = 1, size = 5, color = "blue", alpha = 0.5)
      
      ggplotly(p1, tooltip = "text")
      })
    
    output$results_text <- renderText({
      paste0(
      "Total games selected: ", nrow(filtered_data()), "\n",
      "Percent of games in which the home team covered the spread: ",
      round(filter(filtered_data(), vs_line_home == "covered") |> nrow()*100 /nrow(filtered_data()), 2), "%\n",
      "Percent of games in which the away team covered the spread: ",
      round(filter(filtered_data(), vs_line_away == "covered") |> nrow()*100 /nrow(filtered_data()), 2), "%\n",
      "Percent of games where the teams pushed: ",
      round(filter(filtered_data(), vs_line_home == "push") |> nrow()*100 /nrow(filtered_data()), 2), "%"
      )
    })
    
    output$time_graph <- renderPlotly({
      p2 <- filtered_data() |>
        ggplot(aes(x = date, y = actual_result_home, text = paste0(
          season, " week ", week, "<br>",
          home_team, " ", str_extract(result_home, "\\d+-\\d+"),
          " ", away_team, "<br>",
          "Home team spread: ", spread_home, "<br>",
          "Home team result: ", actual_result_home))) +
        geom_point(alpha = 0.6) +
        scale_x_date(
          breaks = "1 month",
          limits = range(filtered_data()$date)
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p2, tooltip = "text")
    })

}
        
# Run the application 
shinyApp(ui = ui, server = server)
