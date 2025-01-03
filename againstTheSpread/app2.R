library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(plotly)
library(shinyjs)

# Business Logic ----------------------

## Read Data, Clean Data
data <- read_rds("../data/data.rds")

teams <- c("--All--", str_sort(unique(data$home_team)))

data <- data |>
  mutate(total_points = str_extract_all(result_home, "\\d+") |>
           lapply( function(x) sum(as.numeric(x))) |> as.numeric())

data <- data |> mutate(week = as.numeric(week))

spread_model <- lm(actual_result_home ~ 
                             home_team + away_team + spread_home +
                             win_percentage_home + win_percentage_away +
                             week + day + game_time + divisional +
                             after_bye_home + after_bye_away + 
                             over_under + temperature, data = data)

over_under_model <- lm(total_points ~ 
                         home_team + away_team + spread_home +
                         win_percentage_home + win_percentage_away +
                         week + day + game_time + divisional +
                         after_bye_home + after_bye_away + 
                         over_under + temperature, data = data)

# cleaning done in final_cleaning.R file

## UI --------------------------
ui <- fluidPage(
  
  useShinyjs(),
  
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  titlePanel("Against the Spread"),
  
  tags$head(
    tags$style(HTML("
      h2 {
        font-size: 50px
      }
    "))
  ),
  
  tabsetPanel(
    tabPanel(
      "Home",
      source("./intropage.R", local = TRUE)$value
    ),
    tabPanel("Game Results",
             sidebarLayout(
               sidebarPanel(
                 id = "tab2_panel",
                 actionButton("reset_tab2", "Reset inputs"),
                 
                 # variable inputs
                 
                 selectInput("home_team", "Select home team", choices = teams),
                 selectInput("away_team", "Select away team", choices = teams),
                 selectInput("either_team", 
                             "Alternatively, select a team that can either be home or away", 
                             choices = teams),
                 
                 sliderInput("home_spread", "Home team spread", value = c(-20, 20), min = -20, max = 20, step = 0.5),
                 
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
                 
                 sliderInput("over_under", "Over/Under", value = c(30, 60), min = 30, max = 60, step = 0.5),
                 sliderInput("temperature", "Temperature (F)", value = c(0, 110), min = 0, max = 110)
                 
               ),
               mainPanel(
                 conditionalPanel(
                   "output.filtered_data_nrow", verbatimTextOutput("empty_error") 
                 ),
                 verbatimTextOutput("results_text"),
                 plotlyOutput("spread_vs_actual"),
                 tags$br(),
                 verbatimTextOutput("ou_text"),
                 plotlyOutput("ou_vs_actual"),
                 uiOutput("plot_info")
               )
             )
    ),
    
    tabPanel("Result Predictor", 
             sidebarLayout(
               sidebarPanel(
                 id = "tab3_panel",
                 fluidRow(
                   column(6, 
                          actionButton("reset_tab3", "Reset inputs")
                   ),
                   column(6, 
                          actionButton("build_model", "Calculate predictions", 
                                       style = "background-color: #6ccca9; position: absolute; 
                                       right: 20px; border-color: #6ccca9; color: white;")
                   )
                 ),
                 
                 # variables
                 
                 selectInput("home_team2", "Select home team", choices = str_sort(unique(data$home_team))),
                 selectInput("away_team2", "Select away team", choices = str_sort(unique(data$home_team))),
                 
                 sliderInput("home_spread2", "Home team spread", value = 0, min = -20, max = 20, step = 0.5),
                 
                 numericInput("home_win_percentage2", "Home team win percentage", value = 0.5, min = 0, max = 1, step = 0.01),
                 numericInput("away_win_percentage2", "Away team win percentage", value = 0.5, min = 0, max = 1, step = 0.01),
                 
                 sliderInput("week2", "Week of the season", value = 1, min = 1, max = 18),
                 selectInput("day2", "Day of the week",
                                    choices = c("Mon", "Tue", "Thu", "Fri", "Sat", "Sun"),
                                    selected = c("Sun")),
                 selectInput("game_time2", "Game time",
                                    choices = c("early", "afternoon", "night"),
                                    selected = c("early")),
                 
                 checkboxInput("divisional2", "Divisional game?"),
                 checkboxInput("after_bye_home2", "Home team coming off bye?"),
                 checkboxInput("after_bye_away2", "Away team coming off bye?"),
                 
                 sliderInput("over_under2", "Over/Under", value = 45, min = 30, max = 60, step = 0.5),
                 sliderInput("temperature2", "Temperature (F)", value = 55, min = 0, max = 110),
                 
                 actionButton("reset_all", "Hard reset")
                 
               ),
               
               mainPanel(
                 verbatimTextOutput("output_predictions")
               )
             )),
    
    tabPanel("Raw Data", DTOutput("table"))
    
  )
)


# Define server logic -------------------

# tab 2 ---------------------------
server <- function(input, output) {
  
  observeEvent(input$reset_tab2, {
    shinyjs::reset("tab2_panel")
  })
  
  filtered_data <- reactive({
    data |>
      mutate(week = as.numeric(week)) |>
      filter(
        input$home_team == "--All--" | home_team == input$home_team,
        input$away_team == "--All--" | away_team == input$away_team,
        input$either_team == "--All--" | 
          (home_team == input$either_team | away_team == input$either_team),
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
  
  output$games <- renderUI({
    paste0("Total games selected: ", nrow(filtered_data()))
  })
  
  output$results_text <- renderText({
    paste0(
      "Total games selected: ", nrow(filtered_data()), "\n",
      "Percent of games in which the home team covered the spread: ",
      round(filter(filtered_data(), vs_line_home == "covered") |> nrow()*100 
            /nrow(filtered_data()), 2), "%\n",
      "Percent of games in which the away team covered the spread: ",
      round(filter(filtered_data(), vs_line_away == "covered") |> nrow()*100 
            /nrow(filtered_data()), 2), "%\n",
      "Push: ",
      round(filter(filtered_data(), vs_line_home == "push") |> nrow()*100 /nrow(filtered_data()), 2), "%"
    )
  })
  
  output$ou_text <- renderText({
    paste0(
      "Percent of games in which the over hit: ", 
      round(filter(filtered_data(), ou_result == "over") |> 
              nrow()*100 /nrow(filtered_data()), 2), "%\n",
      "Percent of games in which the under hit: ",
      round(filter(filtered_data(), ou_result == "under") |> nrow()*100
            /nrow(filtered_data()), 2), "%\n", "Push: ",
      round(filter(filtered_data(), ou_result == "push") |> nrow()*100 
            /nrow(filtered_data()), 2), "%"
    )
  })
  
  output$plot_info <- renderUI( HTML(
    "<br>Note: Home teams are shown on the right in the plots' interactive tooltips. 
    The first plot shows the spread and result from the home team's perspective. 
    (Each game has two spreads: a negative spread for the favorite and a positive 
    spread for the underdog.) A negative result counterintuitively means that 
    the home team won.")
  )
  
  output$spread_vs_actual <- renderPlotly({
    p1 <- ggplot(filtered_data(), aes(actual_result_home, spread_home,
                                      text = paste0(
                                        season, " Week ", week, "<br>",
                                        away_team, " ", str_extract(result_away, "\\d+-\\d+"),
                                        " ", home_team, "<br>",
                                        "Home team spread: ", spread_home, "<br>",
                                        "Home team result: ", actual_result_home))) +
      geom_point(alpha = .4) +
      geom_abline(slope=1, color= "blue") +
      annotate("text", x = 20, y = -15, label = "Away team covered", 
               hjust = 1, vjust = -1, size = 5, color = "blue", alpha = .5) +
      annotate("text", x = -35, y = 12, label = "Home team covered", 
               hjust = 0, vjust = 1, size = 5, color = "blue", alpha = 0.5) +
      labs(title = "Spread vs Actual Result",
           x = "Actual result (home team)",
           y = "Vegas spread (home team)") +
      theme(plot.title=element_text(size=16,face="bold",color="gray30"),
            axis.title=element_text(size=14))
    
    ggplotly(p1, tooltip = "text")
  })
  
  output$ou_vs_actual <- renderPlotly({
    p2 <- ggplot(filtered_data(), aes(total_points, over_under,
                                      text = paste0(
                                        season, " Week ", week, "<br>",
                                        away_team, " ", str_extract(result_away, "\\d+-\\d+"),
                                        " ", home_team, "<br>",
                                        "O/U: ", over_under, "<br>",
                                        "Points scored: ", total_points))) +
      geom_point(alpha = .4) +
      geom_abline(slope=1, color= "blue") +
      annotate("text", x = 70, y = 35, label = "Over", 
               hjust = 1, vjust = -1, size = 5, color = "blue", alpha = .5) +
      annotate("text", x = 18, y = 55, label = "Under", 
               hjust = 0, vjust = 1, size = 5, color = "blue", alpha = 0.5) +
      labs(title = "Over/Under vs Actual Points Scored",
           x = "Actual points scored",
           y = "Vegas over/under") +
      theme(plot.title=element_text(size=16,face="bold", color="gray30"),
            axis.title=element_text(size=14))
    
    ggplotly(p2, tooltip = "text")
  })
  
  # checking to ensure there is at least 1 game
  
  output$empty_error <- renderText({
    "WARNING: No games match your criteria. Try broadening your search."
  })
  
  output$filtered_data_nrow <- renderText({
    if (nrow(filtered_data()) == 0){
      return(TRUE)
    }
  })
  
  outputOptions(output, "filtered_data_nrow", suspendWhenHidden = FALSE)
  
  
# tab 3 ---------------------------
  
  observeEvent(input$reset_all, {
    shinyjs::reset("tab3_panel")
    predictions$all_outputs <- list()
    output$output_predictions <- renderText({})
  })
  
  observeEvent(input$reset_tab3, {
    shinyjs::reset("tab3_panel")
  })
  
  predictions <- reactiveValues(all_outputs = list())
  
  observeEvent(input$build_model, {
    
    shinyjs::show("model")
    
    validate(need((isolate(input$home_win_percentage2) <= 1 &
                     isolate(input$home_win_percentage2) >= 0), 
                  "ERROR: Win percentages must be between 0 and 1"))
    validate(need((isolate(input$away_win_percentage2) <= 1 &
                     isolate(input$away_win_percentage2) >= 0), 
                  "ERROR: Win percentages must be between 0 and 1"))
    
    new_data <- data.frame(
      home_team = isolate(input$home_team2),
      away_team = isolate(input$away_team2),
      spread_home = isolate(input$home_spread2),
      win_percentage_home = isolate(input$home_win_percentage2),
      win_percentage_away = isolate(input$away_win_percentage2),
      week = isolate(input$week2),
      day = isolate(input$day2),
      game_time = isolate(input$game_time2),
      divisional = as.integer(isolate(input$divisional2)),
      after_bye_home = as.integer(isolate(input$after_bye_home2)),
      after_bye_away = as.integer(isolate(input$after_bye_away2)),
      over_under = isolate(input$over_under2),
      temperature = isolate(input$temperature2)
    )
    
    margin <- predict(spread_model, newdata = new_data)
    points <- predict(over_under_model, newdata = new_data)
    
    spread_diff <- input$home_spread2 - margin
    
    win_probability <- pt(margin / 12.32, 890, lower.tail = FALSE)
    percent_chance_spread <- pt(spread_diff / 12.32, 890)
    
    points_diff <- input$over_under2 - points
    percent_chance_over <- pt(points_diff / 12.98, 890, lower.tail = FALSE)
    
    home <- isolate(input$home_team2)
    away <- isolate(input$away_team2)
    
    new_output <- paste0(
      "Game: ", length(predictions$all_outputs) + 1, "\n",
      home, " win probability: ", round(win_probability * 100, 2), "%\n",
      away, " win probability: ", 100 - round(win_probability * 100, 2), "%\n",
      "\nHome team spread (", home, "): ", isolate(input$home_spread2), 
      "\nProjected result for ", home, ": ", round(margin, 2),
      "\nPercent chance ", home, " covers the spread: ", round(percent_chance_spread * 100, 2), "%",
      "\nPercent chance ", away, " covers the spread: ", 100 - round(percent_chance_spread * 100, 2), "%\n",
      "\nOver/under: ", isolate(input$over_under2),
      "\nProjected number of points scored: ", round(points, 2),
      "\nPercent chance the over hits: ", round(percent_chance_over * 100, 2), "%",
      "\nPercent chance the under hits: ", 100 - round(percent_chance_over * 100, 2), "%\n"
      )
    
    predictions$all_outputs <- c(new_output, predictions$all_outputs)
    
    output$output_predictions <- renderText({
      paste(predictions$all_outputs, collapse = 
      "\n----------------------------------------------\n")
    })
})
  
  
# tab 4 ---------------------------
  output$table <- renderDT({
    datatable(data, options = list(pageLength = 20, order = list(2, 'desc'))
              , filter = "top")
  })
}

# Run the application -------------------
shinyApp(ui = ui, server = server)
