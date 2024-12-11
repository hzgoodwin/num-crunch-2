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

data <- data |>
  mutate(total_points = str_extract_all(result_home, "\\d+") |>
           lapply(function (x) sum(as.numeric(x))) |> as.numeric())

# cleaning done in final_cleaning.R file

predInp <- function(pred) {
  numericInput(
    inputId = paste0("input_", pred),
    label = paste("Value for", pred),
    value = 1
  )
}


## UI
ui <- fluidPage(
  
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  titlePanel("Against the Spread"),
  tabsetPanel(
    tabPanel(
      "Introduction",
      HTML(
        "<h3>Against the Spread</h3>",
        "<p><b>Welcome to Against the Spread!</b> This app gives users the ability to filter NFL games starting from the 2021 season to help inform a decision on what team to bet on. A few things to know before getting into our app:</p>",
        
        "<ul>
          <li>This app is not a crystal ball or a future-telling machine, gambling is a risk. Never bet more than you can afford to lose. Never chase your losses. Gambling problem? Call 1-800-GAMBLER (CO, DC, IL, IN, LA, MD, MS, NJ, OH, PA, TN, VA, WV, WY), Call 877-8-HOPENY or text HOPENY (467369) (NY), Call 1-800-327-5050 (MA).</li>
          <li>This app is not providing any financial advice. This app organizes and filters historical gambling odds and final scores of completed NFL games. There is no guarantee trends continue, even with evidence to the contrary. Sports are unpredictable. Gamble responsibly.</li>
        </ul>",
        
        "<h4>Instructions</h4>",
        "<p>To begin, select the “Actual results v. Vegas Spread” tab in the above menu bar. Once selected, you will be greeted with a page divided in half. On the left side is a column with several variables and filters that can be edited, and on the right is a one-character-based and two-graphic-based output result. Let’s go through each variable and filter in the left-side column:</p>",
        
        "<ul>
          <li><b>Select Home Team & Select Away Team:</b> This filter allows the user to select their home of choice and their away team of choice. This filter can be used to view a team’s entire history of games at home or away if only one filter is used. Example: Every home game the Seahawks have played since 2021. This filter can also be used to see every matchup between two teams. Example: Every game the Seahawks have been the home team and the 49ers have been the away team.</li>
          <li><b>Home Team Spread:</b> This filter allows the user to give a range of searchable odds (-20 to 20) for the home team. Example: Every game where the home team is favored by between -5 and -10.</li>
          <li><b>Home and Away Team Winning Percentage:</b> These filters allow the user to select games played where the home or away team has a specific winning percentage. Example: Every game where the home team has a winning percentage of between .67 and 1. This filter can also be combined for home and away teams. Example: Every game played where the home team has a winning percentage of between .67 and 1 and the away team has a winning percentage of between 0 and .5.</li>
          <li><b>Season:</b> This variable allows the user to select one, multiple, or all seasons of which to pull games from. Example: Every game played in 2022 and 2023.</li>
          <li><b>Week of Season:</b> This variable allows the user to select a range of weeks or a single week where games were played. Example: Every game played between weeks 14 and 17.</li>
          <li><b>Day of Week:</b> This allows the user to select which day(s) of the week a game is played on. It also allows the user to exclude certain days. Example: Only show games played on Thursdays.</li>
          <li><b>Game Time:</b> This filter allows the user to select the time of day the game was played. Example: Every game played at night or every game played in the afternoon and night.</li>
          <li><b>Divisional Games Only:</b> This variable allows the user to toggle games where the home team is only playing other teams in their division. Example: Games where the Seahawks are the home team and are only playing teams in the NFC West.</li>
          <li><b>Home Team off Bye / Away Team off Bye:</b> This variable allows the user to toggle whether a team is coming off a bye week or not.</li>
          <li><b>Over/Under:</b> This filter allows the user to find games that had a certain level of predicted scoring. Example: Games where the over/under was between 40 and 50 points.</li>
          <li><b>Temperature:</b> This filter allows the user to select a range of temperatures and find games played within that range. Example: Every game played where the temperature was between 32- and 45-degrees Fahrenheit.</li>
        </ul>",
        
        "<p>These variables can be used on their own, but the power of this app is their use in combination to make a unique query for a game you are interested in betting on. For example, if you are interested in betting on Rams vs 49ers on Thursday, December 11th, 2024, you can search for similar games by inputting a home team spread of between -5 and 0, Thursday games only, at night, and divisional games only. You would find that 8 games meet similar criteria, and 62.5% of the time, the home team covered the spread.</p>"
      ),
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
                 plotlyOutput("ou_vs_actual")
               )
             )
    ),
    tabPanel("Spread v. User Selected Variables",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("X_var", "Report Year of Interest:", 
                                    choices = names(data)),
                 sliderInput("conflev", "Confidence Level", value = 0.95, min = 0, max = 1),
                 varSelectInput("plotpred", "Residual & Correlation Plot for Predictor", data = data),
                 uiOutput("predmeaninputs")
               ),
               mainPanel(
                 verbatimTextOutput("lm_sum"),
                 verbatimTextOutput("confint"),
                 plotOutput("resplotfit"),
                 plotOutput("qqplot"),
                 plotOutput("resplotpred"),
                 plotOutput("MLR_plot"),
                 plotOutput("timeplot"),
                 verbatimTextOutput("predmeanYOut")
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
  
  # correlation plot
  
  output$MLR_plot <- renderPlot({
    validate(
      need(length(input$X_var) > 0, message = FALSE)
    )
    ggplot(data = data, aes(x = !!(input$plotpred), y = spread_home)) +
      geom_point() +
      geom_smooth(method = lm, se = FALSE)
  })
  
  # time-correlation plot
  
  output$timeplot <- renderPlot({
    validate(
      need(length(input$X_var) > 0, message = FALSE)
    )
    ggplot(data = data, aes(x = date, y = resid(model()))) +
      geom_line() +
      geom_point()
  })
  
  # Predict mean of Y at single obs:
  
  output$predmeaninputs <- renderUI({
    map(input$X_var, predInp)
  })
  
  output$predmeanYOut <- renderPrint({
    validate(
      need(length(input$X_var) > 0, message = FALSE)
    )
    temp_df <- map(input$X_var, \(x) input[[paste0("input_", x)]]) |> 
      setNames(input$X_var) |> 
      data.frame()
    predict(model(), newdata = temp_df,
            interval = "confidence", level = input$conflev)
  })
  
  # tab 2-------------
  output$results_text <- renderText({
    paste0(
      "Total games selected: ", nrow(filtered_data()), "\n",
      "Percent of games in which the home team covered the spread: ",
      round(filter(filtered_data(), vs_line_home == "covered") |> nrow()*100 
            /nrow(filtered_data()), 2), "%\n",
      "Percent of games in which the away team covered the spread: ",
      round(filter(filtered_data(), vs_line_away == "covered") |> nrow()*100 
            /nrow(filtered_data()), 2), "%\n",
      "Percent of games where the teams pushed: ",
      round(filter(filtered_data(), vs_line_home == "push") |> nrow()*100 /nrow(filtered_data()), 2), "%"
    )
  })
  
  output$spread_vs_actual <- renderPlotly({
    p1 <- ggplot(filtered_data(), aes(actual_result_home, spread_home,
                                      text = paste0(
                                        season, " week ", week, "<br>",
                                        home_team, " ", str_extract(result_home, "\\d+-\\d+"),
                                        " ", away_team, "<br>",
                                        "Home team spread: ", spread_home, "<br>",
                                        "Home team result: ", actual_result_home))) +
      geom_point(alpha = .4) +
      geom_abline(slope=1, color= "blue") +
      annotate("text", x = 12, y = -30, label = "Away team covered", 
               hjust = 1, vjust = -1, size = 5, color = "blue", alpha = .5) +
      annotate("text", x = -15, y = 25, label = "Home team covered", 
               hjust = 0, vjust = 1, size = 5, color = "blue", alpha = 0.5)
    
    ggplotly(p1, tooltip = "text")
  })
  
  output$ou_vs_actual <- renderPlotly({
    p2 <- ggplot(filtered_data(), aes(total_points, over_under,
                                      text = paste0(
                                        season, " week ", week, "<br>",
                                        home_team, " ", str_extract(result_home, "\\d+-\\d+"),
                                        " ", away_team, "<br>",
                                        "Home team spread: ", spread_home, "<br>",
                                        "Home team result: ", actual_result_home))) +
      geom_point(alpha = .4) +
      geom_abline(slope=1, color= "blue") +
      annotate("text", x = 12, y = -30, label = "X", 
               hjust = 1, vjust = -1, size = 5, color = "blue", alpha = .5) +
      annotate("text", x = -15, y = 25, label = "Y", 
               hjust = 0, vjust = 1, size = 5, color = "blue", alpha = 0.5)
    
    ggplotly(p2, tooltip = "text")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
