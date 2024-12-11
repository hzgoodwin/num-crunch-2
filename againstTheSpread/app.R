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

discard_cols <- c("season", "date", "ou_result", "result_home",
                  "result_away", "vs_line_home", "vs_line_away",
                  "actual_result_home", "actual_result_away", "spread_away")

# cleaning done in final_cleaning.R file

# Function for updating model input UI
predInp <- function(pred) {
  if (is.numeric(data[[pred]])) {
    numericInput(
      inputId = paste0("input_", pred),
      label = paste0("Value for ", pred),
      value = 1)
  } else if (is.factor(data[[pred]])) {
    selectInput(
      inputId = paste0("input_", pred),
      label = paste0("Value for ", pred),
      choices = levels(data[[pred]])
    )
  }
}


## UI
ui <- fluidPage(
  
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
      "Introduction",
      source("../vignette/intropage.R", local = TRUE)$value
    ),
    
    tabPanel("Game Results",
             sidebarLayout(
               sidebarPanel(
                 
                 shinyjs::useShinyjs(),  # for reset button
                 id = "side-panel",
                 actionButton("reset_input", "Reset inputs"),
                 
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
                 
                 sliderInput("over_under", "Over/Under", value = c(30, 60), min = 30, max = 60),
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
    tabPanel("Spread v. User Selected Variables",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("X_var", "Variables of interest:", 
                                    choices = names(data)[!names(data) %in% discard_cols]),
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
    ),
    
    tabPanel("Raw data", DTOutput("table"))
    
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
  })
  
  filtered_data <- reactive({
    data |>
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
      geom_point() +
      facet_wrap(year(data$date), scales = "free_x")
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
    for (p in input$X_var) {
      if (is.factor(data[[p]])) {
        temp_df[[p]] <- factor(temp_df[[p]], levels = levels(data[[p]]))
      }
    }
    predict(model(), newdata = temp_df,
            interval = "confidence", level = input$conflev)
  })
  
  # tab 2-------------
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
  
  output$empty_error <- renderText({
    "WARNING: No games match your criteria. Try broadening your search."
  })
  
  output$filtered_data_nrow <- renderText({
    if (nrow(filtered_data()) == 0){
      return(TRUE)
    }
  })
  
  outputOptions(output, "filtered_data_nrow", suspendWhenHidden = FALSE)
  
  output$table <- renderDT({
      datatable(data, options = list(pageLength = 20, order = list(2, 'desc'))
                , filter = "top")
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
