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
      textOutput( "<h3>Against the Spread</h3>",
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
      )
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
    tabPanel(
      "Raw Data",
      checkboxInput("num_only", "Select only numerical variables?"),
      dataTableOutput("allData")
    )
  )
)

# Define server logic
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
