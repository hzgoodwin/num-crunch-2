library(tidyverse)

# loading data
game_time <- read_csv("./data-raw/gametimedata.csv")
bye_week <- read_csv("./data-raw/bye_week.csv")
team_records <- read_csv("./data-raw/team_records.csv")

# tidying col names
names(game_time) <- game_time[1,]
game_time <- game_time[-1,]

# combining
temp <- game_time |>
  select(Team, Date, `Game time`) |>
  left_join(bye_week, ., by = c("Team", "Date"))