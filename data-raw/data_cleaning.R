library(tidyverse)

# loading data
game_time <- read_csv("./data-raw/gametimedata.csv")
bye_week <- read_csv("./data-raw/bye_week.csv")
team_records <- read_csv("./data-raw/team_records.csv")

# tidying col names
names(game_time) <- game_time[1,]
game_time <- game_time[-1,]

# removing duplicates
game_time <- distinct(game_time)
bye_week <- distinct(bye_week)

# fixing types
game_time$`G#` <- as.numeric(game_time$`G#`)
game_time$Week <- as.numeric(game_time$Week)

# combining
temp <- game_time |>
  select(Team, Date, `G#`, Week, `Game time`) |>
  inner_join(bye_week, ., by = c("Team", "Date", "G#", "Week"))

# converting to date
temp$Date <- mdy(temp$Date)

# seasons
