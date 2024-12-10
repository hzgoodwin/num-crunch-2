library(tidyverse)
library(janitor)

# loading data
game_time <- read_csv("./data-raw/gametimedata.csv")
bye_week <- read_csv("./data-raw/bye_week2.csv")
team_records <- read_csv("./data-raw/team_records.csv")

# tidying col names
names(game_time) <- game_time[1,]
game_time <- game_time[-1,]

# removing duplicates
game_time <- distinct(game_time)
bye_week <- distinct(bye_week)

# fixing types
game_time$Week <- as.numeric(game_time$Week)

# combining
temp <- game_time |>
  select(Team, Date, Week, `Game time`) |>
  inner_join(bye_week, by = c("Team", "Date", "Week"))

# converting to date
temp$Date <- mdy(temp$Date)

# seasons
temp <- temp |>
  mutate(Season = case_when(
    Date > ymd("2024-01-07") ~ "2024",
    Date %within% interval(ymd("2023-09-07"), ymd("2024-01-07")) ~ "2023",
    Date %within% interval(ymd("2022-09-08"), ymd("2023-01-08")) ~ "2022",
    Date %within% interval(ymd("2021-09-09"), ymd("2022-01-09")) ~ "2021",
    TRUE ~ NA
  )
) # not all seasons have the same number of games -- some seasons are missing a few
# games that did not have recorded temperatures

# merging
team_records <- team_records |> 
  rename(win_percentage = `W-L%...4`) |>
  select(Season, Team, win_percentage)

team_records$Season <- as.character(team_records$Season)

temp <- inner_join(team_records, temp, by = c("Season", "Team"))

# fixing home and away
temp <- temp |>
  rename(Location = `...9`)

temp$Location[is.na(temp$Location)] <- "home"
temp$Location[temp$Location == "@"] <- "away"

# divisional opponent
afc_west <- c("KAN", "LAC", "DEN", "LVR")
afc_east <- c("BUF", "MIA", "NYJ", "NWE")
afc_north <- c("PIT", "BAL", "CIN", "CLE")
afc_south <- c("HOU", "IND", "TEN", "JAX")
nfc_east <- c("DAL", "WAS", "NYG", "PHI")
nfc_north <- c("DET", "MIN", "GNB", "CHI")
nfc_south <- c("ATL", "TAM", "NOR", "CAR")
nfc_west <-c("SEA", "ARI", "LAR", "SFO")

temp <- temp |>
  mutate(divisional = case_when(
    (Team %in% afc_west & Opp %in% afc_west) ~ 1,
    (Team %in% afc_east & Opp %in% afc_east) ~ 1,
    (Team %in% afc_north & Opp %in% afc_north) ~ 1,
    (Team %in% afc_south & Opp %in% afc_south) ~ 1,
    (Team %in% nfc_east & Opp %in% nfc_east) ~ 1,
    (Team %in% nfc_north & Opp %in% nfc_north) ~ 1,
    (Team %in% nfc_south & Opp %in% nfc_south) ~ 1,
    (Team %in% nfc_west & Opp %in% nfc_west) ~ 1,
    TRUE ~ 0
  )
)

# removing extra columns
temp <- temp |> select(!(Rk|Spread...12))

# one observation per row
temp <- temp |>
  mutate(
    home_team = if_else(Location == "home", Team, Opp),
    away_team = if_else(Location == "away", Team, Opp)
  ) 

temp <- temp |> distinct() |> pivot_wider(
  id_cols = c(Season, Date, home_team, away_team, Temperature, divisional, Week, 
              `Game time`, `Over/Under`, Day, `OU Result`),
  names_from = Location,
  values_from = c(win_percentage, Spread...4, Result, `vs. Line`, after_bye))

# removing problematic rows
temp <- temp |> drop_na()

# tidying names
temp <- temp |> clean_names()

temp <- temp |>
  rename(spread_away = spread_4_away,
         spread_home = spread_4_home)

# adding result
temp |>
  mutate(actual_result_home = ,
         actual_result_away = )
