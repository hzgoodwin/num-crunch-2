point_difference <- function(result) {
  game <- regexec("(W|L) (\\d+)-(\\d+)", result)
  result_parts <- regmatches(result, game)[[1]]
  outcome <- result_parts[2]
  team_score <- as.numeric(result_parts[3])
  opponent_score <- as.numeric(result_parts[4])
  pointdiff <- team_score - opponent_score
  invertdiff <- -pointdiff
  return(invertdiff)
}
merged_data$difference <- sapply(merged_data$Result, point_difference)