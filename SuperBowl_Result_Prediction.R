library(dplyr)
library(readxl)
library(tibble)
library(tidyr)
library(readxl)
full_year_conf_champ_game_stats <- read_excel("full year conf champ game stats.xlsx")
View(full_year_conf_champ_game_stats)
nfl <- full_year_conf_champ_game_stats
nfl
colnames(nfl)
colnames(nfl) <- gsub(" ", "_", colnames(nfl))
nfl
colnames(nfl)
#Get Eagles/Chiefs Stats
eagles_stats <- nfl %>% filter(nfl$Team_games == "Eagles")
chiefs_stats <- nfl %>% filter(nfl$Team_games == "Chiefs")

nfl <- nfl%>% left_join(avg_opponent_ranking, by = c("Team_games" = "Team_games"))
#Utilize average rankings based on strength of opponent
avg_opponent_ranking <- nfl %>% group_by(Team_games) %>% summarize(avg_opponet_ranking = mean(opponent_rank, na.rm = TRUE)
)
avg_opponent_ranking
#Add the adjusted ranking rows into the dataset
adjusted_ranks <- nfl %>% mutate(across(c(points_for, points_allowed, passing__yards, passing_tds, passing_ints, pass_yard_allowed, pass_td_allowed, rushing_yards, rushing_tds, rush_yard_allowed, rush_td_allowed), ~ . * avg_opponet_ranking, .names = "adjusted_{.col}"))
adjusted_ranks
view(adjusted_ranks)

#Find averages of the teams by their stats
team_avg <- rank_adjusted %>% group_by(Team_games) %>% summarise( average_adjusted_passing_yards = mean(adjusted_passing__yards, na.rm = TRUE) / 10, average_adjusted_passing_tds = mean(adjusted_passing_tds, na.rm = TRUE) / 10, average_adjusted_rushing_yards = mean(adjusted_rushing_yards, na.rm = TRUE) / 10, average_adjusted_rushing_tds = mean(adjusted_rushing_tds, na.rm = TRUE) / 10, average_adjusted_pass_yard_allowed = mean(adjusted_pass_yard_allowed, na.rm = TRUE) / 10, average_adjusted_pass_td_allowed = mean(adjusted_pass_td_allowed, na.rm = TRUE) / 10, average_adjusted_rush_yard_allowed = mean(adjusted_rush_yard_allowed, na.rm = TRUE) / 10, average_adjusted_rush_td_allowed = mean(adjusted_rush_td_allowed, na.rm = TRUE) / 10, average_adjusted_pass_ints = mean(adjusted_passing_ints / 10))

#create the model that will compare each team vs the other
model <- glm(adjusted_points_for ~ adjusted_passing__yards + adjusted_rushing_yards + adjusted_passing_tds + adjusted_rushing_tds - adjusted_passing_ints / (adjusted_pass_yard_allowed + adjusted_pass_td_allowed + adjusted_rush_yard_allowed + adjusted_rush_td_allowed), data = adjusted_ranks)
model
#Generate game data based on each stat category
game <- data.frame(
adjusted_passing__yards = team_avg$average_adjusted_passing_yards,
adjusted_passing_tds = team_avg$average_adjusted_passing_tds,
adjusted_rushing_yards = team_avg$average_adjusted_rushing_yards,
adjusted_rushing_tds = team_avg$average_adjusted_rushing_tds,
adjusted_pass_yard_allowed = team_avg$average_adjusted_pass_yard_allowed,
adjusted_pass_td_allowed = team_avg$average_adjusted_pass_td_allowed,
adjusted_rush_yard_allowed = team_avg$average_adjusted_rush_yard_allowed,
adjusted_rush_td_allowed = team_avg$average_adjusted_rush_td_allowed, 
adjusted_passing_ints = team_avg$average_adjusted_pass_ints
)
view(game)
game <- game %>% rename(adjusted_pass_ints = adjusted_passing_ints)
#Find the game stats for each team specifically
game_eagles <- subset(game1, Teams == "Eagles")
game_eagles
game_chiefs <- subset(game1, Teams == "Chiefs")
game_chiefs
#Generate a prediction using the predict() function
predict_eagles <- predict(model, newdata = game_eagles )
predict_chiefs <- predict(model, newdata = game_chiefs)


predict_eagles
predict_chiefs

#Due to the data not being structured for NFL games we want to scale the scoring differential 
raw <- predict_eagles - predict_chiefs
raw
normal <- raw / mean(c(predict_chiefs, predict_eagles)) * 35
normal

print(paste("The Eagles beat the Chiefs by", round(normal, 0)))

#Output 
"The Eagles beat the Chiefs by 2"
