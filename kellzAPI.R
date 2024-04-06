
# loading and reading ----
library(tidyverse)
#devtools::install_github("https://github.com/macraesdirtysocks/YFAR")
library(YFAR)

my_token <- y_create_token(my_key = 'dj0yJmk9R2VtbXNoaVNhVmFYJmQ9WVdrOWFYQmxaVE5oTUVNbWNHbzlNQT09JnM9Y29uc3VtZXJzZWNyZXQmc3Y9MCZ4PWVi',
                           my_secret = 'a3572a3b51372fe04052034779b8063f87b7b692',
                           app_name = 'fantasyBaseballYoruba')

my_token$can_refresh() # check if token is refreshable
my_token$refresh() # refresh token


# league information ----
games <- list()
games <- y_games(my_token)

# 422 is game key for baseball
# ryder's league key = '422.l.55262'
# kell'z league key = '422.l.78145'

# calling the fantasy weeks
weeks <- y_weeks(game_key = '422', token_name = my_token)

# getting team data for teams in the league
teams <- y_teams(league_key = '422.l.78145', token_name = my_token)
# i am team id 2 = 422.l.78145.t.2 , dave is 3

# getting team stats for a certain week ----
#week1 <- y_team_stats(key = '422.l.78145.t.2', my_token, week = '1') 

# all teams
week1 <- y_team_stats(key = teams$team_key, my_token, week = '1')

# all teams, all weeks
allWeeks <- y_team_stats(key = teams$team_key, my_token, week = c(1:13))



#allteams, all season
allSeason <- y_team_stats(key = teams$team_key, my_token)


stat_cats <- y_stats_categories(game_key = '422', my_token)

stat_cats2 <- stat_cats %>%
  select(stat_id, display_name)
# getting stats per team for week 1

week1Stats <- week1 %>%
  select(team_key, team_name, team_stats) %>%
  #filter(row_number() == 1) %>%
  unnest(team_stats)


week1Stats2 <- left_join(week1Stats, stat_cats2, by = 'stat_id') %>%
  filter(display_name != 'H/AB') %>%
  mutate(value = as.numeric(value)) %>%
  select(-stat_id) %>%
  spread(key = display_name, value = value) %>%
  mutate(Week = 'Week 1', .before = team_key) %>%
  relocate(IP,
           R, HR, RBI, SB, OPS,
           W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
  rename(SVH = `SV+H`)
# get averages, vstandar deviations!
# flip team name to columns maybe!!
# zscore is value - average / standard eeviation
# split z socre for hitting and pitching. then get the sum for total strength that week
# then rank that
# and then do z score for x number of weeks!!!

# see how many matchus i would win/lose per week!!!

week1Stats2 %>%
  mutate(ERA = (ERA - mean(ERA))/sd(ERA))

week1zScore <- week1Stats2 %>%
  select(1:3)

zScore <- function(statVar){
  
  week1zScore[[statVar]] <- (week1Stats2[[statVar]] - mean(week1Stats2[[statVar]])) / sd(week1Stats2[[statVar]])
  week1zScore

}

zScore("ERA")

# looping through list!!!1

statsList <- c('R', 'HR', 'RBI', 'SB', 'OPS',
               'W', 'K', 'ERA', 'WHIP', 'SVH')

for (c in statsList){
  week1zScore <- zScore(c)
}


rank(week1Rank[[ERA]])

# Creating the ranking categories ----
week1Rank <- week1Stats2 %>%
  select(1:3)

rankings <- function(statVar){
  week1Rank[[statVar]] <- rank(-week1Stats2[[statVar]])
  week1Rank
}

for (c in statsList){
  week1Rank <- rankings(c)
}



# doing all weeks a once

allWeekStats <- allWeeks %>%
  select(team_stats_week, team_key, team_name, team_stats) %>%
  unnest(team_stats)


allWeekStats2 <- left_join(allWeekStats, stat_cats2, by = 'stat_id') %>%
  filter(display_name != 'H/AB') %>%
  mutate(value = as.numeric(value)) %>%
  select(-stat_id)


allWeekStats3 <- allWeekStats2 %>%
  group_by(team_stats_week, display_name) %>%
  mutate(Rank = rank(-value),
         zScore = (value - mean(value))/sd(value))


# Creating the "results" table for a single week ----
actualValues <- allWeekStats3 %>%
  ungroup() %>%
  filter(team_stats_week == '13') %>%
  select(-c(Rank, zScore, team_stats_week, team_key )) %>%
  spread(key = display_name, value = value) %>%
  mutate(Week = 'Week 13', .before = team_name) %>%
  relocate(IP,
           R, HR, RBI, SB, OPS,
           W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
  rename(SVH = `SV+H`)

rankings <- allWeekStats3 %>%
  ungroup() %>%
  filter(team_stats_week == '13') %>%
  select(-c(value, zScore, team_stats_week, team_key )) %>%
  spread(key = display_name, value = Rank) %>%
  mutate(Week = 'Week 13', .before = team_name) %>%
  relocate(IP,
           R, HR, RBI, SB, OPS,
           W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
  rename(SVH = `SV+H`)

zScore <- allWeekStats3 %>%
  ungroup() %>%
  filter(team_stats_week == '13') %>%
  select(-c(Rank, value, team_stats_week, team_key )) %>%
  spread(key = display_name, value = zScore) %>%
  mutate(Week = 'Week 13', .before = team_name) %>%
  relocate(IP,
           R, HR, RBI, SB, OPS,
           W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
  rename(SVH = `SV+H`)

# z score sums
zScoreSumsCat <- allWeekStats3 %>%
  ungroup() %>%
  filter(team_stats_week == '13') %>%
  mutate(Week = 'Week 13') %>%
  group_by(Week, team_name, position_type) %>%
  summarise(zScoreSum = sum(zScore)) %>%
  spread(key = team_name, value = zScoreSum)


zScoreSumsCat2 <- allWeekStats3 %>%
  ungroup() %>%
  filter(team_stats_week == '13') %>%
  mutate(Week = 'Week 13') %>%
  group_by(Week, team_name) %>%
  summarise(zScoreSum = sum(zScore)) %>%
  spread(key = team_name, value = zScoreSum) %>%
  mutate(position_type = 'Total')

zScoreSumsCat3 <- rbind(zScoreSumsCat, zScoreSumsCat2)

# to do ----
# create loop for ranks xxxxxxxxxxxxxx

# pull all weeks, can I create rankings in a long dataset????


# what do i want?/
#data set for all the actuals
# data set for all the ranks
# dataset for all the z scores

# flip this into a long dataset and merge all weeks!!!
# can visualize the change over time!!!!!!!



# naming conventions for each week,


#


# draft results ----
draft <- y_draft_results(key = '422.l.55262', my_token)

draftADP <- y_draft_adp(key = '422.l.55262', my_token)
                    
# Player key.
# Last but not least probably the most succinct way to use `y_draft_adp()`, supply a vector of player keys. The player resource will contain the least amount of additional overhead information.
# 
# y_draft_adp(key = vector_of_player_keys, token_name = my_token