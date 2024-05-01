

# loading and reading ----
library(tidyverse)
#devtools::install_github("https://github.com/macraesdirtysocks/YFAR")
library(YFAR)

# Creating Connection to Yahoo ----
my_token <- y_create_token(my_key = 'dj0yJmk9R2VtbXNoaVNhVmFYJmQ9WVdrOWFYQmxaVE5oTUVNbWNHbzlNQT09JnM9Y29uc3VtZXJzZWNyZXQmc3Y9MCZ4PWVi',
                           my_secret = 'a3572a3b51372fe04052034779b8063f87b7b692',
                           app_name = 'fantasyBaseballYoruba')

my_token$can_refresh() # check if token is refreshable
my_token$refresh() # refresh token

# league information ----
games <- list()
games <- y_games(my_token)


# Pulling stats from Yahoo ----
teams <- y_teams(league_key = '431.l.93970', token_name = my_token)
allWeeks <- y_team_stats(key = teams$team_key, my_token, week = c(1:24))

stat_cats <- y_stats_categories(game_key = '431', my_token) %>%
  select(stat_id, display_name, position_type)
 
allWeekStats <- allWeeks %>%
  filter(team_remaining_games == 0) %>% # only getting info for completed weeks
  select(team_stats_week, team_key, team_name, team_stats) %>%
  unnest(team_stats)

allWeekStats2 <- left_join(allWeekStats, stat_cats, by = 'stat_id') %>%
  filter(display_name != 'H/AB') %>%
  mutate(value = as.numeric(value)) %>%
  select(-stat_id)

allWeekStats3 <- allWeekStats2 %>%
  group_by(team_stats_week, display_name) %>%
  mutate(Rank = rank(-value),
         zScore = ifelse(display_name %in% c('ERA', 'WHIP'),
                         -(value - mean(value))/sd(value),
                         (value - mean(value))/sd(value)))

# Creating the "results" table for a single week, templates for functions ----
# actualValues <- allWeekStats3 %>%
#   ungroup() %>%
#   filter(team_stats_week == '13') %>%
#   select(-c(Rank, zScore, team_stats_week, team_key )) %>%
#   spread(key = display_name, value = value) %>%
#   mutate(Week = 'Week 13', .before = team_name) %>%
#   relocate(IP,
#            R, HR, RBI, SB, OPS,
#            W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
#   rename(SVH = `SV+H`)
# 
# rankings <- allWeekStats3 %>%
#   ungroup() %>%
#   filter(team_stats_week == '13') %>%
#   select(-c(value, zScore, team_stats_week, team_key, position_type)) %>%
#   spread(key = display_name, value = Rank) %>%
#   mutate(Week = 'Week 13', .before = team_name) %>%
#   relocate(IP,
#            R, HR, RBI, SB, OPS,
#            W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
#   rename(SVH = `SV+H`)
# 
# zScore <- allWeekStats3 %>%
#   ungroup() %>%
#   filter(team_stats_week == '13') %>%
#   select(-c(Rank, value, team_stats_week, team_key )) %>%
#   spread(key = display_name, value = zScore) %>%
#   mutate(Week = 'Week 13', .before = team_name) %>%
#   relocate(IP,
#            R, HR, RBI, SB, OPS,
#            W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
#   rename(SVH = `SV+H`)
# 
# # z score sums
# zScoreSumsCat <- allWeekStats3 %>%
#   ungroup() %>%
#   filter(team_stats_week == '13') %>%
#   mutate(Week = 'Week 13') %>%
#   group_by(Week, team_name, position_type) %>%
#   summarise(zScoreSum = sum(zScore)) %>%
#     spread(key = team_name, value = zScoreSum)
#   
# 
# zScoreSumsCat2 <- allWeekStats3 %>%
#   ungroup() %>%
#   filter(team_stats_week == '13') %>%
#   mutate(Week = 'Week 13') %>%
#   group_by(Week, team_name) %>%
#   summarise(zScoreSum = sum(zScore)) %>%
#   spread(key = team_name, value = zScoreSum) %>%
#     mutate(position_type = 'Total')
# 
# zScoreSumsCat3 <- rbind(zScoreSumsCat, zScoreSumsCat2)


# creating function for a results table! ----

actualValuesWeek <- function(weekNum){
  actualValues <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum) %>%
    select(-c(Rank, zScore, team_stats_week, team_key, position_type)) %>%
    spread(key = display_name, value = value) %>%
    mutate(Week = paste('Week', weekNum), .before = team_name) %>%
    relocate(IP,
             R, HR, RBI, SB, OPS,
             W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
    rename(SVH = `SV+H`)
  
  actualValues
  
}

rankingsWeek <- function(weekNum){
  
  rankings <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum) %>%
    select(-c(value, zScore, team_stats_week, team_key, position_type)) %>%
    spread(key = display_name, value = Rank) %>%
    mutate(Week = paste('Week', weekNum), .before = team_name) %>%
    relocate(IP,
             R, HR, RBI, SB, OPS,
             W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
    rename(SVH = `SV+H`) 
  
  rankings
}
  
 
zScoreWeek <- function(weekNum){

  zScore <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum) %>%
    select(-c(Rank, value, team_stats_week, team_key, position_type)) %>%
    spread(key = display_name, value = zScore) %>%
    mutate(Week = paste('Week', weekNum), .before = team_name) %>%
    relocate(IP,
             R, HR, RBI, SB, OPS,
             W, K, ERA, WHIP, `SV+H`, .after= team_name) %>%
    rename(SVH = `SV+H`)

  zScore
}

zScoreSumsWeek <- function(weekNum){
  
  zScoreSumsCat <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum) %>%
    mutate(Week = paste('Week', weekNum),
           weekNumber = weekNum) %>%
    group_by(Week, weekNumber, team_name, position_type) %>%
    summarise(zScoreSum = sum(zScore)) %>%
      spread(key = team_name, value = zScoreSum)
  
  
  zScoreSumsCat2 <- allWeekStats3 %>%
    ungroup() %>%
    filter(team_stats_week == weekNum) %>%
    mutate(Week = paste('Week', weekNum),
           weekNumber = weekNum) %>%
    group_by(Week, weekNumber, team_name) %>%
    summarise(zScoreSum = sum(zScore)) %>%
      spread(key = team_name, value = zScoreSum) %>%
      mutate(position_type = 'Total')
  
  zScoreSumsCat3 <- rbind(zScoreSumsCat, zScoreSumsCat2)
  
}


actualValues <- actualValuesWeek('3')
rankings <- rankingsWeek('3')
zScore <- zScoreWeek('3')
zScoreSums <- zScoreSumsWeek('3')

# Creating ZScoreSums Table ----
zScoreSums <- tibble()

for (c in 1:4){
  zScoreSumsadd <- zScoreSumsWeek(c)
  
  zScoreSums <- rbind(zScoreSums, zScoreSumsadd)
}

zScoreSums2 <- zScoreSums %>%
  gather(key = 'team_name', value = 'zScoreSum', 4:15)

# Visualization ----
statsList <- c('R', 'HR', 'RBI', 'SB', 'OPS',
               'W', 'K', 'ERA', 'WHIP', 'SVH')

# Compare stats, 1 Team, hitting x
allWeekStats3 %>%
  filter(position_type == 'B', grepl('Awesome',team_name)) %>%
  ggplot(aes(x = as.numeric(team_stats_week), y = zScore, color = display_name)) +
  geom_line() +
  geom_point()

# Mean z-score by category ----
allWeekStats3 %>%
  filter(position_type == 'B', grepl('Awesome',team_name)) %>%
  #filter(as.numeric(team_stats_week) > 8) %>% # week filter for closer views
  group_by(display_name) %>%
  summarise(zScore = mean(zScore, na.rm = TRUE)) %>%
  ggplot(aes(x = display_name, y = zScore)) +
    geom_col()
# target ops!
# preferably at 3b

# Compare stats, 1 Team, pitching
allWeekStats3 %>%
  filter(position_type == 'P', grepl('Awesome',team_name)) %>%
  ggplot(aes(x = as.numeric(team_stats_week), y = zScore, color = display_name)) +
  geom_line() +
  geom_point()


# pitching
allWeekStats3 %>%
  filter(position_type == 'P', grepl('Awesome',team_name)) %>%
  group_by(display_name) %>%
  summarise(zScore = mean(zScore, na.rm = TRUE)) %>%
  ggplot(aes(x = display_name, y = zScore)) +
  geom_col()
# target whip, trade away K!
# i need to flip teh values for era and whip!!!!!!!!
# nola or burnes?


# Compare Teams, 1 Stat
allWeekStats3 %>%
  #filter(display_name == 'OPS', grepl('Awesome|Cuca',team_name)) %>%
  filter(display_name == 'OPS') %>%
  ggplot(aes(x = as.numeric(team_stats_week), y = zScore, color = team_name)) +
    geom_line() +
    geom_point()

# Compare one stat for all teams
allWeekStats3 %>%
  filter(display_name == 'OPS') %>%
  #filter(display_name == 'OPS', as.numeric(team_stats_week) > 8) %>%
  group_by(team_name) %>%
  summarise(zScore = mean(zScore, na.rm = TRUE)) %>%
  ggplot(aes(x = team_name, y = zScore)) +
    geom_col() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

# zScore, Compare stats 1 team
zScoreSums2 %>%
  filter(grepl('Awesome',team_name)) %>%
  ggplot(aes(x = weekNumber, y = zScoreSum, color = position_type, group = position_type)) +
  geom_line() +
  geom_point()

# zScoreSums2 %>%
#   filter(grepl('Aweso',team_name)) %>%
#   ggplot(aes(x = weekNumber, y = zScoreSum, color = position_type)) +
#   geom_line() +
#   geom_point()

# Total ZScore by Team
zScoreSums2 %>%
  filter(position_type == 'Total') %>%
  #filter(position_type == 'Total', weekNumber > 9) %>%
  group_by(team_name) %>%
  summarise(zScoreSum = sum(zScoreSum)) %>%
  ggplot(aes(x = team_name, y = zScoreSum)) +
  geom_col(fill = '#eb5534') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

# z score for each category!
zScoreSums2 %>%
  filter(position_type != 'Total') %>%
  group_by(team_name, position_type) %>%
  summarise(zScoreSum = sum(zScoreSum)) %>%
  ggplot(aes(x = team_name, y = zScoreSum, fill = position_type)) +
  geom_col(position = 'dodge') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


# to do,
# create packet for the last 2 weeks!
# create packet for the last 4 weeks
# create packet for the last 6 weeks


# shiny site
# dashboard to view all team info
# dashboard to filter visuals for one team!
  # stats over times, batting, pitching,
  # mean zscore for each stat for the selected team!
  # would you win visual?

  


# create the, would I have won? calculator

# relative strength, 
# relative strength when x is punted


# trades
# kyle, nola. Try to get morel + saves?
# dylan, nola. try to get verdugo/bohm + kimbrel 
# tom, nola. try to get Pham + robertson?
# nick, nola. try to get yoshida + ?bautista/clay Holmes


write_csv(actualValues, 'Output Files/week13.csv', na = '')
