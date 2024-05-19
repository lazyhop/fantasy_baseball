
# Loading Libaries ----
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
# I can create static versions of these files to use while not online!
# so like...right now only lol
teams <- y_teams(league_key = '431.l.93970', token_name = my_token)

allWeeks <- y_team_stats(key = teams$team_key, my_token, week = c(1:24))

stat_cats <- y_stats_categories(game_key = '431', my_token) %>%
  select(stat_id, display_name, position_type)


# exporting files
save(teams, file = 'Output Data/teams.RDS')
save(allWeeks, file = 'Output Data/allWeeks.RDS')
save(stat_cats, file = 'Output Data/stat_cats.RDS')


