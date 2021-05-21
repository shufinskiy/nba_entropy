source('utils.R')

`%>%` <- magrittr::`%>%`

dt <- read_and_transform_gamelogs('appnba/data/gamelog.csv')
date <- unique(sort(as.Date(dt$GAME_DATE), decreasing = FALSE))

one_team <- 'DAL'
some_team <- c('DAL', 'POR')

plot_team_count_position(dt, date, one_team, 10)

plot_team_rank_day_to_day(dt, date, one_team, 10)

plot_teams_count_position(dt, date, some_team, 10)

plot_teams_rank_day_to_day(dt, date, some_team, 10)