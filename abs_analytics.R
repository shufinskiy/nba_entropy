`%>%` <- magrittr::`%>%`
source('utils.R')

ngame_sequence <- seq(10, 25)

nba_data <- read_and_transform_gamelogs('appnba/data/gamelog.csv')

date <- unique(sort(as.Date(nba_data$GAME_DATE), decreasing = FALSE))

nba_rank_data <- get_nba_rank_data(nba_data)

### Для конференций
abs_conf_entropy <- function(data){
  ngame <- unique(data$NGAME)
  l <- lapply(ngame, function(x, data){

    dt <- dplyr::filter(data, NGAME == x) %>% 
      dplyr::select(CONFERENCE, RANK, TEAM_ABBREVIATION) %>% 
      dplyr::distinct() %>% 
      dplyr::group_by(CONFERENCE, RANK) %>% 
      dplyr::summarise(COUNT = dplyr::n(), .groups = 'drop')
    
    data.frame(NGAME=x, 
               EAST_CONF = sum(dplyr::pull(dplyr::select(dplyr::filter(dt, CONFERENCE == 'E'), COUNT))/15)/15,
               WEST_CONF = sum(dplyr::pull(dplyr::select(dplyr::filter(dt, CONFERENCE == 'W'), COUNT))/15)/15)
    
  }, data=data)
  df <- dplyr::bind_rows(l) %>% 
    tidyr::pivot_longer(c(EAST_CONF, WEST_CONF), names_to = 'CONFERENCE', values_to = 'ENTROPY')
  return(df)
}

abs_conf <- abs_conf_entropy(nba_rank_data)

plot_conf_entropy(abs_conf)

### Для позиций
abs_rank_entropy <- function(data){
  data %>% 
    dplyr::select(CONFERENCE, TEAM_ABBREVIATION, RANK, NGAME) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(CONFERENCE, RANK, NGAME) %>% 
    dplyr::summarise(ENTROPY = dplyr::n()/15, .groups = 'drop')
}

abs_rank <- abs_rank_entropy(nba_rank_data)

plot_rank_entropy(abs_rank)

abs_rank_median <- abs_rank %>% 
  dplyr::group_by(CONFERENCE, RANK) %>% 
  dplyr::summarise(ENTROPY = median(ENTROPY), .groups='drop')

plot_rank_median_entropy(abs_rank_median)

### Для команд
abs_teams_entropy <- function(data){
  data %>% 
    dplyr::select(TEAM_ABBREVIATION, NGAME, RANK) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(TEAM_ABBREVIATION, NGAME) %>% 
    dplyr::summarise(ENTROPY = dplyr::n()/15, .groups = 'drop_last')
}

abs_teams <- abs_teams_entropy(nba_rank_data)

plot_teams_entropy(abs_teams)

abs_teams_median <- abs_teams %>% 
  dplyr::summarise(ENTROPY = median(ENTROPY), .groups='drop')
