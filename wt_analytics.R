## Взвешенная энропия
`%>%` <- magrittr::`%>%`
source('utils.R')

ngame_sequence <- seq(10, 25)

nba_data <- read_and_transform_gamelogs('appnba/data/gamelog.csv')

date <- unique(sort(as.Date(nba_data$GAME_DATE), decreasing = FALSE))

nba_rank_data <- get_nba_rank_data(nba_data)

### Для конференций
wt_conf_entropy <- function(data){
  ngame <- unique(data$NGAME)
  l <- lapply(ngame, function(x, data){
    
    ndate <- count_date_intervals(data, x)
    
    wt <- dplyr::filter(data, NGAME == x) %>% 
      dplyr::select(CONFERENCE, RANK, TEAM_ABBREVIATION) %>% 
      dplyr::group_by(CONFERENCE, RANK, TEAM_ABBREVIATION) %>% 
      dplyr::summarise(COUNT = dplyr::n(), .groups = 'drop_last') %>% 
      dplyr::mutate(NWEEK = sum(COUNT)) %>% 
      dplyr::mutate(SHARE = COUNT/NWEEK) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(CONFERENCE) %>% 
      dplyr::mutate(NWEEKALL = ndate*15) %>% 
      dplyr::mutate(NWEEKSHARE = NWEEK/NWEEKALL) %>% 
      dplyr::group_by(CONFERENCE, RANK, NWEEKSHARE) %>% 
      dplyr::summarise(ENTROPY = 1 - sum(SHARE^2), .groups = 'drop') %>% 
      dplyr::group_by(CONFERENCE) %>% 
      dplyr::summarise(ENTROPY = weighted.mean(ENTROPY, NWEEKSHARE), .groups = 'drop') %>% 
      dplyr::mutate(NGAME = x)
    
  }, data=data)
  
  return(dplyr::bind_rows(l))
}

wt_conf <- wt_conf_entropy(nba_rank_data)

plot_conf_entropy(wt_conf)

### Для позиций
wt_rank_entropy <- function(data){
  ngame <- unique(data$NGAME)
  l <- lapply(ngame, function(x, data){
    
    ndate <- count_date_intervals(data, x)
    
    wt <- dplyr::filter(data, NGAME == x) %>% 
      dplyr::select(CONFERENCE, RANK, TEAM_ABBREVIATION) %>% 
      dplyr::group_by(CONFERENCE, RANK, TEAM_ABBREVIATION) %>% 
      dplyr::summarise(COUNT = dplyr::n(), .groups = 'drop_last') %>% 
      dplyr::mutate(NWEEK = sum(COUNT)) %>% 
      dplyr::mutate(SHARE = COUNT/NWEEK) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(CONFERENCE) %>% 
      dplyr::group_by(CONFERENCE, RANK) %>% 
      dplyr::summarise(ENTROPY = 1 - sum(SHARE^2), .groups = 'drop') %>% 
      dplyr::mutate(NGAME = x)
    
  }, data=data)
  
  return(dplyr::bind_rows(l))
}

wt_rank <- wt_rank_entropy(nba_rank_data)

plot_rank_entropy(wt_rank)

wt_rank_median <- wt_rank %>% 
  dplyr::group_by(CONFERENCE, RANK) %>% 
  dplyr::summarise(ENTROPY = median(ENTROPY), .groups='drop')

plot_rank_median_entropy(wt_rank_median)

## Для команд
wt_teams_entropy <- function(data){

  ngame <- unique(data$NGAME)
  l <- lapply(ngame, function(x, data){

    wt <- dplyr::filter(nba_rank_data, NGAME == x) %>%
      dplyr::select(CONFERENCE, RANK, TEAM_ABBREVIATION) %>%
      dplyr::group_by(CONFERENCE, TEAM_ABBREVIATION, RANK) %>%
      dplyr::summarise(COUNT = dplyr::n(), .groups = 'drop_last') %>%
      dplyr::mutate(NWEEK = sum(COUNT)) %>%
      dplyr::mutate(SHARE = COUNT/NWEEK) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(TEAM_ABBREVIATION) %>%
      dplyr::summarise(ENTROPY = 1 - sum(SHARE^2), .groups = 'drop') %>%
      dplyr::mutate(NGAME = x)

  }, data=data)

  return(dplyr::bind_rows(l))
}

wt_teams <- wt_teams_entropy(nba_rank_data)

plot_teams_entropy(wt_teams)

wt_teams_median <- wt_teams %>%
  dplyr::group_by(TEAM_ABBREVIATION) %>%
  dplyr::summarise(ENTROPY = median(ENTROPY), .groups='drop')
