`%>%` <- magrittr::`%>%`

read_and_transform_gamelogs <- function(path){
  dt <- read.csv(path) %>% 
    dplyr::mutate(GAME_DATE = as.Date(GAME_DATE)) %>% 
    dplyr::arrange(GAME_DATE) %>% 
    dplyr::mutate(CONFERENCE = dplyr::if_else(TEAM_ID %in% west_conf, 'W', 'E'))

}

get_nba_rank_data <- function(data){
  lapply(ngame_sequence, function(x, data, date){
    dt <- games_winshare_by_days(data, date, x)
    dt <- dplyr::mutate(dt, NGAME = x)
    return(dt)
  }, data=nba_data, date=date) %>% 
    dplyr::bind_rows()
}

games_winshare_by_days <- function(data, date, ngame=10){
  l <- list()
  for(i in date){
    dt_ <- data %>% 
      dplyr::filter(GAME_DATE >= i) %>% 
      dplyr::group_by(TEAM_ABBREVIATION) %>% 
      dplyr::slice_head(n=ngame)
    
    check <- dt_ %>% 
      dplyr::group_by(TEAM_ABBREVIATION) %>% 
      dplyr::summarise(cnt = dplyr::n(), .groups = 'drop') %>% 
      dplyr::pull(cnt)
    
    if (!all(check == ngame)){
      break
    }
    
    dt_ <- dt_ %>%
      dplyr::group_by(TEAM_ABBREVIATION, CONFERENCE, WL) %>%
      dplyr::summarise(WINSHARE = dplyr::n(), .groups = 'drop') %>%
      tidyr::complete(tidyr::nesting(TEAM_ABBREVIATION, CONFERENCE), WL, fill = list('WINSHARE' = 0)) %>% 
      dplyr::filter(WL == 'W') %>%
      dplyr::mutate(WINSHARE = WINSHARE/ngame) %>%
      dplyr::group_by(CONFERENCE) %>%
      dplyr::mutate(RANK = rank(-WINSHARE, ties.method = 'min')) %>%
      dplyr::ungroup() %>%
      dplyr::select(-WL) %>%
      dplyr::mutate(DATE = as.Date(i, origin = '1970-01-01'))
    
    l[as.character(unique(dt_$DATE))] <- list(dt_)
  }
  df <- dplyr::bind_rows(l)
  return(df)
}

count_date_intervals <- function(data, ngame){
  dplyr::filter(data, NGAME == ngame) %>%
      dplyr::select(DATE) %>%
      dplyr::distinct() %>%
      dplyr::pull() %>%
      length()
}

plot_team_count_position <- function(data, date, team, ngame=10){
  team_name <- dplyr::pull(dplyr::distinct(dplyr::filter(data, TEAM_ABBREVIATION == team), TEAM_NAME))
  df  <- games_winshare_by_days(data, date, ngame=ngame)
  
  gg <- df %>% 
    dplyr::filter(TEAM_ABBREVIATION == team) %>% 
    dplyr::group_by(RANK) %>% 
    dplyr::summarise(NDAY = dplyr::n(), .groups = 'drop') %>% 
    ggplot2::ggplot(., ggplot2::aes(x=RANK, y=NDAY)) +
    ggplot2::geom_col(fill=dplyr::pull(dplyr::select(dplyr::filter(table_color, TEAM_ABBREVIATION == team), col1)), alpha = 0.3) +
    ggplot2::scale_x_continuous(breaks = seq(1, 15)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0(team_name, " ", ngame, 
                                 "-rolling games; x-axis - position in  conference on segment; y-axis - number of days for each position in conference")) +
    ggplot2::xlab('position in conference') +
    ggplot2::ylab('number of days')
  return(gg)
}

plot_team_rank_day_to_day <- function(data, date, team, ngame=10){
  team_name <- dplyr::pull(dplyr::distinct(dplyr::filter(data, TEAM_ABBREVIATION == team), TEAM_NAME))
  
  df  <- games_winshare_by_days(data, date, ngame=ngame)
  scalex <- if (dplyr::pull(dplyr::count(dplyr::distinct(dplyr::select(df, DATE)))) < 7) FALSE else TRUE
  
  gg <- df %>% 
    dplyr::filter(TEAM_ABBREVIATION == team) %>% 
    ggplot2::ggplot(., ggplot2::aes(x=DATE, y=RANK, fill=WINSHARE)) +
    ggplot2::geom_col(color=dplyr::pull(dplyr::select(dplyr::filter(table_color, TEAM_ABBREVIATION == team), col2))) +
    ggplot2::scale_y_reverse(breaks = seq(1, 15)) +
    ggplot2::coord_cartesian(ylim = c(15, 0.5), expand = FALSE) +
    {if(scalex) ggplot2::scale_x_date(breaks = 'week')} +
    ggplot2::scale_fill_gradient(low='white', high=dplyr::pull(dplyr::select(dplyr::filter(table_color, TEAM_ABBREVIATION == team), col1)),
                                 limits=c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   axis.title.x = ggplot2::element_blank()) +
    ggplot2::labs(title = paste0(team_name, " ", ngame, 
                                 "-rolling games; x-axis - Start date; y-axis - position in  conference on segment")) + 
    ggplot2::ylab('position in conference')
  return(gg)
}

plot_teams_count_position <- function(data, date, teams, ngame=10){
  df <- games_winshare_by_days(data, date, ngame = ngame)
  
  gg <- df %>% 
    dplyr::filter(TEAM_ABBREVIATION %in% teams) %>% 
    dplyr::group_by(TEAM_ABBREVIATION, RANK) %>% 
    dplyr::summarise(NDAY = dplyr::n(), .groups = 'drop') %>% 
    dplyr::rename(TEAM = TEAM_ABBREVIATION) %>% 
    ggplot2::ggplot(., ggplot2::aes(x=RANK, y=NDAY, fill=TEAM)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(preserve = 'single'), alpha=0.3) +
    ggplot2::theme_minimal() + 
    ggplot2::scale_x_continuous(breaks = seq(1, 15)) +
    ggplot2::labs(title = paste0(ngame, '-rolling games; x-axis - position in  conference on segment; y-axis - number of days for each position in conference'))
  
  return(gg)
}

plot_teams_rank_day_to_day <- function(data, date, teams, ngame=10){
  df <- games_winshare_by_days(data, date, ngame=ngame)
  scalex <- if (dplyr::pull(dplyr::count(dplyr::distinct(dplyr::select(df, DATE)))) < 7) FALSE else TRUE
  point <- if (dplyr::pull(dplyr::count(dplyr::distinct(dplyr::select(df, DATE)))) < 2) TRUE else FALSE
  
  gg <- df %>% 
    dplyr::filter(TEAM_ABBREVIATION %in% teams) %>% 
    dplyr::rename(TEAM = TEAM_ABBREVIATION) %>% 
    ggplot2::ggplot(., ggplot2::aes(x=DATE, y=RANK, colour=TEAM, labels=WINSHARE)) +
    {if(point) ggplot2::geom_point()} +
    ggplot2::geom_line() +
    ggplot2::scale_y_reverse(breaks = seq(1, 15)) +
    ggplot2::coord_cartesian(ylim = c(15, 0.5), expand = if (point) TRUE else FALSE) +
    {if(scalex) ggplot2::scale_x_date(breaks = 'week')} +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   axis.title.x = ggplot2::element_blank()) +
    ggplot2::labs(title=paste0(ngame,
                               "-rolling games; x-axis - Start date; y-axis - position in  conference on segment")) +
    ggplot2::ylab('position in conference')
  return(gg)
}

plot_conf_entropy <- function(data){
  
  ggplot2::ggplot(data, ggplot2::aes(x=NGAME, y=ENTROPY, colour=CONFERENCE)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(breaks=seq(10, 25)) +
    ggplot2::scale_y_continuous(n.breaks = 10) +
    ggplot2::labs(title='Entropy for different number of games by conference') +
    ggplot2::xlab('number of games per segment') +
    ggplot2::ylab('Entropy') +
    ggplot2::theme(legend.position = 'bottom',
                   plot.title = ggplot2::element_text(hjust=0.5))
}

plot_rank_entropy <- function(data){
  ggplot2::ggplot(data, ggplot2::aes(x=NGAME, y=ENTROPY, colour=CONFERENCE)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(RANK)) +
    ggplot2::theme_light() +
    ggplot2::labs(title = 'Entropy for each position and different number of games by conference') +
    ggplot2::xlab('number of games per segment') +
    ggplot2::ylab('Entropy') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5),
                   legend.position = 'bottom')
}

plot_rank_median_entropy <- function(data){
  ggplot2::ggplot(data, ggplot2::aes(x=RANK, y=ENTROPY, colour=CONFERENCE)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = seq(1, 15)) +
    ggplot2::scale_y_continuous(breaks = seq(0.2, 0.85, 0.05)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = 'Median entropy for conference positions') +
    ggplot2::xlab('position in conference') +
    ggplot2::ylab('Entropy') +
    ggplot2::theme(legend.position = 'bottom',
                   plot.title = ggplot2::element_text(hjust=0.5))
}

plot_teams_entropy <- function(data){
  ggplot2::ggplot(data, ggplot2::aes(x=NGAME, y=ENTROPY, colour=TEAM_ABBREVIATION)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(TEAM_ABBREVIATION)) +
    ggplot2::theme_light() +
    ggplot2::labs(title = 'Median entropy for different number of games by teams') +
    ggplot2::xlab('number of games per segment') +
    ggplot2::ylab('Entropy') +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5),
                   legend.position = 'none')
  
}

east_conf <- c(1610612737, 1610612738, 1610612751, 1610612766, 1610612741, 1610612739, 1610612765, 1610612754,
               1610612748, 1610612749, 1610612752, 1610612753, 1610612755, 1610612761, 1610612764)

west_conf <- c(1610612742, 1610612743, 1610612744, 1610612745, 1610612746, 1610612747, 1610612763, 1610612750,
               1610612740, 1610612760, 1610612756, 1610612757, 1610612758, 1610612759, 1610612762)

table_color <- data.frame(TEAM_ID = c(1610612737, 1610612738, 1610612751, 1610612766, 1610612741, 1610612739, 1610612742,
                                      1610612743, 1610612765, 1610612744, 1610612745, 1610612754, 1610612746, 1610612747,
                                      1610612763, 1610612748, 1610612749, 1610612750, 1610612740, 1610612752, 1610612760,
                                      1610612753, 1610612755, 1610612756, 1610612757, 1610612758, 1610612759, 1610612761,
                                      1610612762, 1610612764),
                          TEAM_NAME = c("Atlanta Hawks",          "Boston Celtics",         "Brooklyn Nets",         
                                        "Charlotte Hornets",      "Chicago Bulls",          "Cleveland Cavaliers",   
                                        "Dallas Mavericks",       "Denver Nuggets",         "Detroit Pistons",       
                                        "Golden State Warriors",  "Houston Rockets",        "Indiana Pacers",        
                                        "LA Clippers",            "Los Angeles Lakers",     "Memphis Grizzlies",     
                                        "Miami Heat",             "Milwaukee Bucks",        "Minnesota Timberwolves",
                                        "New Orleans Pelicans",   "New York Knicks",        "Oklahoma City Thunder", 
                                        "Orlando Magic",          "Philadelphia 76ers",     "Phoenix Suns",          
                                        "Portland Trail Blazers", "Sacramento Kings",       "San Antonio Spurs",     
                                        "Toronto Raptors",        "Utah Jazz",              "Washington Wizards"),
                          TEAM_ABBREVIATION = c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL",
                                                "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR",
                                                "UTA", "WAS"),
                          col1 = c("#E03A3E", "#007A33", "#000000", "#1D1160", "#CE1141", "#6F263D", "#00538C", "#0E2240",
                                   "#C8102E", "#006BB6", "#CE1141", "#002D62", "#C8102E", "#552583", "#5D76A9", "#98002E",
                                   "#00471B", "#0C2340", "#0C2340", "#006BB6", "#007AC1", "#0077C0", "#006BB6", "#1D1160",
                                   "#E03A3E", "#5A2D81", "#C4CED4", "#CE1141", "#002B5C", "#002B5C"),
                          name_col1 = c("HAWKS RED",           "CELTICS GREEN",       "BLACK",              
                                        "HORNETS PURPLE",      "BULLS RED",           "CAVALIERS WINE",     
                                        "ROYAL BLUE",          "MIDNIGHT BLUE",       "RED",                
                                        "WARRIORS ROYAL BLUE", "RED",                 "PACERS BLUE",        
                                        "RED",                 "LAKERS PURPLE",       "BLUE",               
                                        "RED",                 "GOOD LAND GREEN",     "MIDNIGHT BLUE",      
                                        "PELICANS NAVY",       "KNICKS BLUE",         "THUNDER BLUE",       
                                        "MAGIC BLUE",          "BLUE",                "PURPLE",             
                                        "RED",                 "PURPLE",              "SILVER",             
                                        "RED",                 "NAVY",                "NAVY BLUE"),
                          col2 = c("#C1D32F", "#BA9653", "#FFFFFF", "#00788C", "#000000", "#041E42", "#002B5E", "#FEC524",
                                   "#006BB6", "#FDB927", "#000000", "#FDBB30", "#1D428A", "#FDB927", "#12173F", "#F9A01B",
                                   "#EEE1C6", "#236192", "#C8102E", "#F58426", "#EF3B24", "#C4CED4", "#ED174C", "#E56020",
                                   "#000000", "#63727A", "#000000", "#000000", "#00471B", "#E31837"),
                          name_col2 = c("VOLT GREEN",       "CELTICS GOLD",     "WHITE",            "TEAL",            
                                        "BLACK",            "CAVALIERS NAVY",   "NAVY BLUE",        "SUNSHINE YELLOW", 
                                        "ROYAL",            "GOLDEN YELLOW",    "BLACK",            "YELLOW",          
                                        "BLUE",             "GOLD",             "NAVY",             "YELLOW",          
                                        "CREAM CITY CREAM", "LAKE BLUE",        "PELICANS RED",     "KNICKS ORANGE",   
                                        "SUNSET",           "SILVER",           "RED",              "ORANGE",          
                                        "BLACK",            "GRAY",             "BLACK",            "BLACK",           
                                        "GREEN",            "RED"),
                          col3 = c("#26282A", "#963821", "#CD1041",        "#A1A1A4", NA,        "#FFB81C", "#B8C4CA", "#8B2131",
                                   "#BEC0C2", "#26282A", "#C4CED4", "#BEC0C2", "#BEC0C2", "#000000", "#F5B112", "#000000",
                                   "#0077C0", "#9EA2A2", "#85714D", "#BEC0C2", "#002D62", "#000000", "#002B5C", "#000000",
                                   NA,        "#000000", NA,        "#A1A1A4", "#F9A01B", "#C4CED4"),
                          name_col3 = c("HAWKS CHARCOAL",   "CELTICS BROWN",    "RED",                 "GRAY",            
                                        NA,                 "CAVALIERS NAVY",  "SILVER",           "FLATIRONS RED",   
                                        "GRAY",             "SLATE",            "SILVER",           "SILVER",          
                                        "SILVER",           "BLACK",            "YELLOW",           "BLACK",           
                                        "GREAT LAKES BLUE", "MOONLIGHT GREY",   "PELICANS GOLD",    "KNICKS SILVER",   
                                        "BLUE",             "BLACK",            "NAVY",             "BLACK",           
                                        NA,                "BLACK",            NA,                 "SILVER",          
                                        "YELLOW",           "SILVER" ),
                          col4 = c( NA,        "#E59E6D", NA,        NA,        NA,        "#000000", "#000000", "#1D428A",
                                    "#002D62", NA,        NA,        NA,        "#000000", NA,        "#707271", NA,       
                                    "#000000", "#78BE20", NA,        "#000000", "#FDBB30", NA,        "#C4CED4", "#63727A",
                                    NA,        NA,        NA,        "#B4975A", NA,        NA   ),
                          name_col4 = c( NA,                "CELTICS BEIGE",   NA,                NA,               
                                         NA,                "CAVALIERS BLACK", "BLACK",           "SKYLINE BLUE",   
                                         "NAVY",            NA,                NA,                NA,               
                                         "BLACK",           NA,                "GRAY",            NA,              
                                         "BLACK",           "AURORA GREEN",    NA,                "KNICKS BLACK",   
                                         "YELLOW",          NA,                "SILVER",          "GRAY",           
                                         NA,                NA,                NA,                "GOLD",           
                                         NA,                NA ),
                          col5 = c(NA,        "#000000", NA,        NA,        NA,        NA,        NA,        NA,       
                                   NA,        NA,        NA,        NA,        NA,        NA,        NA,        NA,       
                                   NA,        NA,        NA,        NA,        NA,        NA,        NA,        "#F9AD1B",
                                   NA,        NA,        NA,        NA,        NA,        NA   ),
                          name_col5 = c(NA,              "CELTICS BLACK", NA,              NA,              NA,             
                                        NA,              NA,              NA,              NA,              NA,             
                                        NA,              NA,              NA,              NA,              NA,             
                                        NA,              NA,              NA,              NA,              NA,             
                                        NA,              NA,              NA,              "YELLOW",        NA,             
                                        NA,             NA,              NA,              NA,              NA  ),
                          col6 = c(NA,        NA,        NA,        NA,        NA,        NA,        NA,        NA,       
                                   NA,       NA,        NA,        NA,        NA,        NA,        NA,        NA,       
                                   NA,        NA,        NA,        NA,        NA,        NA,        NA,        "#B95915",
                                   NA,        NA,       NA,        NA,        NA,        NA    ),
                          name_col6 = c(NA,            NA,            NA,            NA,            NA,            NA,           
                                        NA,            NA,            NA,            NA,            NA,            NA,           
                                        NA,            NA,            NA,            NA,            NA,            NA,           
                                        NA,            NA,            NA,            NA,            NA,            "DARK ORANGE",
                                        NA,            NA,            NA,            NA,            NA,            NA ),
                          col7 = c(NA,        NA,        NA,        NA,        NA,        NA,        NA,        NA,       
                                   NA,        NA,        NA,        NA,        NA,        NA,        NA,        NA,       
                                   NA,        NA,        NA,        NA,        NA,        NA,        NA,        "#BEC0C2",
                                   NA,        NA,        NA,        NA,        NA,        NA ),
                          name_col7 = c(NA,           NA,           NA,           NA,           NA,           NA,          
                                        NA,           NA,           NA,           NA,           NA,           NA,          
                                        NA,           NA,           NA,           NA,           NA,           NA,          
                                        NA,           NA,           NA,           NA,           NA,           "LIGHT GRAY",
                                        NA,           NA,           NA,           NA,           NA,           NA))
