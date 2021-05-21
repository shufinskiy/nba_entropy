library(shiny, warn.conflicts = FALSE)
library(shinythemes, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)

nbadata <- read.csv('data/gamelog.csv')
source('helpers.R')

ui <- shiny::fluidPage(theme = shinythemes::shinytheme('flatly'),
                shiny::titlePanel(shiny::h1('NBA season 2020/21 application', align = 'center'), windowTitle = 'NBA season 2020/21 application'),
                
                shiny::sidebarLayout(
                    shiny::sidebarPanel(width = 2,
                        shiny::selectInput('team', 'Select team(s):', choices = sort(unique(nbadata$TEAM_ABBREVIATION)),
                                    selected = sort(unique(nbadata$TEAM_ABBREVIATION))[1], multiple = TRUE),
                        shiny::br(),
                        shiny::radioButtons('type', 'Chart type', 
                                     choices = list("Rank day-to-day" = "dtd",
                                                    "NDAY by rank position" = 'nday'), selected = 'dtd'),
                        shiny::br(),
                        shiny::sliderInput('ngame', 'N game', 1, 72, 10, 1),
                        shiny::br(),
                        shiny::dateRangeInput('date', 'Select daterange:', start = '2020-12-22', end = '2021-05-16', 
                                       min = '2020-12-22', max = '2021-05-16', weekstart = 1),
                        shiny::br(),
                        shiny::submitButton('Make')
                    ),
                    
                    shiny::mainPanel(width = 10,
                              shiny::tabsetPanel(
                                  shiny::tabPanel(shiny::h3("Plot"),
                                           plotly::plotlyOutput('plot', height = '800px')
                                           ),
                                  shiny::tabPanel(shiny::h3("Data"),
                                           DT::dataTableOutput('table')
                                           )
                                  )
                              )
                              
                )
    

)


server <- function(input, output) {
    
    data <- shiny::reactive({
        data <- nbadata %>% 
            dplyr::mutate(GAME_DATE = as.Date(GAME_DATE)) %>% 
            dplyr::arrange(GAME_DATE) %>% 
            dplyr::mutate(CONFERENCE = dplyr::if_else(TEAM_ID %in% west_conf, 'W', 'E')) %>% 
            dplyr::filter(GAME_DATE >= input$date[1] & GAME_DATE <= input$date[2])
        data
    })

    date <- shiny::reactive({unique(sort(as.Date(data()$GAME_DATE), decreasing = FALSE))}) 
    
    len_teams <- shiny::reactive({if(length(input$team) == 1) '1' else 'many'})
    
    output$plot <- plotly::renderPlotly({
        shiny::withProgress(message = 'Loading data', value = 1, detail = 'please waiting', {
            
            plot_func <- switch(len_teams(),
                                '1' = switch(input$type,
                                             nday = plot_team_count_position,
                                             dtd = plot_team_rank_day_to_day),
                                'many' = switch(input$type,
                                                nday = plot_teams_count_position,
                                                dtd = plot_teams_rank_day_to_day)
                                )
            
            tryCatch(plotly::ggplotly(plot_func(data(), date(), input$team, input$ngame)), error = function(e) plotly::plotly_empty())
        })
        })
    
    output$table <- DT::renderDT({
        
        dt <- data() %>% 
            dplyr::filter(TEAM_ABBREVIATION %in% input$team) %>% 
            dplyr::select(TEAM_NAME, GAME_DATE, MATCHUP, WL, PTS, PLUS_MINUS)
        
        DT::datatable(dt) %>% 
            DT::formatStyle('WL', color = DT::styleEqual(sort(unique(dt$WL)), c('red', 'green')))
        })
}

shiny::shinyApp(ui = ui, server = server)
