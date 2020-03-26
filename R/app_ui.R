#' @import shiny
#' @import shinyWidgets
#' @import shinydashboard
app_ui <- function() {
  data = golem::get_golem_options()
  
  tagList(
    golem_add_external_resources(),
    
    dashboardPage(title = 'Topic browser',
                  dashboardHeader(title = 'Methode'),
                  dashboardSidebar(width = 400, collapsed=T,
                                   sidebarMenu(sidebar_ui(data))
                                   ),
                  dashboardBody(
                    
                    fluidRow(
                      
                      box(width=12, height=1000,
                          fluidRow(
                            column(width=3, shinyWidgets::pickerInput('topic_filter', width='100%', multiple=T, label = 'Kies een of meerdere topics', choices=list(), options = list(`actions-box` = TRUE))),
                            column(width=3, shinyWidgets::pickerInput('media_filter', width='100%', multiple=T, label = 'Filter op medium', choices=list(), options = list(`actions-box` = TRUE))),
                            column(width=2, selectInput('aggregate', label = 'Datum per', width = '100%', choices=list('Per dag'='day', 'Per week'='week', 'Per maand'='month'), selected = 'week')),
                            column(width=2, selectInput('dateselect', label = 'Datum selectie', width = '100%', choices=list('Afgelopen week'='week', 'Afgelopen maand'='maand', 'Afgelopen jaar'='jaar', 'Hele periode'='alles', 'Vrije selectie'='vrij'), selected = 'alles'))
                          ),
                          fluidRow(
                            column(width=4, align='left'),
                            column(width=8, align='center',
                              plotOutput('wordcloud', height='500px', width='500px'),
                              dygraphs::dygraphOutput("dategraph", height='300px', width = '90%')
                            )
                          )
                      )                           
                    ),
                    
                    fluidRow(
                      box(width=6, height=1000,
                          h2(textOutput('articlelist_header')),
                          br(),
                          div(sparkline::sparklineOutput("test_spark"),
                              DT::dataTableOutput('articlelist', height = 'auto', width='100%'))
                      ),
                      
                      box(width=6, height=1000,
                          shiny::fluidRow(
                            column(width=12, height=900,
                                   h3('Nieuwsberichten', align='center'),
                                   br(),
                                   div(class='textbox', 
                                       htmlOutput('articles'),  
                                       style = "overflow-y: scroll; height: 750px")
                            )
                          )
                      )
                    ))
    )
  )
}

sidebar_ui <- function(data, sidebarheight='200vh', inputcontainer_height) {
  topic_names = readRDS(data$topic_names_file)
  l = as.list(paste0('topic_', 1:length(topic_names)))
  names(l) = topic_names
  div(
    h2('Topic labels', align='center'),
    selectizeInput('sb_select_topic', 'Kies een topic om aan te passen', choices = l, selected=l[[1]]),
    div(
      shinyWidgets::searchInput('sb_rename_topic', label = "Geef nieuw label", value='', btnSearch = icon('refresh'))
    )
  )
  
  
  #div(
  #  h2('Topic labels', align='center'),
  #  lapply(1:length(topic_names), function(i) {
  #    shinyWidgets::searchInput(paste0('topic_',i), label = NULL, value=topic_names[i], resetValue=topic_names[i],
  #                              btnSearch = icon('refresh'))
  #  })
  #)
}


#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'shinyBZtopics')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
