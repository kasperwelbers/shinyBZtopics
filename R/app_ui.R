#' @import shiny
#' @import shinyWidgets
#' @import shinydashboard
app_ui <- function() {
  tagList(
    golem_add_external_resources(),
    
    dashboardPage(title = 'Topic browser',
                  dashboardHeader(title = 'Methode'),
                  dashboardSidebar(width = 300, collapsed=T,
                                   sidebarMenu(sidebar_ui())
                                   ),
                  dashboardBody(
                    
                    fluidRow(
                      
                      box(width=12, height=600,
                          fluidRow(
                            column(width=3, shinyWidgets::pickerInput('topic_filter', width='100%', multiple=T, label = 'Kies een of meerdere topics', choices=list(), options = list(`actions-box` = TRUE))),
                            column(width=3, shinyWidgets::pickerInput('media_filter', width='100%', multiple=T, label = 'Filter op medium', choices=list(), options = list(`actions-box` = TRUE))),
                            column(width=2, selectInput('aggregate', label = 'Datum per', width = '100%', choices=list('Per dag'='day', 'Per week'='week', 'Per maand'='month'), selected = 'week')),
                            column(width=2, selectInput('dateselect', label = 'Datum selectie', width = '100%', choices=list('Afgelopen week'='week', 'Afgelopen maand'='maand', 'Afgelopen jaar'='jaar', 'Hele periode'='alles', 'Vrije selectie'='vrij'), selected = 'alles'))
                          ),
                          div(align='center',
                              dygraphs::dygraphOutput("dategraph", height='420px', width = '90%')
                          )
                      )                           
                    ),
                    
                    fluidRow(
                      box(width=6, height=1000,
                          h2(textOutput('articlelist_header')),
                          br(),
                          div(DT::dataTableOutput('articlelist', height = 'auto', width='100%'))
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

sidebar_ui <- function(sidebarheight='200vh', inputcontainer_height) {
  div(list(
    h3('Some meta functions (e.g., word blacklist')
  ))
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
