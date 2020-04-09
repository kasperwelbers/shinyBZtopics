#' @import shiny
#' @import shinyWidgets
#' @import shinydashboard
app_ui <- function() {
  data = golem::get_golem_options()
  
  tagList(
    golem_add_external_resources(),
    
    dashboardPage(title = 'Topic browser',
                  dashboardHeader(title = 'Instellingen'),
                  dashboardSidebar(width = 400, collapsed=T,
                                   sidebar_ui(data, q)
                                   ),
                  dashboardBody(
                    
                    fluidRow(
                      
                      box(width=12, height=700, 
                          fluidRow(
                            column(width=3, shinyWidgets::pickerInput('topic_filter', width='100%', multiple=T, label = 'Kies een of meerdere topics', choices=list(), options = list(`actions-box` = TRUE))),
                            column(width=3, shinyWidgets::pickerInput('query_filter', width='100%', multiple=T, label = 'Filter op zoekterm', choices=list(), options = list(`actions-box` = TRUE))),
                            column(width=2, shinyWidgets::pickerInput('media_filter', width='100%', multiple=T, label = 'Filter op medium', choices=list(), options = list(`actions-box` = TRUE))),
                            column(width=2, selectInput('aggregate', label = 'Datum per', width = '100%', choices=list('Per dag'='day', 'Per week'='week', 'Per maand'='month'), selected = 'week')),
                            column(width=2, selectInput('dateselect', label = 'Datum selectie', width = '100%', choices=list('Afgelopen week'='week', 'Afgelopen maand'='maand', 'Afgelopen jaar'='jaar', 'Hele periode'='alles', 'Vrije selectie'='vrij'), selected = 'alles'))
                          ),
                          fluidRow(
                            conditionalPanel("input.topic_filter.length >= 1", 
                              column(width=6, align='left',
                                dygraphs::dygraphOutput("dategraph", height='300px', width = '90%'),
                                br(),
                                column(offset=1, width=11, div(id='legend', style = "overflow-y: scroll; height: 250px")),
                                div(downloadLink('save_graph', 'PNG'),
                                    downloadLink('save_data', 'CSV'), align='center'))
                            ),
                            column(width=6, align='center',
                              plotOutput('wordcloud', height='500px', width='500px')
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

sidebar_ui <- function(data, q, sidebarheight='200vh', inputcontainer_height) {
  topic_names = readRDS(data$topic_names_file)
  l = as.list(paste0('topic_', 1:length(topic_names)))
  names(l) = topic_names
  
  tagList(
    div(align='center',
      radioButtons('sidebar_tab', inline=T, label="Wat wil je aanpassen?", 
                  choices = list('Labels en kleuren'= 'modify_topics', 'Groepen'= 'modify_groups', 'Zoektermen'='modify_queries'), 
                  selected = 'modify_topics')
    ),
    br(), br(),
    conditionalPanel("input.sidebar_tab == 'modify_topics' || input.sidebar_tab == 'modify_groups'", {
      div(align='center', style = "width: 350px; height: 220px; margin-left: 20px",
        div(align='center', selectizeInput('sb_select_topic', 'Selecteer topic', choices = l, selected=l[[1]])),
        column(width=12, 
               #h5('Frequente woorden'),
               textOutput('sb_top_words_prob'),
               #h5('Onderscheidende woorden'),
               #textOutput('sb_top_words_lift')
        )
      )
    }),
    conditionalPanel("input.sidebar_tab == 'modify_topics'", {
      div(align='left',style="margin-left:20px", 
        #h2('Topic labels', align='center'),
        column(width=12, p('Hier kun je de labels en kleuren van topics aanpassen. Veranderingen zijn voor alle gebruikers zichtbaar.')),
        fluidRow(
          div(align='center',
            column(width=8, shinyWidgets::searchInput('sb_rename_topic', label = "Nieuw label", value='', btnSearch = icon('refresh'))),
            column(width=4, colourpicker::colourInput('sb_color_topic', label = 'Kleur'))
          )
        )
      )
    }),
    conditionalPanel("input.sidebar_tab == 'modify_groups'", {
      div(align='left', style="margin-left:20px", width='400px',
          column(width=12, p('Hier kun je andere topics onder het geselecteerde topic groeperen. De topics zijn dan niet meer zichtbaar, en de resultaten worden bij het bovenliggende topic opgeteld')),
          column(width=12, span(textOutput('in_group', ), style="color: green")),
          br(),
          div(align='center', style = "height: 800px",
            shinyWidgets::multiInput('sb_group_topic', 'Groepeer onder dit topic',choices = list(), width = '350px'),
            uiOutput('save_groups_button')
          )
          
      )
    }),
    conditionalPanel("input.sidebar_tab == 'modify_queries'", {
      div(align='center',
        h2('Zoektermen', align='center'),
        p('Hier kun je zoektermen bekijken en eventueel aanpassen.', br(), 
          'Aanpassingen worden niet opgeslagen'),
        textAreaInput('queries', height='300px', label = "", value='', placeholder = 'label# zoekterm AND zoekterm ...')
      )
    })
  )
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
