#' @import shiny
#' @import shinyWidgets
#' @import magrittr
#' @import data.table
app_server <- function(input, output,session) {
  ## get tcorpus (tc) and stm model (m) as global data, as loaded into memory in run_topicbrowser
  ## This way, there should not be multiple copies of the data for concurrent users.
  data = golem::get_golem_options()
  
  if (data$token_auth) observe(validate_token(session))
  
  data$topic_colors = create_topic_colors(data$topic_names)
  set_widget_defaults(session, output, data)

  meta_table = reactive(create_meta_table(input, data))
  graph_data = reactive(create_graph_data(input, meta_table()))
  
  output$dategraph = dygraphs::renderDygraph(create_graph(input, data, graph_data()))
  
  articlelist_data = reactive(create_articlelist_data(input, data, meta_table(), daterange()))
  
  output$articlelist = DT::renderDataTable(create_articlelist(articlelist_data()), 
                                           options=list(dom = 'tp', stateSave=T), rownames=F, selection='single')
  
  observeEvent(articlelist_data(), {
    output$articles = renderText(create_articles(input, data, articlelist_data()))
  }, ignoreInit=T)
  
  daterange <- debounce(reactive({
    dr = input$dategraph_date_window
  }), 500)
  
  observeEvent(daterange(), {
    p = is_datepreset(graph_data(), as.Date(daterange()))
    updateSelectInput(session, 'dateselect', selected=p)
  })
}
