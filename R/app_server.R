#' @import shiny
#' @import shinyWidgets
#' @import magrittr
#' @import data.table
app_server <- function(input, output,session) {
  ## get tcorpus (tc) and stm model (m) as global data, as loaded into memory in run_topicbrowser
  ## This way, there should not be multiple copies of the data for concurrent users.
  data = golem::get_golem_options()
  if (data$token_auth) observe(validate_token(session))
  set_widget_defaults(session, data)                                
  
  query_txt = debounce(reactive(input$queries), 500)
  observe(update_query_widgets(session, input, query_txt()))
  
  ## topic name management
  topic_names = reactiveFileReader(session=session, intervalMillis = 1000, filePath = data$topic_names_file, readFunc = readRDS)
  observe(update_sidebar_topic_names(session, input, topic_names()))
  topic_colors = reactive(create_topic_colors(topic_names()))
  observeEvent(input$sb_select_topic, updateSearchInput(session, 'sb_rename_topic', value=''))
  observeEvent(input$sb_rename_topic_search, save_topic_names(session, input, data, topic_names()))
  observe(set_topic_names(session, input, data, topic_names()))        ## observe to be updated
  
  
  observeEvent(input$topic_filter, {
    output$wordcloud = renderPlot(create_wordcloud(input$topic_filter, data, topic_names(), topic_colors()))
  }, ignoreInit=T)
  
  

  topic_scores = reactive(prepare_topic_scores(data))
  meta_table = reactive(create_meta_table(input, data, topic_scores()))
  
  graph_data = reactive(create_graph_data(input, meta_table()))
  output$dategraph = dygraphs::renderDygraph(create_graph(input, data, graph_data(), topic_names(), topic_colors()))
  
  articlelist_data = reactive(create_articlelist_data(input, data, meta_table(), daterange()))
  
  output$articlelist = DT::renderDataTable(create_articlelist(data, articlelist_data(), topic_colors()))
  
  observeEvent(articlelist_data(), {
    output$articles = renderText(create_articles(input, data, articlelist_data(), topic_names(), topic_colors()))
  }, ignoreInit=T)
  
  daterange <- debounce(reactive({
    dr = input$dategraph_date_window
  }), 500)
  
  observeEvent(daterange(), {
    p = is_datepreset(graph_data(), as.Date(daterange()))
    updateSelectInput(session, 'dateselect', selected=p)
  })
}


