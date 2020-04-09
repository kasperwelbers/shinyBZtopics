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
  
  ## topic data management
  topic_names = reactiveFileReader(session=session, intervalMillis = 1000, filePath = data$topic_names_file, readFunc = readRDS)
  observe(update_sidebar_topic_names(session, input, topic_names()))
  
  topic_colors = reactiveFileReader(session=session, intervalMillis = 1000, filePath = data$topic_colors_file, readFunc = readRDS)
  topic_groups = reactiveFileReader(session=session, intervalMillis = 1000, filePath = data$topic_groups_file, readFunc = readRDS)
  
  observeEvent(input$sb_select_topic, on_select_topic(session, output, input, data, topic_names=topic_names(), topic_colors=topic_colors(), topic_groups=topic_groups()))
  observeEvent(input$sb_rename_topic_search, save_topic_names(session, input, data, topic_names()))
  observeEvent(input$sb_color_topic, save_topic_color(session, input, data, topic_colors()))
  observeEvent(input$sb_group_topic, ignoreNULL = F, {
    output$save_groups_button = renderUI(actionButton('sb_group_topic_save', 'Bevestig groepering', color='green'))
  })
  observeEvent(input$sb_group_topic_save, save_topic_groups(session, output, input, data, topic_names(), topic_groups()))
  observe(set_topic_names(session, input, data, topic_names()))        ## observe to be updated
  

  topic_filter <- debounce(reactive({input$topic_filter}), millis = 500)

  
  ## wordcloud  
  topwords_matrix = reactive({create_topwords_matrix(data, topic_groups())})
  topwords = reactive({create_topwords(data, topwords_matrix())})
  observeEvent(topic_filter(), {
    output$wordcloud = renderPlot(create_wordcloud(topic_filter(), data, topic_names(), topic_colors(), topwords(), topwords_matrix()))
  }, ignoreInit=T)

  
  
  topic_scores = reactive(prepare_topic_scores(data, topic_groups()))
  meta_table = reactive(create_meta_table(input, data, topic_filter(), topic_scores()))
  
  graph_data = reactive(create_graph_data(input, meta_table()))
  graph = reactive(create_graph(input, data, graph_data(), topic_filter(), topic_names(), topic_colors()))
  output$dategraph = dygraphs::renderDygraph(graph())
  
  articlelist_data = reactive(create_articlelist_data(input, data, topic_filter(), meta_table(), daterange()))
  
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
  
  output$save_graph = downloadHandler(function() 'topicbrowser_timegraph.png', 
                                      function(file) {
                                        fn = save_graph(session, graph())
                                        file.copy(fn, file)
                                      },
                                      contentType = "image/png")
  
  output$save_data = downloadHandler(function() 'topicbrowser_timegraph_data.csv', 
                                      function(file) {
                                        d = graph_data()
                                        selected_topics_i = as.numeric(gsub('topic_','',colnames(d)[-1]))
                                        colnames(d) = c('date', topic_names()[selected_topics_i])
                                        write.csv(d, file, row.names=F)
                                      },
                                      contentType = "text/csv")
  
  
}

