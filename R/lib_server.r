validate_token <- function(session) {
  tokens = readLines('.valid_tokens')
  query <- parseQueryString(session$clientData$url_search)
  if (!query$token %in% tokens) stop("Close session due to invalid token")
}

create_meta_table <- function(input, data) {
  if (length(input$topic_filter) == 0) return(NULL)
  print('meta update')
  ## m$theta is the document X topic matrix, with rows representing the multinomial distribution
  topic_scores = data.table::as.data.table(data$m$theta)
  colnames(topic_scores) = data$topic_names
  
  if (length(input$topic_filter) > 0) 
    topic_scores = subset(topic_scores, select = colnames(topic_scores) %in% input$topic_filter)
    
  meta_table = cbind(data$tc$get_meta(columns = c('doc_id','medium','headline','date')), topic_scores)
  
  ## Later on, sort article list by documents that score highest on topic selection
  meta_table$topic_sum = rowSums(topic_scores)
  
  if (length(input$media_filter) > 0) 
    meta_table = subset(meta_table, meta_table$medium %in% input$media_filter)

  meta_table
}

create_graph_data <- function(input, meta_table) {
  if (is.null(meta_table)) return(NULL)
  print('graph data update')
  graph_data = subset(meta_table, select = !colnames(meta_table) %in% c('date','headline','doc_id','medium'))
  if (input$aggregate == 'day') graph_data$agg_date = as.Date(meta_table$date)
  if (input$aggregate == 'week') graph_data$agg_date = as.Date(cut(meta_table$date, breaks = 'weeks'))
  if (input$aggregate == 'month') graph_data$agg_date = as.Date(cut(meta_table$date, breaks = 'months'))
  
  graph_data = as.data.table(graph_data)
  agg = graph_data[, lapply(.SD, sum), by='agg_date']
  
  data.table::setorderv(agg, 'agg_date')
  agg[is.na(agg)] = 0
  agg
}


set_widget_defaults <- function(session, output, data) {
  mindate = min(as.Date(data$tc$meta$date))
  maxdate = max(as.Date(data$tc$meta$date))
  updateDateRangeInput(session, 'daterange', start= mindate, end=maxdate)
  
  media = sort(unique(data$tc$meta$medium))
  shinyWidgets::updatePickerInput(session, inputId = 'media_filter', choices = as.list(media))
  
  shinyWidgets::updatePickerInput(session, inputId = 'topic_filter', choices = as.list(data$topic_names))
  
}

create_topic_colors <- function(topic_names) {
  topics = sort(topic_names)
  colors = as.list(rainbow(length(topics)))
  names(colors) = topics
  colors
}

create_graph <- function(input, data, graph_data) {
  if (is.null(graph_data)) return(NULL)
  print('create graph')
  if (input$dateselect != 'vrij') {
    mindate = as.Date(graph_data$agg_date[1])
    enddate = as.Date(graph_data$agg_date[nrow(graph_data)])
    if (input$dateselect == 'week') startdate = enddate - 7
    if (input$dateselect == 'maand') startdate = enddate - 30
    if (input$dateselect == 'jaar') startdate = enddate - 365
    if (input$dateselect == 'alles') startdate = mindate
    if (startdate < mindate) startdate = mindate
    datewindow = c(startdate, enddate)
  } else {
    isolate(
      if (!is.null(input$dategraph_date_window)) datewindow = as.Date(input$dategraph_date_window)
    )
  }
  
  col = if (length(input$topic_filter) > 0) as.character(unlist(data$topic_colors[input$topic_filter])) else as.character(unlist(data$topic_colors))
  col = substr(col, start = 0, stop = 7)
  

  dygraphs::dygraph(graph_data, main = 'Aandacht voor topics over tijd') %>%
      dygraphs::dyRangeSelector(retainDateWindow=F, height=30, dateWindow = datewindow,
                                fillColor = " #A7B1C4", strokeColor = "#808FAB", keepMouseZoom = T) %>%
      #dygraphs::dyStackedBarChart() %>%
      dygraphs::dyUnzoom() %>%
      dygraphs::dyLegend(show='onmouseover', showZeroValues = F, labelsSeparateLines = T) %>%
      dygraphs::dyOptions(useDataTimezone = T, colors=col)

}



is_datepreset <- function(graph_data, datewindow) {
  mindate = as.Date(graph_data$agg_date[1])
  enddate = as.Date(graph_data$agg_date[nrow(graph_data)])
  
  if (datewindow[2] == enddate) {
    if (datewindow[1] == enddate - 7) return('week')
    if (datewindow[1] == enddate - 30) return('maand')
    if (datewindow[1] == enddate - 365) return('jaar')
    if (datewindow[1] == mindate) return('alles')
  }
  return('vrij')
}



create_articlelist_data <- function(input, data, meta_table, daterange) {
  if (is.null(meta_table)) return(NULL)
  
  m = subset(meta_table, select = c('doc_id','medium','date','headline','topic_sum'))
  m$date = as.Date(m$date)
  
  if (!is.null(daterange)) {
    daterange = as.Date(daterange)
    if (input$aggregate == 'week') daterange[2] = daterange[2] + 6
    if (input$aggregate == 'month') daterange[2] = month_ceiling(daterange[2])
    date_seq = seq.Date(daterange[1], daterange[2], by = 1)
    date_seq = rev(date_seq)   ## lastest date first
    m = m[list(date=date_seq),on='date', nomatch=0]
  }

  if (nrow(m) == 0) NULL else {
    m = m[order(-m$topic_sum),]
    subset(m, select=c('doc_id','medium','date','headline'))
  }
}

create_articlelist <- function(articlelist_data) {
  if (is.null(articlelist_data)) return(NULL)
  subset(articlelist_data, select=c('date','medium','headline'))
}

month_ceiling <- function(x) {
  y = data.table::year(x)
  m = data.table::month(x)
  if (m == 12) {
    y = y + 1
    m = 1
  } else 
    m = m + 1
  next_month = as.Date(sprintf('%s-%s-01', y, m))
  next_month - 1
}

create_articles <- function(input, data, articlelist) {
  if (is.null(articlelist)) return(NULL)
  
  ## get article index for table pagination
  pag_start = input$articlelist_state$start
  pag_end = pag_start + input$articlelist_state$length
  art_i = (pag_start+1):pag_end
  art_id = articlelist$doc_id[art_i]
  
  tokens = tc$get(doc_id = art_id)
  tokens$topic = as.character(data$topic_names[tokens$topic])
  tokens$topic = factor(tokens$topic, na.omit(unique(tokens$topic)))
  meta = tc$get_meta(columns = c('doc_id','medium','headline','date'), doc_id = art_id)
  
  
  
  col = as.character(unlist(data$topic_colors[levels(tokens$topic)]))
  col = substr(col, start = 0, stop = 7)
  
  tokens$token = tokenbrowser::category_highlight_tokens(tokens$token, tokens$topic, alpha=0.3, span_adjacent = T, colors = col)
  if ('space' %in% colnames(tokens)) tokens$token = paste(tokens$token, tokens$space, sep='')
  doc = tokenbrowser::wrap_documents(tokens, meta)
  
  #doc = gsub('<doc_id>.*</doc_id>', '<doc_id></doc_id>', doc)
  doc = gsub('(<br>) *(<br>)((<br>)| )+', '<br><br>', doc)
  doc = gsub('\\s', '', doc, fixed=T)
  doc
}
