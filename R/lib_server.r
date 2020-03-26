validate_token <- function(session) {
  tokens = readLines('.valid_tokens')
  query <- parseQueryString(session$clientData$url_search)
  if (!query$token %in% tokens) stop("Close session due to invalid token")
}

prepare_topic_scores <- function(data) {
  print('neeees')
  meta_cols = c('doc_id','medium','headline','date')
  topic_scores = data.table::as.data.table(data$m$theta)
  colnames(topic_scores) = paste0('topic_', 1:ncol(topic_scores))
  topic_scores = cbind(data$tc$get_meta(columns = meta_cols), topic_scores)
  data.table::setindexv(topic_scores, c('medium','date'))
}

create_meta_table <- function(input, data, topic_scores) {
  if (length(input$topic_filter) == 0) return(NULL)
  print('meta update')
  
  meta_cols = c('doc_id','medium','headline','date')
  meta_table = subset(topic_scores, select = colnames(topic_scores) %in% c(meta_cols, input$topic_filter))
  
  if (length(input$media_filter) > 0)
    meta_table = meta_table[list(medium=input$media_filter),,on='medium']
  
  ## if multiple topics are selected, documents with lowest max are mixture topics
  if (length(input$topic_filter) == 1)
    meta_table$topic_mix = 1 - meta_table[,5]
  else {
    meta_table$topic_mix = apply(meta_table[,5:ncol(meta_table)], 1, mixture_score)
  }
  
  meta_table
}

mixture_score <- function(x) {
  ## given the topic proportions for a selection of topics, rank documents on:
  ## 1. being most defined by the selection of topics (i.e. the sum of proportions) 
  ## 2. in case of multiple topics, prioritize mixtures
  exp = 1/length(x)
  err = exp - x
  err[err < 0] = 0   ## ignore negative error (where proportion of topic is higher than expected)
  sum(err)
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
  subset(agg, select = !colnames(agg) == 'topic_mix')
}

create_graph <- function(input, data, graph_data, topic_names, topic_colors) {
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
  
  col = if (length(input$topic_filter) > 0) as.character(unlist(topic_colors[input$topic_filter])) else as.character(unlist(topic_colors))
  col = substr(col, start = 0, stop = 7)
  
  selected_topics = colnames(graph_data)
  selected_topics_i = as.numeric(gsub('topic_','',selected_topics))
  colnames(graph_data) = topic_names[selected_topics_i]
  
  dygraphs::dygraph(graph_data, main = 'Aandacht voor topics over tijd') %>%
    dygraphs::dyRangeSelector(retainDateWindow=F, height=30, dateWindow = datewindow,
                              fillColor = " #A7B1C4", strokeColor = "#808FAB", keepMouseZoom = T) %>%
    dygraphs::dyUnzoom() %>%
    dygraphs::dyLegend(show='onmouseover', showZeroValues = F, labelsSeparateLines = T) %>%
    dygraphs::dyOptions(useDataTimezone = T, colors=col)
  
}

set_widget_defaults <- function(session, data) {
  mindate = min(as.Date(data$tc$meta$date))
  maxdate = max(as.Date(data$tc$meta$date))
  updateDateRangeInput(session, 'daterange', start= mindate, end=maxdate)
  
  media = sort(unique(data$tc$meta$medium))
  shinyWidgets::updatePickerInput(session, inputId = 'media_filter', choices = as.list(media))
}

set_topic_names <- function(session, input, data, topic_names) {
  current_selection = input$topic_filter
  l = as.list(paste0('topic_', 1:length(topic_names)))
  names(l) = topic_names
  l = l[data$topic_selection]
  shinyWidgets::updatePickerInput(session, inputId = 'topic_filter', choices = l, selected=current_selection)
}

rename_topic <- function(session, input, data, topic_names) {
  selected_topic = input$sb_select_topic
  selected_topic_i = as.numeric(gsub('topic_','',selected_topic))
  topic_names[selected_topic_i] = input$sb_rename_topic
  saveRDS(topic_names, file=data$topic_names_file)
}

update_sidebar_topic_names <- function(session, input, topic_names) {
  l = as.list(paste0('topic_', 1:length(topic_names)))
  names(l) = topic_names
  
  selected_topic = input$sb_select_topic
  selected_topic_i = as.numeric(gsub('topic_','',selected_topic))
  
  updateSelectizeInput(session, 'sb_select_topic', choices=l, selected=l[[selected_topic_i]])
}

#sidebar_topic_names <- function(session, input, data) {
#  ntopics = ncol(data$m$theta)
#  topic_names = character(ntopics)
#  for (i in 1:ntopics) {
#    id = paste0('topic_', i)
#    topic_names[i] = input[[id]]
#  }
#  topic_names
#}

create_topic_colors <- function(topics) {
  #topics = sort(topic_names)
  colors = as.list(rainbow(length(topics)))
  names(colors) = paste0('topic_', 1:length(topics))
  colors
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
  
  m = subset(meta_table, select = c('doc_id','medium','date','headline', input$topic_filter, 'topic_mix'))
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
    m = m[order(m$topic_mix),]
    subset(m, select=c('doc_id','medium','date','headline', input$topic_filter))
  }
}


create_articlelist <- function(data, articlelist_data, topic_colors) {
  if (is.null(articlelist_data)) return(NULL)
  
  js <- "function(data, type, full){ return '<span class=spark>' + data + '</span>' }"
  colDefs <- list(list(targets = 4, render = DT::JS(js)))
  
  topic_colors = topic_colors[colnames(articlelist_data)[5:ncol(articlelist_data)]]
  bar_string <- sprintf("type: 'bar', height: '40px', colorMap: [%s], highlightColor: 'black'", paste(paste0("'", topic_colors, "'"), collapse=','))
  cb_bar <- DT::JS(paste0("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { ", 
                          bar_string, " }); }"), collapse = "")
  
  d = articlelist_data[,-1]  ## drop doc_id
  sparkline = as.character(apply(round(d[,4:ncol(d)], 3), 1, paste, collapse=','))   
  d = cbind(d[,1:3], topic=sparkline)
  DT::datatable(d, options=list(dom = 'tp', stateSave=T, columnDefs = colDefs, fnDrawCallback = cb_bar), selection='single')
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

create_articles <- function(input, data, articlelist, topic_names, topic_colors) {
  if (is.null(articlelist)) return(NULL)
  if (is.null(input$articlelist_rows_selected)) return(NULL)
  ## option 1: get article index for table pagination
  #pag_start = input$articlelist_state$start
  #pag_end = pag_start + input$articlelist_state$length
  #art_i = (pag_start+1):pag_end
  #art_id = articlelist$doc_id[art_i]
  
  ## option 2: get selected article
  
  print(input$articlelist_rows_selected)
  art_id = articlelist$doc_id[input$articlelist_rows_selected]
 
  tokens = data$tc$get(doc_id = art_id)
  #tokens$topic = as.character(topic_names[tokens$topic])
  tokens$topic = ifelse(is.na(tokens$topic), NA, paste0('topic_', tokens$topic))
  #token_label = na.omit(unique(tokens$topic))
  tokens$topic = factor(tokens$topic, na.omit(unique(tokens$topic)))
  meta = data$tc$get_meta(columns = c('doc_id','medium','headline','date'), doc_id = art_id)
  
  #topic_id = paste0('topic_', as.numeric(tokens$topic[!is.na(tokens$topic)]))
  col = as.character(unlist(topic_colors[levels(tokens$topic)]))
  col = substr(col, start = 0, stop = 7)
  
  ## switch topic_ids with topic_labels
  topic_i = as.numeric(gsub('topic_','',levels(tokens$topic)))
  levels(tokens$topic) = topic_names[topic_i]
  
  
  topicscore = ifelse(is.na(tokens$topicscore), 0, tokens$topicscore)
  topicscore = tokenbrowser::rescale_var(topicscore, new_min = 0.1, new_max=0.6)
  tokens$token = tokenbrowser::category_highlight_tokens(tokens$token, tokens$topic, alpha=topicscore, span_adjacent = T, colors = col)
  if ('space' %in% colnames(tokens)) tokens$token = paste(tokens$token, tokens$space, sep='')
  doc = tokenbrowser::wrap_documents(tokens, meta)
  
  #doc = gsub('<doc_id>.*</doc_id>', '<doc_id></doc_id>', doc)
  doc = gsub('(<br>) *(<br>)((<br>)| )+', '<br><br>', doc)
  doc = gsub('\\s', '', doc, fixed=T)
  doc
}

create_wordcloud <- function(topic_filter, data, topic_names, topic_colors) {
  if (length(topic_filter) > 0) {
    n = 15
    plot_topicwords(data, n, topics = topic_filter, topic_colors = topic_colors, topic_names=topic_names)
  } else NULL
}



plot_topicwords <- function(data, n, topics, topic_colors, topic_names) {
  selected_topics_i = as.numeric(gsub('topic_','',topics))
  
  words = data$frex_terms[selected_topics_i,1:n]
  words = data.frame(word = as.vector(t(words)),
                     topic = rep(selected_topics_i, each=n))
  
  
  vocab_i = match(words$word, data$m$vocab)
  wordmat = data$frex_matrix[vocab_i,selected_topics_i,drop=F]
  
  col = as.character(unlist(topic_colors[topics]))
  
  rownames(wordmat) = words$word
  colnames(wordmat) = topic_names[selected_topics_i]
  
  par(mar=c(0,0,0,0))
  if (length(topics) == 1)
    wordcloud::wordcloud(words=rownames(wordmat), scale = c(2,0.3), freq = wordmat[,1], colors=col)
  else
    wordcloud::comparison.cloud(wordmat, scale = c(2,0.3), colors=col, title.size = 1, )
}
