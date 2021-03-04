validate_token <- function(session) {
  tokens = readLines('.valid_tokens')
  qs = paste0(session$clientData$url_search, session$clientData$url_hash)
  query <- parseQueryString(qs)
  if (!query$token %in% tokens) stop("Close session due to invalid token")
}

prepare_topic_scores <- function(data, topic_groups) {
  print('topic scores update')
  meta_cols = c('doc_id','medium','headline','date')
  topic_scores = data$m$theta
  colnames(topic_scores) = paste0('topic_', 1:ncol(topic_scores))
  topic_scores = group_matrix(topic_scores, topic_groups)
  topic_scores = data.table::as.data.table(topic_scores)
  topic_scores = cbind(data$tc$get_meta(columns = meta_cols), topic_scores)
  data.table::setindexv(topic_scores, c('medium','date'))
}

get_children <- function(tg, parents, done=parents) {
  .parents = parents
  children = unique(tg[list(parent=.parents),,on='parent',nomatch=0]$child)
  children = setdiff(children, done)
  done = union(parents,children)
  if (length(children) == 0) return(numeric())
  union(children, get_children(tg, children, done=done))
}

group_matrix <- function(m, topic_groups, only_return_top=F) {
  for (parent in unique(topic_groups$parent)) {
    children = get_children(topic_groups, parent)
    if (length(children) > 0) {
      topcols = 
      m[,parent] = m[,parent] + Matrix::rowSums(m[,children,drop=F])   
    }
  }
  if (only_return_top) {
    own_parent = unique(topic_groups$parent[topic_groups$parent == topic_groups$child])
    m = m[,own_parent]
  }
  m
}

create_meta_table <- function(session, input, data, topic_filter, topic_scores) {
  if (length(topic_filter) == 0) return(NULL)
  print('meta update')
  
  meta_cols = c('doc_id','medium','headline','date')
  meta_table = subset(topic_scores, select = colnames(topic_scores) %in% c(meta_cols, topic_filter))
  
  if (length(input$media_filter) > 0)
    meta_table = meta_table[list(medium=input$media_filter),,on='medium']
  

  if (length(input$query_filter) > 0) {
    hits = tryCatch(corpustools::search_contexts(data$tc, input$query_filter),
             error = function(e) NULL)
    if (is.null(hits)) hits = drop_bad_queries(session, input, data)
    if (is.null(hits)) return(NULL)
    .doc_ids = unique(hits$hits$doc_id)
    meta_table = meta_table[list(doc_id=.doc_ids),,on='doc_id']
  }
  
  if (nrow(meta_table) == 0) return(NULL)
  
  
  meta_table
}


drop_bad_queries <- function(session, input, data) {
  n = length(input$query_filter)
  is_bad = vector(length=n)
  for (i in 1:n) {
    q = input$query_filter[i]
    hits = tryCatch(corpustools::search_contexts(data$tc, q), error = function(e) NULL)
    if (is.null(hits)) is_bad[i] = T
  }
  if (any(is_bad)) {
    bad_queries = input$query_filter[is_bad]
    bad_queries = gsub('\\# .*', '', bad_queries)
    print('alert?')
    shinyalert::shinyalert('Incorrecte zoektermen', sprintf('De volgende zoektermen zijn incorrect gespecificeerd:\n\n%s\n\nDeze termen zijn nu gedeactiveerd in de zoektermen filter.', paste(bad_queries, collapse='\n')))
    shinyWidgets::updatePickerInput(session, 'query_filter', selected= input$query_filter[!is_bad])
    if (all(is_bad)) return(NULL)
  }
  corpustools::search_contexts(data$tc, input$query_filter[!is_bad])
}



create_graph_data <- function(input, meta_table) {
  if (is.null(meta_table)) {
    return(NULL)
  }
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

create_graph <- function(input, data, graph_data, topic_filter, topic_names, topic_colors) {
  if (is.null(graph_data)) {
    return(NULL)
  }
  
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
  
  color = if (length(topic_filter) > 0) as.character(unlist(topic_colors[topic_filter])) else as.character(unlist(topic_colors))
  color = substr(color, start = 0, stop = 7)

  selected_topics = colnames(graph_data)[-1]
  selected_topics_i = as.numeric(gsub('topic_','',selected_topics))
  colnames(graph_data)[-1] = topic_names[selected_topics_i]
  
  dygraphs::dygraph(graph_data, main = 'Aandacht voor topics over tijd') %>%
    dygraphs::dyRangeSelector(retainDateWindow=F, height=30, dateWindow = datewindow,
                              fillColor = " #A7B1C4", strokeColor = "#808FAB", keepMouseZoom = T) %>%
    dygraphs::dyUnzoom() %>%
    dygraphs::dyLegend(labelsDiv = 'legend', show='always', showZeroValues = F, labelsSeparateLines = T) %>%
    dygraphs::dyOptions(useDataTimezone = T, colors=color)
  
}

parse_url_queries <- function(session, output) {
  qs = isolate(paste0(session$clientData$url_search, session$clientData$url_hash))
  query <- parseQueryString(qs)
  queries = if (is.null(query$amcat_queries)) NULL else utils::URLdecode(query$amcat_queries)
  #output$sb_imported_queries = renderText(queries)
  queries
}

set_widget_defaults <- function(session, data) {
  mindate = min(as.Date(data$tc$meta$date))
  maxdate = max(as.Date(data$tc$meta$date))
  updateDateRangeInput(session, 'daterange', start= mindate, end=maxdate)
  
  media = sort(unique(data$tc$meta$medium))
  shinyWidgets::updatePickerInput(session, inputId = 'media_filter', choices = as.list(media))
}

set_topic_names <- function(session, input, data, topic_names, topic_groups) {
  isolate({current_selection = input$topic_filter})
  
  valid_choices = unique(topic_groups$parent)
  valid_choices = intersect(valid_choices, paste('topic', data$topic_selection, sep='_'))
  l = as.list(valid_choices)
  names(l) = topic_names[as.numeric(gsub('topic\\_','',valid_choices))]
  #l = as.list(paste0('topic_', 1:length(topic_names)))
  #names(l) = topic_names
  
  new_selection = intersect(current_selection, valid_choices)
  
  shinyWidgets::updatePickerInput(session, inputId = 'topic_filter', choices = l, selected=new_selection)
}

save_topic_names <- function(session, input, data, topic_names) {
  selected_topic = input$sb_select_topic
  selected_topic_i = as.numeric(gsub('topic_','',selected_topic))
  topic_names[selected_topic_i] = input$sb_rename_topic
  saveRDS(topic_names, file=data$topic_names_file)
}

save_topic_color <- function(session, input, data, topic_colors) {
  selected_topic = input$sb_select_topic
  selected_topic_i = as.numeric(gsub('topic_','',selected_topic))
  topic_colors[selected_topic_i] = input$sb_color_topic
  saveRDS(topic_colors, file=data$topic_colors_file)
}

save_topic_groups <- function(session, output, input, data, topic_names, topic_groups) {
  .children = input$sb_group_topic
  .rm_children = topic_groups[list(parent = input$sb_select_topic),,on='parent']$child
  .rm_children = setdiff(.rm_children, .children)
  
  if (length(.children) > 0)
    topic_groups[list(child = .children), parent := input$sb_select_topic, on='child']
  if (length(.rm_children) > 0)
    topic_groups[list(child = .rm_children), parent := child, on='child']
  
  valid_choices = unique(topic_groups$parent)
  choices = as.list(valid_choices)
  names(choices) = topic_names[as.numeric(gsub('topic\\_','',valid_choices))]
  new_selection = intersect(input$topic_filter, valid_choices)
  shinyWidgets::updatePickerInput(session, inputId = 'topic_filter', choices = choices, selected=new_selection)
  
  output$save_groups_button = renderUI(NULL)
  saveRDS(topic_groups, file=data$topic_groups_file)
}

save_queries <- function(session, output, input, data) {
  queries = input$sb_queries
  saveRDS(queries, file=data$queries_file)
  output$save_queries_button = renderUI(NULL)
}

on_select_topic <- function(session, output, input, data, topic_names, topic_colors, topic_groups) {
  selected_topic = input$sb_select_topic
  selected_topic_i = as.numeric(gsub('topic_','',selected_topic))
 
  output$sb_top_words = renderText(paste(data$top_terms$frex[selected_topic_i,], collapse=', '))
  #output$sb_top_words_lift = renderText(paste(head(data$top_terms$lift[,selected_topic_i], 10), collapse=', '))
  updateSearchInput(session, 'sb_rename_topic', value='')
  colourpicker::updateColourInput(session, 'sb_color_topic', value=topic_colors[[selected_topic]])
  
  parent = topic_groups[list(child = selected_topic),,on='child']$parent
  if (parent == selected_topic)
    output$in_group = renderText('')
  else {
    parent_i = as.numeric(gsub('topic\\_','',parent))
    output$in_group = renderText(sprintf('Let op: Het topic "%s" is zelf al gegroepeerd onder topic "%s". Alle hier geselecteerde topics zullen daar ook onder gegroepeerd worden', topic_names[selected_topic_i], topic_names[parent_i]))
  }
  
  valid_choices = topic_groups$child[!topic_groups$child %in% c(selected_topic,parent)]
  choices = as.list(valid_choices)
  names(choices) = topic_names[as.numeric(gsub('topic\\_','',unlist(choices)))]
  selected = topic_groups[list(parent = selected_topic),,on='parent', nomatch=0]$child
  shinyWidgets::updateMultiInput(session, 'sb_group_topic', choices=choices, selected=selected)
}

update_sidebar_topic_names <- function(session, input, topic_names) {
  l = as.list(paste0('topic_', 1:length(topic_names)))
  names(l) = topic_names
  
  selected_topic = input$sb_select_topic
  selected_topic_i = as.numeric(gsub('topic_','',selected_topic))
  
  updateSelectizeInput(session, 'sb_select_topic', choices=l, selected=l[[selected_topic_i]])
}


update_query_widgets <- function(session, input, imported_queries, queries) {
  updateTextAreaInput(session, 'sb_queries', value = queries)
  queries = rbind(parse_queries(imported_queries),
                  parse_queries(queries))
  dup_i = match(queries$code, unique(queries$code))

  if (nrow(queries) > 0) {
    queries$code = as.character(queries$code)
    queries$code = stringi::stri_trim(queries$code)
    queries$code = unique_label(queries$code)
  }
  
  l = as.list(paste(queries$code, queries$query, sep='# '))
  names(l) = queries$code
  current_selection = gsub('# .*', '', input$query_filter) %in% queries$code
  current_selection = input$query_filter[current_selection]
  shinyWidgets::updatePickerInput(session, inputId = 'query_filter', choices = l, selected=current_selection)
}


unique_label <- function(x) {
  x = as.character(x)
  dup_str = paste0(x, '#', 1)
  i = 2
  while(TRUE){
    dupl = duplicated(dup_str)
    if (any(dupl)) {
      dup_str[dupl] = paste0(dup_str[dupl], '#', i)
      i = i + 1
    } else break
  }
  dup_i = as.numeric(gsub('.*#','',dup_str))
  ifelse(dup_i > 1, sprintf('%s (%s)', x, dup_i), x)
}


parse_queries <- function(query_txt) {
  if (is.null(query_txt)) return(NULL)
  queries_txt = strsplit(query_txt, '\n')[[1]]
  queries_txt = queries_txt[grepl('#', queries_txt)]
  data.frame(code = gsub('#.*','', queries_txt),
             query = gsub('.*# *', '', queries_txt))
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

create_topic_colors <- function(k) {
  ## default topic colors, only made at first run
  colors = as.list(rainbow(k))
  names(colors) = paste0('topic_', 1:k)
  colors
}

create_topic_groups <- function(k) {
  ## default topic merges (that is, no mergers), only made at first run
  data.table::data.table(parent = paste0('topic_', 1:k),
                         child = paste0('topic_', 1:k))
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



create_articlelist_data <- function(input, data, topic_filter, meta_table, daterange) {
  if (is.null(meta_table)) return(NULL)
  
  m = subset(meta_table, select = c('doc_id','medium','date','headline', topic_filter))
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
    ## if multiple topics are selected, documents with lowest max are mixture topics
    if (length(topic_filter) == 1)
      m$topic_mix = m[,5]
    else {
      m$topic_mix = apply(m[,5:ncol(m)], 1, mixture_score)
    }
    m = m[order(-m$topic_mix),]
    subset(m, select=c('doc_id','medium','date','headline', topic_filter))
  }
}



mixture_score <- function(x) {
  ## given the topic proportions for a selection of topics, rank documents on:
  ## 1. being most defined by the selection of topics (i.e. the sum of proportions) 
  ## 2. in case of multiple topics, prioritize mixtures
  sum(x) - sd(x)
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
  if (ncol(d) == 4)
    sparkline = paste0(d[[4]],',0')
  else
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
  tokens$token = tokenbrowser::category_highlight_tokens(tokens$token, tokens$topic, alpha=topicscore, span_adjacent = T, colors = col, doc_id = tokens$doc_id)
  if ('space' %in% colnames(tokens)) tokens$token = paste(tokens$token, tokens$space, sep='')
  doc = tokenbrowser::wrap_documents(tokens, meta)
  
  #doc = gsub('<doc_id>.*</doc_id>', '<doc_id></doc_id>', doc)
  doc = gsub('(<br>) *(<br>)((<br>)| )+', '<br><br>', doc)
  doc = gsub('\\s', '', doc, fixed=T)
  doc
}

create_topwords_matrix <- function(data, topic_groups) {
  logbeta = data$m$beta$logbeta[[1]]
  logbeta = t(logbeta)
  colnames(logbeta) = paste0('topic_', 1:ncol(logbeta))
  logbeta = group_matrix(logbeta, topic_groups, only_return_top = T)
  exp(logbeta)
}

create_topwords <- function(data, topwords_matrix) {
  max_n=50
  #apply(topwords_matrix, 2, function(x) data$m$vocab[order(-x)[1:max_n]])
  wordcounts <- data$m$settings$dim$wcounts$x
  vocab = data$m$vocab
  frexlabels <- stm::calcfrex(log(t(topwords_matrix)), 0.5, wordcounts)
  apply(frexlabels, 2, function(x) vocab[head(x, max_n)])
}

create_wordcloud <- function(topic_filter, data, topic_names, topic_colors, top_words, top_words_matrix) {
  if (length(topic_filter) > 0) {
    n = max(10, 50 / length(topic_filter) )
    plot_topicwords(data, n, topics = topic_filter, topic_colors = topic_colors, topic_names=topic_names, top_words, top_words_matrix)
  } else NULL
}

plot_topicwords <- function(data, n, topics, topic_colors, topic_names, top_words, top_words_matrix) {
  selected_topics_i = as.numeric(gsub('topic_','',topics))
  
  words = top_words[1:n, selected_topics_i]
  words = data.frame(word = as.vector(t(words)),
                     topic = rep(selected_topics_i, each=n))
  words = words[!duplicated(words$word),]

  vocab_i = match(words$word, data$m$vocab)
  

  wordmat = top_words_matrix[vocab_i,paste("topic", selected_topics_i, sep='_'),drop=F]

  col = as.character(unlist(topic_colors[topics]))
  
  rownames(wordmat) = words$word
  colnames(wordmat) = topic_names[selected_topics_i]
  
  par(mar=c(0,0,0,0))
  set.seed(1)
  if (length(topics) == 1)
    wordcloud::wordcloud(words=rownames(wordmat), scale = c(2,0.7), freq = wordmat[,1], colors=col)
  else
    wordcloud::comparison.cloud(wordmat, scale = c(2,0.7), colors=col, title.size = 0.001)
}

save_graph <- function(session, g) {
  g = htmlwidgets::appendContent(g, shiny::br(), shiny::div(id='legend', style="margin-left:100px"))
  g$width = 1000
  g$height = 600
  user = if (is.null(session$user)) 'test' else session$user
  dir = file.path(tempdir(), user)
  if (!dir.exists(dir)) dir.create(dir, recursive=T)
  tf = file.path(dir, 'graph.html')
  tf2 = file.path(dir, 'graph.png')
  htmlwidgets::saveWidget(g, tf)
  webshot::webshot(tf, file=tf2)
  tf2
}
