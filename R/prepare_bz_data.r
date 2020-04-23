
prepare_data <- function(ids, path, deduplicate, db_file, K, pos, min_docfreq, max_docfreq_pct, remove, ...) {
  message('Reading tokens from database')
  tc = get_tc(db_file, ids)
  tc$meta$date = as.POSIXct(tc$meta$date)
  
  if (!is.na(deduplicate)) {
    message("\nDeduplicating")
    tc$deduplicate('token', meta_cols = 'medium', keep='last', hour_window=24, date_col = 'date', similarity = deduplicate)
  }
  
  feature=NULL
  tc$preprocess('lemma', new_column = 'feature', lowercase = F, as_ascii = T)
  tc$set('feature', gsub('^[-"\']|[-"\']$', '', feature))
  
  
  tc$feature_subset('feature', subset = POS %in% pos, min_char = 2, 
                    min_docfreq = min_docfreq, max_docfreq = floor(max_docfreq_pct * tc$n_meta))
  if (!is.null(remove)) tc$feature_subset('feature', subset = !feature %in% remove)
  
  ## minimize size as much as possible
  tc$delete_columns(c('start','end','sentence','lemma','POS'))
  
  dtm = corpustools::get_dfm(tc, feature='feature', drop_empty_terms = T)
  full_docs = Matrix::rowSums(dtm) > 0
  
  dtm = quanteda::dfm_subset(dtm, full_docs)
  tc$subset_meta(full_docs)
  
  if (!all(rownames(dtm) == tc$meta$doc_id)) stop('document names in tc$meta and dtm are not identical. If this happens, please file bug report')
  
  m = stm::stm(documents=dtm, K=K, ...)
  #m = readRDS('shinyBZtopics_data/created_2020-03-27_09:14:07/shinyBZtopics_stm.rds')
  
  ## this was a nice idea, except html totally messes up the spacing
  
  topwords = stm::labelTopics(m, n = 5)
  topic_names = apply(topwords$prob, 1, paste, collapse=', ')
  
  saveRDS(topic_names, file.path(path, 'shinyBZtopics_topicnames.rds'))
  saveRDS(tc, file = file.path(path, 'shinyBZtopics_tc.rds'))
  saveRDS(m, file = file.path(path, 'shinyBZtopics_stm.rds'))
  message('Data has been saved in the current working directory. You can now run run_topicbrowser()')
  return(NULL)
}

most_likely_topic <- function(tc, m) {
  ## chooses the topic with the highest harmonic mean of P(topic|document) and P(term|topic)
  topicXterm = exp(m$beta$logbeta[[1]])
  docXtopic = m$theta
  
  not_na = !is.na(tc$tokens$feature)
  topic = rep(NA, sum(not_na))
  high_score = rep(0, sum(not_na))
  
  vocab_index = match(tc$tokens$feature[not_na], m$vocab)
  doc_index = match(tc$tokens$doc_id[not_na], tc$meta$doc_id)
  
  
  for (i in 1:ncol(m$theta)) {
    doc_score = docXtopic[,i]
    term_score = topicXterm[i,]
    score = doc_score[doc_index] * term_score[vocab_index]
    
    is_higher = score > high_score
    topic[is_higher] = i
    high_score[is_higher] = score[is_higher]  
  }
  toptopic = rep(NA, nrow(tc$tokens))
  topscore = rep(NA, nrow(tc$tokens))
  toptopic[not_na] = topic
  topscore[not_na] = high_score
  list(topic=toptopic, topicscore=topscore)
}

#' Create .valid_tokens file
#' 
#' Creates the .valid_tokens file. Previous versions will be overwritten 
#' (so only the most recently created tokens are valid)
#'
#' @param tokens 
#'
#' @return safety
#' @export
set_tokens <- function(tokens) {
  writeLines(paste(tokens, collapse='\n'), con = '.valid_tokens')
}

rename_cols <- function(d, from, to) {
  for (i in 1:length(from)) {
    colnames(d)[colnames(d) == from[i]] = to[i]
  }
  d
}

#' Download articles from AmCAT
#' 
#' This is a simple wrapper for downloading the full-text articles from AmCAT. Note that the headline and medium column
#' might be different for more recent versions of AmCAT. This function renames them to prepare the data for \code{\link{create_bz_topics_data}}.
#'
#' @param conn         An amcatr connection
#' @param project      AmCAT project
#' @param set          AmCAT articleset
#' @param headline_col    The name of the headline column
#' @param medium_col   The name of the medium column
#' @param clean        There is some mess in the data causing weird tokens. This does some rough regex cleaning
#'
#' @return A data.frame
#' @export
get_amcat_data <- function(conn, project, set, headline_col='headline', medium_col='medium', clean=T) {
  d = amcatr::amcat.hits(conn, queries='*', project=project, sets=set, col = c('id','date',medium_col,headline_col,'text'))   
  d = rename_cols(d, from=c(medium_col, headline_col), to=c('medium','headline'))
  if (clean) {
    d$headline = clean_amcat_set(d$headline)
    d$text = clean_amcat_set(d$text)
  }
  d
}

clean_amcat_set <- function(x) {
  x = gsub(',,|’’', '"', x) 
  x = gsub(',|’', "'", x) 
  x = gsub('\\(\\(\\(|\\)\\)\\)', " ", x) 
  x = gsub('\\|[\\|-]+', "\n", x) 
  x = gsub('\\(\\+\\)', ' ', x)
  x
}

#' Prepare data for topicbrowser
#' 
#' Parses the texts and stores the results in a database, so that texts do not need to be parsed again if data is updated.
#' Then fits the STM model. The results are not returned, but the tcorpus and stm model are saved in the current working directory.
#' These files are then used by the \code{\link{run_topicbrowser}} function. 
#' 
#' Every time the function is run, the data will be stored under a new name (with a time stamp) 
#' in the shinyBZtopics_data folder. This prevents accidentally overwriting data, but it also means
#' that you'll have to keep track of this folder yourself to limit unnecessary data piling up.
#' 
#' By default, the most recent created data is used in \code{\link{run_topicbrowser}}. You can restore
#' a backup with \code{\link{use_data_backup}}.
#'
#' @param d            A data.frame with the columns "headline", "medium", "date" and "text". All other column will be included as metadata.
#' @param pos          A selection of POS tags to use. See \url{https://universaldependencies.org/u/pos/} for the universal dependencies POS tags.
#' @param min_docfreq  The minimum number of documents (absolute value) in which a feature needs to occur to be used in the topic model
#' @param max_docfreq_pct The maximum number of documents in which a feature can occur, given as a percentage of the total number of documents.
#' @param remove       Specific features to be removed, given as a character vector
#' @param deduplicate  Optionally, a similarity threshold for duplicates (only for articles in same medium within 24 hour diffence)
#' @param K            The number of topics in the stm model
#' @param if_existing  If data has been created before, you need to specify if you want to create "new" data with a new stm model, or if you want to "update" the previous stm model with the new/additional data.
#'                     If this is not specified, you will receive a message saying that you should be clear on this, because its important. 
#' @param udpipe_cores Optionally, use parallel processing. THis is only possible with the development version of corpustools. Give a number for the number of cores to use.
#' @param ...          arguments passed to \code{\link{stm}}
#' 
#' @export
create_bz_topics_data <- function(d, pos=c('NOUN','PROPN'), min_docfreq=5, max_docfreq_pct=0.5, remove=NULL, deduplicate=NA, K=50, if_existing=c('stop','new','update'), udpipe_cores=NULL, ...) {
  if (any(!c('headline','medium','date','text') %in% colnames(d))) stop("d should have columns named headline, medium, date and text")
  if_existing = match.arg(if_existing)
  ## the title column will be combined with the text column when creating the tokens
  ## the headline column will then remain as metadata, which looks better when scrolling the documents
  d$title = d$headline

  if (length(list.files(getwd(), 'shinyBZtopics')) > 0) {
    if (if_existing == 'stop') stop('\nWAAAAAIIIT STOP!!!!!\n\nData has already been created before. Therefore specify with the if_existing argument whether you want to create "new" data with a new stm model (a backup of the old data wil be kept), or to "update" the previous stm model with the new/additional data')
  } else 
    if_existing='new'
  
  db_file = file.path(getwd(), 'shinyBZtopics.db')
  tc_db(d, db_file=db_file, udpipe_cores=udpipe_cores)
  
  if (if_existing == 'new') {
    path = paste0('created_', gsub(' ', '_', as.character(Sys.time())))
    path = file.path(getwd(), 'shinyBZtopics_data', path)
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    tc = prepare_data(unique(d$id), path, deduplicate, db_file, K, pos, min_docfreq, max_docfreq_pct, remove, ...)
  }
  if (if_existing == 'update') {
    stop('update still has to be implemented (eeeeen door!!!)')
  }
}
