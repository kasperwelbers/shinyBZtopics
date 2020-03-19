prepare_data <- function(ids, deduplicate, db_file, K, pos, min_docfreq, max_docfreq_pct, remove, ...) {
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
  full_docs = rowSums(dtm) > 0
  
  dtm = quanteda::dfm_subset(dtm, rowSums(dtm) > 0)
  tc$subset_meta(full_docs)
  
  if (!all(rownames(dtm) == tc$meta$doc_id)) stop('document names in tc$meta and dtm are not identical. If this happens, please file bug report')
  
  m = stm::stm(documents=dtm, K=K, ...)
    
  saveRDS(tc, file = 'shinyBZtopics_tc.rds')
  saveRDS(m, file = 'shinyBZtopics_stm.rds')
  message('Data has been saved in the current working directory. You can now run run_topicbrowser()')
  return(NULL)
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
  x = gsub(',,', "''", x) 
  x = gsub('\\(\\(\\(|\\)\\)\\)', "''", x) 
  x = gsub('\\|[\\|-]+', "\n", x) 
  x = gsub('\\(\\+\\)', '', x)
  x
}

#' Prepare data for topicbrowser
#' 
#' Parses the texts and stores the results in a database, so that texts do not need to be parsed again if data is updated.
#' Then fits the STM model. The results are not returned, but the tcorpus and stm model are saved in the current working directory.
#' These files are then used by the \code{\link{run_topicbrowser}} function. Thus, this function only needs to be used the first time and
#' when new data is added. 
#'
#' @param d            A data.frame with the columns "headline", "medium", "date" and "text". All other column will be included as metadata.
#' @param pos          A selection of POS tags to use. See \url{https://universaldependencies.org/u/pos/} for the universal dependencies POS tags.
#' @param min_docfreq  The minimum number of documents (absolute value) in which a feature needs to occur to be used in the topic model
#' @param max_docfreq_pct The maximum number of documents in which a feature can occur, given as a percentage of the total number of documents.
#' @param remove       Specific features to be removed, given as a character vector
#' @param deduplicate  Optionally, a similarity threshold for duplicates (only for articles in same medium within 24 hour diffence)
#' @param K            The number of topics in the stm model
#' @param ...          arguments passed to \code{\link{stm}}
#' 
#' @export
create_bz_topics_data <- function(d, pos=c('NOUN','PROPN'), min_docfreq=5, max_docfreq_pct=0.5, remove=NULL, deduplicate=NA, K=50, ...) {
  if (any(!c('headline','medium','date','text') %in% colnames(d))) stop("d should have columns named headline, medium, date and text")
  
  ## the title column will be combined with the text column when creating the tokens
  ## the headline column will then remain as metadata, which looks better when scrolling the documents
  d$title = d$headline
  
  db_file = file.path(getwd(), 'shinyBZtopics.db')
  tc_db(d, db_file=db_file)

  tc = prepare_data(unique(d$id), deduplicate, db_file, K, pos, min_docfreq, max_docfreq_pct, remove, ...)
}
