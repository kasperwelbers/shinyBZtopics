#' Run the Shiny Application
#'
#' @param bz_data the data, prepared with create_bz_topics_data()
#' @param port    The port
#' @param token_auth If True, read the .valid_tokens file (create with \code{\link{set_tokens}}) to authenticate tokens. 
#'                   The token should be passed to the app with the ?token URL parameter
#' @param topic_selection Optionally, the indices of topics to show
#' @param fname      Optionally, the name of a folder in shinyBZtopics_data/. By default the most recent
#'                   data is used.
#'                   
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_topicbrowser <- function(bz_data, token_auth=F, port=6171, topic_selection=NULL, fname=NULL, ...) {
  if (token_auth && !file.exists('.valid_tokens')) stop('token_auth is used, but now tokens have been created. Use set_tokens(...) first')
  
  ## prepare slow and memory heavy data, so that its not copied and created/loaded by each user
  path = get_data_path(fname)
  
  data_exists = file.exists(file.path(path, 'shinyBZtopics_tc.rds')) && file.exists(file.path(path, 'shinyBZtopics_stm.rds'))
  if (!data_exists) stop('Data for the topic browser has not yet been created. Run create_bz_topics_data(...) first')
  tc = readRDS(file.path(path, 'shinyBZtopics_tc.rds'))
  m = readRDS(file.path(path, 'shinyBZtopics_stm.rds'))
  topic_names_file = file.path(path, 'shinyBZtopics_topicnames.rds')
  
  mlt = most_likely_topic(tc,m)
  tc$tokens$topic = mlt$topic
  tc$tokens$topicscore = mlt$topicscore 
  tc$tokens[, topicscore := topicscore / sum(topicscore, na.rm = T), by='doc_id']
  
  if (!is.null(topic_selection)) {
    tc$tokens$topic[!tc$tokens$topic %in% topic_selection] = NA
  } else {
    topic_selection = 1:ncol(m$theta)
  }
  
  frex_terms = stm::labelTopics(m, n = 50)$frex
  frex_matrix = stm::calcfrex(m$beta$logbeta[[1]])
  
  topic_ids = paste0('topic_', 1:ncol(m$theta))
  
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server, options = list(port=port, ...)), 
    golem_opts = list(tc=tc, m=m, K=ncol(m$theta), topic_names_file=topic_names_file, topic_selection=topic_selection, frex_terms=frex_terms, frex_matrix=frex_matrix, token_auth=token_auth)
  )
}

get_data_path <- function(fname=NULL) {
  f = list.dirs('shinyBZtopics_data', recursive = F, full.names = T)
  if (is.null(fname)) {
    date = strptime(gsub('.*created\\_', '', f), '%Y-%m-%d_%H:%M:%S')
    f = f[order(date, decreasing = T)][1]
  } else {
    if (!fname %in% gsub('.*/', '', f)) stop('fname is not a valid folder name in shinyBZtopics_data/')
  }
  f
}

#' Run stm::topicQuality for current topicbrowser data
#'
#' @return Nothing, just plots
#' @export
plot_topic_quality <- function() {
  path = get_data_path()
  data_exists = file.exists(file.path(path, 'shinyBZtopics_tc.rds')) && file.exists(file.path(path, 'shinyBZtopics_stm.rds'))
  if (!data_exists) stop('Data for the topic browser has not yet been created. Run create_bz_topics_data(...) first')
  tc = readRDS(file.path(path, 'shinyBZtopics_tc.rds'))
  m = readRDS(file.path(path, 'shinyBZtopics_stm.rds'))
  dtm = corpustools::get_dfm(tc, feature='feature', drop_empty_terms = T)
  stm::topicQuality(m, dtm)
}


function() {
 ## project 1916 in amcat en dan set 78102
  library(shinyBZtopics)
  library(amcatr)
  conn = amcat.connect('https://amcat.nl')
  
  d = get_amcat_data(conn, project=1916, set=78102, clean = T)
  create_bz_topics_data(d, 
                        pos = c('NOUN','PROPN'), min_docfreq = 5, max_docfreq_pct = 0.5, deduplicate=0.9,
                        K=50, seed=1)
  run_topicbrowser(token_auth=F)
  
  
 
}