#' Run the Shiny Application
#' 
#' This uses the data most recently created with \code{\link{create_bz_topics_data}}.
#' In case you have (accidentally) overwritten data that you want to use, see \code{\link{use_data_backup}}
#'
#' For some data, such as topic names, the app uses persistent storage shared by all users. 
#' You can thus set the topic names within the app.
#'
#' @param token_auth If True, read the .valid_tokens file (create with \code{\link{set_tokens}}) to authenticate tokens. 
#'                   The token should be passed to the app with the ?token URL parameter
#' @param port    The port
#' @param topic_selection Optionally, the indices of topics to show
#'                   
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_topicbrowser <- function(token_auth=F, port=6171, topic_selection=NULL, ...) {
  if (token_auth && !file.exists('.valid_tokens')) stop('token_auth is used, but no tokens have been created. Use set_tokens(...) first')
  if (!webshot::is_phantomjs_installed()) {
    message("This app uses webshot to enable screenshots of graphs. This requires first installing phantomJS")
    webshot::install_phantomjs()
  }
  
  ## prepare slow and memory heavy data, so that its not copied and created/loaded by each user
  path = get_data_path(NULL)
  
  data_exists = file.exists(file.path(path, 'shinyBZtopics_tc.rds')) && file.exists(file.path(path, 'shinyBZtopics_stm.rds')) && file.exists(file.path(path, 'shinyBZtopics_topicnames.rds'))
  if (!data_exists) stop('Data for the topic browser has not yet been created. Run create_bz_topics_data(...) first')
  tc = readRDS(file.path(path, 'shinyBZtopics_tc.rds'))
  m = readRDS(file.path(path, 'shinyBZtopics_stm.rds'))
  topic_names_file = file.path(path, 'shinyBZtopics_topicnames.rds')
  
  ## topic colors, topic groups and queries are created on the spot if they do not yet exist
  topic_colors_file = file.path(path, 'shinyBZtopics_colors.rds')
  if (!file.exists(topic_colors_file)) {
    topic_colors = create_topic_colors(ncol(m$theta))
    saveRDS(topic_colors, topic_colors_file)
  }
  
  topic_groups_file = file.path(path, 'shinyBZtopics_groups.rds')
  if (!file.exists(topic_groups_file)) {
    topic_groups = create_topic_groups(ncol(m$theta))
    saveRDS(topic_groups, topic_groups_file)
  }
  
  queries_file = file.path(path, 'shinyBZtopics_queries.rds')
  if (!file.exists(queries_file)) {
    queries = ''
    saveRDS(queries, queries_file)
  }
  
  mlt = most_likely_topic(tc,m)
  tc$tokens$topic = mlt$topic
  tc$tokens$topicscore = mlt$topicscore 
  tc$tokens[, topicscore := topicscore / sum(topicscore, na.rm = T), by='doc_id']
  
  if (!is.null(topic_selection)) {
    tc$tokens$topic[!tc$tokens$topic %in% topic_selection] = NA
  } else {
    topic_selection = 1:ncol(m$theta)
  }
  
  top_terms = stm::labelTopics(m, n = 20)
  #frex_matrix = stm::calcfrex(m$beta$logbeta[[1]])
  
  topic_ids = paste0('topic_', 1:ncol(m$theta))
  
  url = 'http://127.0.0.1:6171?amcat_queries=bz# bz OR "buitenlandse zaken"\nminister# minister'
  url = utils::URLencode(url)
  message(paste0('When the server is running, you can test opening a session in your browser with url parameters for amcat style queries\n\n', 
                 url))
  
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server, options = list(port=port, ...)), 
    golem_opts = list(tc=tc, m=m, K=ncol(m$theta), topic_names_file=topic_names_file, topic_colors_file=topic_colors_file, topic_groups_file=topic_groups_file, queries_file=queries_file,
                      topic_selection=topic_selection, top_terms=top_terms, token_auth=token_auth)
  )
}

#' Restore previous shinyBZtopics data
#' 
#' TLDR: give the name of a backup folder in shinyBZtopics_data/ to restore this data.
#' 
#' All previously created data with \code{\link{create_bz_topics_data}} is stored in the shinyBZtopics_data folder,
#' with timestamps of the time the data was first created. Whenever you run run_topicbrowser, the most recent
#' data is used in the app. If you pass the name of one of the backup folder (named 'created_DATETIME')
#' to the use_data_backup function, it will create a new copy with a new timestamp.
#' 
#' Note that if you updated data (using if_existing = 'update'), this will have created a new data folder (the original data will still have its own backup)
#'
#' @param fname The name of a folder in shinyBZtopics_data/
#'
#' @export
use_data_backup <- function(fname) {
  if (!dir.exists(file.path(getwd(), 'shinyBZtopics_data'))) stop('no data has been created yet')
  
  path = file.path(getwd(), get_data_path(fname))
  
  new_path = file.path(getwd(),
                       'shinyBZtopics_data',
                       paste0('created_', gsub(' ', '_', as.character(Sys.time()))))
  x = file.rename(path, new_path)
}

get_data_path <- function(fname=NULL) {
  f = list.dirs('shinyBZtopics_data', recursive = F, full.names = T)
  if (is.null(fname)) {
    date = strptime(gsub('.*created\\_', '', f), '%Y-%m-%d_%H:%M:%S')
    f = f[order(date, decreasing = T)][1]
  } else {
    which_f = grepl(fname, f, fixed=T)
    if (!any(which_f)) stop('fname is not a valid folder name in shinyBZtopics_data/')
    f = f[which_f]
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
  
  conn = amcat.connect('https://bzk.nieuwsmonitor.org')
  
  d = get_amcat_data(conn, project=7, set=4606, clean = T, headline_col = 'title', medium_col = 'medium')
  
  create_bz_topics_data(d, if_existing = 'new', 
                        pos = c('NOUN','PROPN'), min_docfreq = 5, max_docfreq_pct = 0.5, deduplicate=0.9,
                        K=50, seed=1)
  run_topicbrowser(token_auth=F)
 }