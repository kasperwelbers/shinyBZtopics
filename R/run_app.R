#' Run the Shiny Application
#'
#' @param bz_data the data, prepared with create_bz_topics_data()
#' @param port    The port
#' @param token_auth If True, read the .valid_tokens file (create with \code{\link{set_tokens}}) to authenticate tokens. 
#'                   The token should be passed to the app with the ?token URL parameter
#'                   
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_topicbrowser <- function(bz_data, token_auth=F, port=6171, topic_names=NULL, ...) {
  data_exists = file.exists('shinyBZtopics_tc.rds') && file.exists('shinyBZtopics_stm.rds')
  if (!data_exists) stop('Data for the topic browser has not yet been created. Run create_bz_topics_data(...) first')
  if (token_auth && !file.exists('.valid_tokens')) stop('token_auth is used, but now tokens have been created. Use set_tokens(...) first')
  
  tc = readRDS('shinyBZtopics_tc.rds')
  m = readRDS('shinyBZtopics_stm.rds')
  
  tc$tokens$topic = most_likely_topic(tc,m)
  
  if (is.null(topic_names)) 
    topic_names = paste('topic', 1:ncol(m$theta), sep='_')
  else
    if (!length(topic_names) == ncol(m$theta)) stop('topic_names needs to be a vector of the same length as the number of topics')
  
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server, options = list(port=port, ...)), 
    golem_opts = list(tc=tc, m=m, topic_names=topic_names, token_auth=token_auth)
  )
}

most_likely_topic <- function(tc, m) {
  ## chooses the topic with the highest harmonic mean of P(topic|document) and P(term|topic)
  topicXterm = exp(m$beta$logbeta[[1]])
  docXtopic = m$theta
  
  vocab_index = match(tc$tokens$feature, m$vocab)
  doc_index = match(tc$tokens$doc_id, tc$meta$doc_id)
  
  topic = NA
  high_score = ifelse(is.na(tc$tokens$feature), NA, 0)
  harmonic_mean = function(x,y) (2*x*y) / (x+y)
  for (i in 1:ncol(m$theta)) {
    doc_score = docXtopic[,i]
    term_score = topicXterm[i,]
    score = harmonic_mean(doc_score[doc_index], term_score[vocab_index])
    
    is_higher = !is.na(score) & score > high_score
    topic[is_higher] = i
    high_score[is_higher] = score[is_higher]
  }
  topic
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