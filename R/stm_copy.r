## this is copied from stm for calculating the frex scores 
## (stm only returns the order)


stm_safelog <- function(x, min=-1000) {
  out <- log(x)
  out[which(out < min)] <- min
  out
}

stm_js.estimate <- function (prob, ct) {
  if (ct <= 1) {
    return(rep(1/length(prob), length(prob)))
  }
  mlvar <- prob * (1 - prob)/(ct - 1)
  unif <- rep(1/length(prob), length(prob))
  deviation <- sum((prob - unif)^2)
  if (deviation == 0) 
    return(prob)
  lambda <- sum(mlvar)/deviation
  if (is.nan(lambda)) 
    return(unif)
  if (lambda > 1) 
    lambda <- 1
  if (lambda < 0) 
    lambda <- 0
  lambda * unif + (1 - lambda) * prob
}

stm_frex_scores <- function(logbeta, w = 0.5, wordcounts = NULL) {
  ## this is the function from the stm package, but returning the scores instead of the order
  excl <- t(t(logbeta) - matrixStats::colLogSumExps(logbeta))
  if (!is.null(wordcounts)) {
    excl <- stm_safelog(sapply(1:ncol(excl), function(x) stm_js.estimate(exp(excl[, 
                                                                              x]), wordcounts[x])))
  }
  freqscore <- apply(logbeta, 1, data.table::frank)/ncol(logbeta)
  exclscore <- apply(excl, 1, data.table::frank)/ncol(logbeta)
  frex <- 1/(w/freqscore + (1 - w)/exclscore)
  frex
}