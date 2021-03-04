
<!-- README.md is generated from README.Rmd. Please edit that file -->
Topic browser
=========

Trial version for a topic browser Shiny app

This only works with specific data for a project, so it's not yet intended for use outside of those involved (though at some point a more general version will probably be shared)

How to use
----------

This requires the amcatr package (which is not on CRAN)

```r
devtools::install_github('amcat/amcat-r')
devtools::install_github('kasperwelbers/shinyBZtopics')
```

The following code reads the data from amcat and launches an instance at the given port.
Note that it also deduplicates the data (turn of by setting deduplicate to NA)

``` r
library(shinyBZtopics)
library(amcatr)
conn = amcat.connect('https://amcat.nl')
  
d = get_amcat_data(conn, project=1916, set=78102, clean = T)

create_bz_topics_data(d, 
                      pos = c('NOUN','PROPN'), min_docfreq = 5, max_docfreq_pct = 0.5, deduplicate=0.9,
                      K=50, seed=1)

run_topicbrowser(token_auth=F)
```

Optionally, you can use multiple cores for parsing the data

```{r}
create_bz_topics_data(d, udpipe_cores=4,
                      pos = c('NOUN','PROPN'), min_docfreq = 5, max_docfreq_pct = 0.5, deduplicate=0.9,
                      K=50, seed=1)
```
