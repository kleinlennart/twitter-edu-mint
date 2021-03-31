# custom convenience function for wider tables (with kableExtra)
View.tweets <- function(x) {
  kable(x) %>% kable_styling("striped", full_width = TRUE)
}

#
tweet_lookup <- function(dat, keyword, remove_retweets = TRUE) {
  if (remove_retweets) {
    dat <- dat %>% filter(!is_retweet)
  }

  results <-     dat %>%
    filter(
      str_detect(text_clean, regex(keyword, ignore_case = TRUE))
    )
  # Non-greedy matching
  # regex(paste0(hashtag, "(\\W|$)"), ignore_case = TRUE)
  

  cat("n =", nrow(results))
  return(results)
}



final %>% group_by(status_id) %>% 
  summarise(
    methods_used = paste(method, collapse = ", " )
  )
  

