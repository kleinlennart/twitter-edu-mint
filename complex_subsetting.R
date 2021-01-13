library(tidyverse)
library(kableExtra)
library(googlesheets4)

dat <- readRDS("../DATA/TWITTER_DEUTSCHLAND_NOV_2020_FINAL.rds")

dat <- dat %>% filter(!is_retweet)


dat <- dat %>%
  mutate(
    text_clean = text %>%
      tolower() %>% ## ATTTENTION
      str_replace_all("ä", "ae") %>%
      str_replace_all("ö", "oe") %>%
      str_replace_all("ü", "ue") %>%
      str_replace_all("ß", "ss") %>%

      str_replace_all(pattern = "https\\S*", "") %>% # urls
      str_replace_all("http\\S*", "") %>% # urls
      str_replace_all("@\\S*", "") %>% # tagging
      str_replace_all("&amp", "") %>% # ampersand encoding
      str_replace_all("[\r\n]", " ") %>% # line breaks

      # str_replace_all("(?!#)(?!-)(?!_)[[:punct:]]", "") %>% # remove all punctuation, but hashtags (negative lookahead)
      # Note: "_-" are often needed for meaningful words, Eigennamen...

      str_replace_all(pattern = "\\s+", replacement = " ") %>% # multiple white space to single space
      base::trimws() # remove white space at start and end of tweets
  )



##### 4. Complex Subject Keywords Method ####


# mathe_words <- mathe_list$words %>% paste0(collapse = "|")
mathe_list <- read_sheet(
  "https://docs.google.com/spreadsheets/d/14DjH2wdz33SQHIJZVcq03IvxNdU-g89LnSbpO_uQgD8/edit#gid=262396079",
  "TEST"
) %>% pull(words) # import as single column vector
# TODO: Dopplungen removen


mathe_list <- mathe_list %>% tolower() %>% 
  str_replace_all("ä", "ae") %>%
  str_replace_all("ö", "oe") %>%
  str_replace_all("ü", "ue") %>%
  str_replace_all("ß", "ss") %>% 
  str_replace_all("-", "[ -]") # dash or space
  

## Looping approach
results <- data.frame()
n <- length(mathe_list)
for (i in 1:n) {
  cat("\n", i, "/", n, " | ", mathe_list[i], sep = "")
  results <- bind_rows(
    results,
    dat %>% filter(str_detect(text, regex(mathe_list[i],
      ignore_case = TRUE
    )))
  )
}
