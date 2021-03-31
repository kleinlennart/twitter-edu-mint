library(tidyverse)
library(kableExtra)
library(googlesheets4)

dat <- readRDS("../DATA/TWITTER_DEUTSCHLAND_NOV_2020_FINAL.rds")

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

      str_replace_all(pattern = "\\s+", replacement = " ") %>% # multiple white space to single space
      base::trimws() # remove white space at start and end of tweets
  )


##### 4. Complex Subject Keywords Method ####

tryCatch(mathe_list <- read_sheet(
  "https://docs.google.com/spreadsheets/d/14DjH2wdz33SQHIJZVcq03IvxNdU-g89LnSbpO_uQgD8/edit#gid=262396079",
  "DATA #2"
) %>% pull(Keywords)) # import as single column vector

# Number of duplicates
mathe_list <- mathe_list %>%
  na.omit() %>% # Remove NAs
  unique() %>% # Remove possible duplicates
  sort()

## Clean Keyword list
mathe_list <- mathe_list %>%
  tolower() %>%
  str_replace_all("ä", "ae") %>%
  str_replace_all("ö", "oe") %>%
  str_replace_all("ü", "ue") %>%
  str_replace_all("ß", "ss")
  # regex beide Schreibweisen (mit Bindestrich oder ohne)
  # str_replace_all("-", "[ -]")

mathe_keywords <- data.frame(keyword = mathe_list)

#### Looping approach (greedy) ####
# remove retweets from dataset
dat <- dat %>% filter(!is_retweet)

# create empty data frame for sampling
results <- data.frame()

# Variables for progress indication
n <- nrow(mathe_keywords)
i <- 1
for (keyword in mathe_list) {
  cat("\n", i, "/", n, " | ", nrow(results), " | ", keyword, sep = "")

  results <- bind_rows(
    results,
    dat %>% filter(str_detect(text_clean, regex(keyword,
      ignore_case = TRUE
    )))
  )
  # length of results to calculate relative gain
  mathe_keywords$n[i] <- nrow(results)
  i <- i + 1
}
rm(keyword)

# Add Search descriptives
results <- results %>% 
  mutate(
    subject = "Mathe",
    method = "Keywords"
  )


# Remove duplicates from double matching
results <- results %>% filter(!duplicated(status_id))

saveRDS(results, "../OUTPUTS/mathe_results_all2.rds")

##########
# sheet_write(
#   mathe_keywords, "https://docs.google.com/spreadsheets/d/14DjH2wdz33SQHIJZVcq03IvxNdU-g89LnSbpO_uQgD8/edit#gid=1344669147",
#   ""
# )


