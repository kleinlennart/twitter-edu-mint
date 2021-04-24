library(tidyverse)
library(kableExtra)
library(googlesheets4)

dat <- readRDS("../DATA/TWITTER_DEUTSCHLAND_NOV_2020_FINAL.rds")


#### 1. User Keywords Method ####
teacher_tweets_mathe <- dat %>%
  filter(!is_retweet) %>%
  filter(str_detect(description, regex("mathe|maths", ignore_case = TRUE))) %>%
  mutate(
    subject = "Mathe",
    method = "User"
  )


teacher_tweets_physik <- dat %>%
  filter(!is_retweet) %>%
  filter(str_detect(description, regex("physik|physics",
    ignore_case = TRUE
  ))) %>%
  mutate(
    subject = "Physik",
    method = "User"
  )

teacher_tweets <- bind_rows(teacher_tweets_mathe, teacher_tweets_physik)

#### 2. Subject Hashtags Method ####

## Change umlaute in Text
dat <- dat %>%
  mutate(text_clean = text %>%
    tolower() %>%
    str_replace_all("ä", "ae") %>%
    str_replace_all("ö", "oe") %>%
    str_replace_all("ü", "ue") %>%
    str_replace_all("ß", "ss"))

hashtags <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1vv5Td-XMsrdHU_u9vaZTlqlTGPj1bScDYNq8OuVFS9U/edit#gid=704172647",
  "Subject Hashtags"
)


#### Physik
physik_regex <- hashtags %>%
  pull(Physik) %>%
  na.omit() %>%
  paste(collapse = "(\\W|$)|") %>%
  paste0("(\\W|$)")

physik_hashtags <- dat %>%
  filter(!is_retweet) %>%
  filter(str_detect(text_clean, regex(physik_regex, ignore_case = TRUE))) %>%
  mutate(
    subject = "Physik",
    method = "Hashtags"
  )



##### Mathe

mathe_regex <- hashtags %>%
  pull(Mathe) %>%
  na.omit() %>%
  paste(collapse = "(\\W|$)|") %>%
  paste0("(\\W|$)")

mathe_hashtags <- dat %>%
  filter(!is_retweet) %>%
  filter(str_detect(text_clean, regex(mathe_regex, ignore_case = TRUE))) %>%
  mutate(
    subject = "Mathe",
    method = "Hashtags"
  )

hashtag_tweets <- bind_rows(physik_hashtags, mathe_hashtags)


###### Alle
# all_regex <- paste0(mathe_regex, "|", physik_regex)
# all_hashtags <- dat %>%
#   filter(!is_retweet) %>%
#   filter(str_detect(text_clean, regex(all_regex, ignore_case = TRUE)))
# 
# ## Overlap Report:
# (nrow(mathe_hashtags) + nrow(physik_hashtags)) - nrow(all_hashtags)


#### 3. Naive Tweet Keyword Method ####

naive_mathe <- dat %>%
  filter(!is_retweet) %>%
  filter(str_detect(text_clean, regex("mathe|maths", ignore_case = TRUE))) %>%
  mutate(
    subject = "Mathe",
    method = "Naive"
  )

naive_physik <- dat %>%
  filter(!is_retweet) %>%
  filter(str_detect(text_clean, regex("physik|physics", ignore_case = TRUE))) %>%
  mutate(
    subject = "Physik",
    method = "Naive"
  )

naive_tweets <- bind_rows(naive_mathe, naive_physik)

#### + Summary ####
basic_methods <- bind_rows(teacher_tweets, hashtag_tweets, naive_tweets) 

saveRDS(basic_methods, "../OUTPUTS/basic_methods.rds")





  
