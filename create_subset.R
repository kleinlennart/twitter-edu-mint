library(tidyverse)
library(kableExtra)
library(googlesheets4)

dat <- readRDS("../DATA/TWITTER_DEUTSCHLAND_NOV_2020_FINAL.rds")


#### 1. User Keywords Method ####
teacher_tweets <- dat %>%
  filter(!is_retweet) %>%
  filter(str_detect(description, regex("mathe|maths|physik|physics",
    ignore_case = TRUE
  )))

nrow(teacher_tweets)
teacher_tweets %>%
  pull(user_id) %>%
  unique() %>%
  length()


#### 2. Subject Hashtags Method ####

## Change umlaute in Text
dat <- dat %>%
  mutate(text_clean = text %>%
    str_replace_all("ä", "ae") %>%
    str_replace_all("ö", "oe") %>%
    str_replace_all("ü", "ue") %>%
    str_replace_all("ß", "ss"))


#### Physik
physik_regex <- "#didaktikphysik|#leifiphysik|#lieblingsfachphysik|#pflichtfachphysik|#physikdidaktik|#physikedu|#physiklehrer|#physiklehrerinnen|#physiklehrerwettbewerb|#physiklehrkraefte|#physiklk|#physiknachhilfe|#physikstunde|#physikunterricht|#physiklehrerzimmer"
physik_hashtags <- dat %>%
  filter(!is_retweet) %>%
  filter(str_detect(text_clean, regex(physik_regex, ignore_case = TRUE)))

nrow(physik_hashtags)

##### Mathe

mathe_regex <- "#flippedmathe|#letsrockmathe|#matheabi|#matheabitur|#matheabiturpruefung|#mathebydanieljung|#mathedidaktik|#mathegym|#mathelehren|#mathelehrer|#mathelehrerin|#mathelehrerinnen|#mathelehrkraefte|#mathematikdidaktik|#mathematikdidaktikerinnen|#mathematikfoerderwahn|#mathematiklehren|#mathematiklehrer|#mathematiklehrerin|#mathematiklehrerinnen|#mathematiklehrkraefte|#mathematiklehrkraeften|#mathematikuntericht|#mathematikunterricht|#mathenachhilfe|#matheunterricht|#pflichtfachmathe|#pflichtfachmathematik|#schulfachmathematik|#mathetwitterlehrerzimmer"
mathe_hashtags <- dat %>%
  filter(!is_retweet) %>%
  filter(str_detect(text_clean, regex(mathe_regex, ignore_case = TRUE)))
nrow(mathe_hashtags)


###### Alle
all_regex <- paste0(mathe_regex, "|", physik_regex)
all_hashtags <- dat %>%
  filter(!is_retweet) %>%
  filter(str_detect(text_clean, regex(all_regex, ignore_case = TRUE)))

## Overlap Report:
(nrow(mathe_hashtags) + nrow(physik_hashtags)) - nrow(all_hashtags)


#### 3. Naive Tweet Keyword Method ####
naive_regex <- "mathe|maths|physik|physics"
naive_tweets <- dat %>%
  filter(!is_retweet) %>%
  filter(str_detect(text_clean, regex(all_regex, ignore_case = TRUE)))

nrow(naive_tweets)


#### + Summary ####
basic_methods <- bind_rows(teacher_tweets, teacher_tweets, naive_tweets) %>%
  filter(!duplicated(status_id))

