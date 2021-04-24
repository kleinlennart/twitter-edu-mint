### Match old with new dataset ###
library(tidyverse)

raw <- readRDS("data/TWITTER_DEUTSCHLAND_OCT_2020_RAW_UNCHANGED.rds")
# dat <- readRDS("../DATA/TWITTER_DEUTSCHLAND_NOV_2020_FINAL.rds")

# select relevant variables and clean retweets
relevant <- raw %>% filter(!is_retweet) %>% select(status_id, user_id, created_at, text)
# Clean mentions (matching with anon data)
relevant <- relevant %>% 
  mutate(text_clean = text %>% str_remove_all("@[[:alnum:]_]+"))

relevant <- relevant %>% distinct(status_id, .keep_all = TRUE)

### Maths ------------------------------------------------------------
mathe <- readRDS("coded/coded_mathe.rds")
  
mathe <- mathe %>% 
  mutate(text_clean = text %>% str_remove_all("@[[:alnum:]_]+"))

match_mathe <- left_join(mathe, relevant, by = "text_clean", keep = TRUE)
saveRDS(match_mathe, "download/match_mathe.rds")

### Physics  ------------------------------------------------------------
physik <- readRDS("coded/coded_physik.rds")

physik <- physik %>% 
  mutate(text_clean = text %>% str_remove_all("@[[:alnum:]_]+"))

match_physik <- left_join(physik, relevant, by = "text_clean", keep = TRUE)
saveRDS(match_physik, "download/match_physik.rds")

### Maths / Physics  ------------------------------------------------------------
mathe_physik <- readRDS("coded/coded_mathe_physik.rds")

mathe_physik <- mathe_physik %>% 
  mutate(text_clean = text %>% str_remove_all("@[[:alnum:]_]+"))

match_mathe_physik <- left_join(mathe_physik, relevant, by = "text_clean", keep = TRUE)
saveRDS(match_mathe_physik, "download/match_mathe_physik.rds")
