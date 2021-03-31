### Read in and merge + parse downloaded tweets ###

library(tidyverse)

# Get Functions
source(here::here("R", "tweet_clean_functions.R"))


try <- test %>%
  rename_vars()

%>%
  fill_missing_tibbles() %>%
  preselect_vars() 

  
  prepare_join() %>%
  drop_duplicates() %>%
  join_tables()


files <- dir(path = "tweet-data/", full.names = TRUE, pattern = ".rds")
# Iterate over all conversations in parsed data and clean + join + rbind
final <- imap_dfr(files[1:10], function(file, index) {
  # imap for progress indication
  cat("\014Processing", index, "/", length(files), "tweets")
  file %>%
    readRDS() %>% 
    rename_vars() %>%
    preselect_vars() %>%
    fill_missing_tibbles() %>%
    prepare_join() %>%
    drop_duplicates() %>%
    join_tables()
  
  # FIXME: Check if tweet is empty (not public anymore)
  # how would this be signified?
  # if (nrow(d$main) == 0) {
  #   return(NULL)
  # } else {
  })

cat("Finishing up...")
# final <- final %>%
### FIXME: THIS distinct() COULD BE PROBLEMATIC, NEED TO CHECK MORE
distinct(status_id, .keep_all = TRUE) %>%
  arrange(created_at) # not formated yet

cat("Saving Dataset...")
saveRDS(final, "parsed/test_conv-1278413164040855553.rds")

# TODO: Paper Vars Selection here?


### Select forward replies ----------------------------------------
# use hoist()?

final <- final %>% hoist("referenced_tweets", "type", .remove = FALSE)
final %>%
  filter(type == "NULL") %>%
  View()

unnest_variables <- function() {
  # information about private reference gets lost this way...
  # FIXME: could also just change all Nulls to NA globally...
  final$ref_replied_to_status_id <- map_chr(final$referenced_tweets, ~ ifelse(is.null(.x), NA_character_, .x$id[.x$type == "replied_to"]))
  final$ref_quoted_status_id <- map_chr(final$referenced_tweets, ~ ifelse(is.null(.x), NA_character_, .x$id[.x$type == "quoted"]))
  final$ref_retweeted_status_id <- map_chr(final$referenced_tweets, ~ ifelse(is.null(.x), NA_character_, .x$id[.x$type == "retweeted"]))
  # More?
}
conversations %>% filter(conversation_id == "1343613890580668416") -> test

test$status_id

# Task: get all replies to this tweet
# status_id 1343617636169617411 (is not head) should have 4 replies 


final %>% filter(ref_replied_to_status_id == "1343617636169617411") %>% View()

