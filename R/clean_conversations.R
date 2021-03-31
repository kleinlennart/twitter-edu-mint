### Script for complete Conversations Parsing + Cleaning from .rds files ###
library(tidyverse)
source("R/conversations_functions.R")

# get all conversation files
cat("\nGetting all conversation files...")
files <- dir(path = "conversations-data", full.names = TRUE, pattern = "[[:digit:]]+.rds")
cat("\nFound", length(files), ".rds files")

# Iterate over all conversations in parsed data and clean + join + rbind
final <- imap_dfr(files, function(file, index) {
  # imap for progress indication
  cat("\014Processing", index, "/", length(files), "conversations")
  # Read in file and merge subqueries (pages)
  d <- file %>%
    readRDS() %>%
    merge_queries()

  # Check if conversation is empty
  if (nrow(d$main) == 0) {
    return(NULL) # empty row in rbind
    cat(file, file = "download/empty_conversations.txt", append = TRUE)
  } else {
    # Clean and join data
    d %>%
      rename_vars() %>%
      # preselect_vars() %>%
      fill_missing_tibbles() %>%
      prepare_join() %>%
      drop_duplicates() %>%
      join_tables() %>%
      unnest_referenced_tweets %>% 
      # Add variable from which conversation the data resulted
      mutate(conversation_dat = str_extract(file, "[:digit:]+"))
  }
})

cat("\nSaving Raw Dataset...")
saveRDS(final, paste0("data/all_conversations_raw_", nrow(final), ".rds"))

cat("\nFinishing up...")
clean <- final %>%
distinct(status_id, .keep_all = TRUE) %>%
  arrange(desc(created_at)) # in timeline order

cat("\nSaving Dataset...")
saveRDS(clean, paste0("data/all_conversations_final.rds"))
