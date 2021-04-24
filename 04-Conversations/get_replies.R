# Reference on replies:
# https://cborchers.com/2021/03/23/notes-on-downloading-conversations-through-twitters-v2-api/

library(tidyverse)

# dataframe with all cleaned downloaded conversations
# see "clean_conversations.R" for more
conv <- readRDS("data/all_conversations_final.rds")

## tibble of cleaned downloaded tweets with "status_id", "created_at" and "conversation_id"
# with n>0 replies, can be replies themselves (not head)
# see "extract-conversations.R" for generation
original <- readRDS("data/conversations_reference_sheet.rds")

# status_ids of all initially downloaded tweets (hashtag search) with n>0 replies
greenlight <- original$status_id # pass the heads of conversations

# target = get the replies to these tweets
target <- original$status_id

# get reply when it replies to a downloaded original tweet (either head or subthread)
found <- conv$status_id[conv$ref_replied_to_status_id %in% target] # initial search

i <- 1
# recursive lookup
while (length(found) > 0) {
  cat("Found", length(found), "replies in layer", i, "\n")
  # append found replies to greenlight
  greenlight <- c(greenlight, found)
  # Search in next layer (replies to replies)
  target <- found # already found replies become the new base layer
  found <- conv$status_id[conv$ref_replied_to_status_id %in% target]
  i <- i + 1 # next layer
}
cat("\nFinished reply lookup!")

cat("\nCollecting data...")
replies <- conv %>%
  filter(status_id %in% greenlight) %>% # select only the found forward replies
  mutate(is_forward_reply = TRUE) %>%  # add variable 
  arrange(desc(created_at))

cat("\nSaving", nrow(replies),"unique replies")
saveRDS(replies, "data/forward-replies-final.rds")

usethis::ui_done("Finished!!")
beepr::beep("coin")