### Get Conversations ###

library(httr)
library(tidyverse)

## API Authorization
bearer_token <- read_file("token.txt")
headers <- c(
  Authorization = sprintf("Bearer %s", bearer_token)
)

## List of Twitter API Params
params <- list(
  query = NULL, # empty to fill up
  max_results = "500", # range 10-100 / 500
  start_time = "2008-01-01T00:00:00Z", # (YYYY-MM-DDTHH:mm:ssZ) -> RFC3339 date-time
  end_time = "2020-12-31T23:59:59Z", # (YYYY-MM-DDTHH:mm:ssZ)
  tweet.fields = "attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text,withheld",
  expansions = "attachments.poll_ids,attachments.media_keys,author_id,geo.place_id,in_reply_to_user_id,referenced_tweets.id,entities.mentions.username,referenced_tweets.id.author_id",
  media.fields = "duration_ms,height,media_key,preview_image_url,public_metrics,type,url,width",
  place.fields = "contained_within,country,country_code,full_name,geo,id,name,place_type",
  poll.fields = "duration_minutes,end_datetime,id,options,voting_status",
  user.fields = "created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,verified,withheld",
  next_token = NULL # https://developer.twitter.com/en/docs/twitter-api/tweets/search/integrate/paginate
)

# Get Conversation by conversation_id from API (optionally add next_token for pagination)
get_conversation <- function(conversation_id, next_token = NULL, .headers = headers, .params = params) {
  cat("\nQuery:", conversation_id, "| next_token:", next_token, file = log_file)
  
  .params$query <- paste0("conversation_id:", conversation_id)
  .params$next_token <- next_token
  
  response <- httr::GET(
    url = "https://api.twitter.com/2/tweets/search/all",
    httr::add_headers(.headers = .headers), query = .params
  )
  cat(" | Status:", status_code(response), file = log_file)
  return(response)
}

# Return data from API Response (based on jsonlite::fromJSON)
parse_response <- function(response) {
  dat <- content(
    response,
    as = "parsed",
    type = "application/json",
    simplifyDataFrame = TRUE
    # TODO: flatten??
  )
  return(dat)
}

# Parsing method compatible with our existing data cleaning methods
parse_json_response <- function(response) {
  dat <- httr::content(response, as = "text") %>%
    jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)
  return(dat)
}

### Test ------------------------------------------------------------

# dat <- list()
# dat[[1]] <- get_conversation("1003905350335115264") %>% parse_json_response()



### Basic Pagination -----------------------------------------------------------
# open appendable log file
# not the best solution because you have to search in all 4 log files, well, can't write simultaneously
log_file <- file(paste0("logs/conversations_", Sys.Date(), ".log"), open = "a")

cid_file <- "download/cids_download.txt"

progress <- 1
while (length(readLines(cid_file)) > 0) {
  cat("\nWaiting 3 s because of rate limits...")
  Sys.sleep(3)
  conv_id <- readLines(cid_file)[1] %>% str_remove_all(" ")
  cat("\n", progress, "| Query conversation:", conv_id)
  
  i <- 1
  results <- list()
  # first API call
  results[[i]] <- get_conversation(conv_id, next_token = NULL) %>% parse_json_response()
  # get next token
  token <- results[[i]][["meta"]][["next_token"]]
  
  # as long as a next_token exists, download next "page"
  while (!is.null(token)) {
    cat("\nGoing to next page...")
    cat("\nWaiting 3 s because of rate limits...")
    Sys.sleep(3)
    cat("\nQuery next_token", token, "for conversation", conv_id, ":")
    i <- i + 1
    results[[i]] <- get_conversation(conv_id, next_token = token) %>%
      parse_json_response()
    
    # get next token and loop
    token <- results[[i]][["meta"]][["next_token"]]
  }
  
  cat("\nSaving conversation...")
  saveRDS(results, paste0("conversations-data/", conv_id, ".rds"))
  cat("saved!")
  
  # Update reference file
  writeLines(readLines(cid_file)[-1], cid_file)
  cat("\nReference file", cid_file, "updated!\n")
  progress <- progress + 1
}
