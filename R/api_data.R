### Get updated data from status ids in dataset ###
library(tidyverse)

files <- dir("download", pattern = ".rds", full.names = TRUE)
tweets <- map_dfr(files, readRDS)

tweets <- tweets %>% distinct(status_id, .keep_all = TRUE)
# tweets %>% count(fachlich_physik, fachlich_mathe) %>% clipr::write_clip()

download <- tweets %>% pull(status_id)

# write_lines(download, "download/ids.txt")
# ids_file <- "download/ids.txt"

saveRDS(download, "coded/all_codings_final_for_download.rds")

### API Call ---------------------------------------------------------------------

library(httr)
library(tidyverse)

## API Authorization
bearer_token <- read_file("token.txt")
headers <- c(
  Authorization = sprintf("Bearer %s", bearer_token)
)

## List of Twitter API Params (all)
params <- list(
  # non_public_metrics,organic_metrics,promoted_metrics only with user context authorization
  tweet.fields = "attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text,withheld",
  expansions = "attachments.poll_ids,attachments.media_keys,author_id,geo.place_id,in_reply_to_user_id,referenced_tweets.id,entities.mentions.username,referenced_tweets.id.author_id",
  media.fields = "duration_ms,height,media_key,non_public_metrics,organic_metrics,preview_image_url,promoted_metrics,public_metrics,type,url,width",
  place.fields = "contained_within,country,country_code,full_name,geo,id,name,place_type",
  poll.fields = "duration_minutes,end_datetime,id,options,voting_status",
  user.fields = "created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,verified,withheld"
)

get_tweet_lookup <- function(status_id, .headers = headers, .params = params) {
  cat("\nLookup tweet;", status_id, file = log_file)
  
  response <- httr::GET(
    url = paste0("https://api.twitter.com/2/tweets/", status_id),
    httr::add_headers(.headers = .headers), query = .params
  )
  
  cat(";Status", httr::status_code(response), file = log_file)
  return(response)
}

# Return data from API Response (based on jsonlite::fromJSON)
parse_response <- function(response) {
  dat <- httr::content(
    response,
    as = "parsed",
    type = "application/json",
    simplifyDataFrame = TRUE
  )
  return(dat)
}

# Parsing method compatible with our existing data cleaning methods
parse_json_response <- function(response) {
  parsed <- httr::content(response, as = "text") %>%
    # flatten did not work properly, maybe do as one step with recursive = TRUE
     jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)
  return(parsed)
}

### Test ------------------------------------------------------------
id <- "862204655316742145"
dat <- get_tweet_lookup(id) %>% parse_response()

### Loop trough ids ------------------------------------------------------
log_file <- file(paste0("logs/download_", Sys.Date(), ".log"), open = "a")

# TODO: add time estimation with lubridate 
i <- 1
while (length(readLines(ids_file)) > 0) {
  cat("\nWaiting 3 s because of rate limits...")
  Sys.sleep(3)

  id <- readLines(ids_file)[1] %>% str_remove_all(" ")
  cat("\n", i, "| Lookup:", id)

  # Api Call
  # FIXME: results[[1]] damn it
  results <- get_tweet_lookup(id) %>% parse_json_response()

  cat("\nSaving tweet...")
  saveRDS(results, paste0("tweet-data/", id, ".rds"))
  cat("saved!")

  # Update reference file
  writeLines(readLines(ids_file)[-1], ids_file)
  cat("\nReference file", ids_file, "updated!\n")
  i <- i + 1
}

cat("\nFinished download!")
beepr::beep("mario")
