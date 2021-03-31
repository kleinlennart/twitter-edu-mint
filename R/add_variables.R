# tweet reference is onedirectional and in each type mutually exclusive (only one retweeted, quoted and replied_to possible)
unnest_referenced_tweets <- function(d) {
  # information about private tweets gets lost when NULL is removed
  d$ref_replied_to_status_id <- map_chr(d$referenced_tweets, ~ ifelse(is.null(.x), NA_character_, .x$id[.x$type == "replied_to"]))
  d$ref_quoted_status_id <- map_chr(d$referenced_tweets, ~ ifelse(is.null(.x), NA_character_, .x$id[.x$type == "quoted"]))
  d$ref_retweeted_status_id <- map_chr(d$referenced_tweets, ~ ifelse(is.null(.x), NA_character_, .x$id[.x$type == "retweeted"]))
  # More?
  # FIXME: could also just change all NULLs to NA globally...
  return(d)
}

add_reply_variables <- function(dat)  {
  dat %>% mutate(
    is_head = status_id == conversation_id,
    is_reply_to_head = ref_replied_to_status_id == conversation_id
  )
}


# is_head

# is_retweet
# is_reply
# is_quote

# clean date e.g.

# explore data