
# get tweet files
files <- dir(path = "tweet-data", full.names = TRUE, pattern = ".rds")


# TODO: check empty files
conversations <- map_dfr(files, function(file) {
  d <- file %>% readRDS()
  tibble(
    status_id = d$data$id,
    created_at = d$data$created_at,
    reply_count = d$data$public_metrics$reply_count,
    conversation_id = d$data$conversation_id
  )
})

conversations <- conversations %>%
  filter(reply_count > 0) %>% 
  rowid_to_column("index")                  

# is_head indicates if status is a conversation starter
conversations <- conversations %>% mutate(is_head = status_id == conversation_id)

# save conversations reference
conversations %>% saveRDS("data/conversations_reference_sheet.rds")

# write download file
conversations %>% 
  pull(conversation_id) %>% 
  unique() %>% 
  write_lines("download/cids_download.txt")
                     
                     
            
