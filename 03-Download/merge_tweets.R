### Data Cleaning -------------------------------------------------------
library(tidyverse)

# get all tweet files
cat("\nGetting all .rds tweet files...")
path <- here::here("data", "tweet-data/")
files <- dir(path = path, full.names = TRUE, pattern = "[[:digit:]]+.rds")
cat("\nFound", length(files), ".rds files\n")

# Get Functions
source(here::here("03-Download", "tweet_merge_functions.R"))

# Read in all tweets into one list
dat <- map(files, ~ .x %>% readRDS())
# remove missing tweets
dat <- dat %>% keep(~ .x$data %>%
  is.null() %>%
  `!`())

# Transform main to tibble
all <- tibble(
  main = map(dat, "data")
)

# merge and wrangle includes
includes <- dat %>%
  merge_includes() %>%
  rename_vars() %>%
  preselect_vars() %>%
  drop_duplicates()

# rectangle main
clean <- all %>%
  unnest_wider("main") %>%
  unnest_wider("public_metrics") %>%
  unnest_wider("attachments") %>%
  unnest_wider("geo")


final <- clean %>%
  rename_main() %>%
  select(-contains("entities"), -contains("poll"), -contains("context_annotations")) %>%
  left_join(includes$users, by = "user_id") %>% 
  left_join(includes$places, by = "place_id")

## Add referenced tweets
final$referenced_tweets <- map(final$referenced_tweets, function(ref) {
  if (is.null(ref)) {
    # return empty structure with same attributes for data consistency #tidy
    return(structure(list(), .Names = character(0), row.names = integer(0), class = c("data.frame")))
  } else {
    # join primary keys with variables of referenced_tweets
    all_ref <- left_join(ref, includes$tweets, by = c("id" = "ref_status_id"), keep = TRUE)
    return(all_ref)
  }
})

## Clean
cat("\nFinishing up dataset...")
final <- final %>%
  distinct(status_id, .keep_all = TRUE) %>%
  unnest_references()
  arrange(desc(created_at)) # timeline order (newest on top)

# Export ---------------------------------------------------------------------  
  
results_path <- here::here("data", "tweets", "all_tweets_april.rds")
cat("\nSaving Dataset to:\n", results_path, sep = "")
saveRDS(final, results_path)

beepr::beep("coin")
usethis::ui_done("Finished!")
