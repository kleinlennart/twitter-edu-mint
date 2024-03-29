merge_includes <- function(dat) {
  # iterate over each API query response in input list and rbind the subtables together (not joining)
  all <- list()
  # all$main <- map(dat, ~ .x$data) # This change fixed weird json parsing error
  all$users <- map_dfr(dat, ~ .x$includes$users)
  all$tweets <- map_dfr(dat, ~ .x$includes$tweets)
  all$media <- map_dfr(dat, ~ .x$includes$media)
  all$places <- map_dfr(dat, ~ .x$includes$places)
  # all$errors ?
  # all$polls ?
  # all$meta
  return(all)
}



rename_main <- function(dat, report_missing = FALSE) {
  main_names <- c(
    id = "status_id",
    author_id = "user_id",
    text = "text",
    created_at = "created_at",
    conversation_id = "conversation_id",
    lang = "lang",
    possibly_sensitive = "possibly_sensitive",
    referenced_tweets = "referenced_tweets",
    reply_settings = "reply_settings",
    context_annotations = "context_annotations",
    source = "source",
    in_reply_to_user_id = "in_reply_to_user_id",
    entities.mentions = "entities.mentions",
    entities.urls = "entities.urls",
    entities.hashtags = "entities.hashtags",
    entities.annotations = "annotations", # TODO: consider renaming
    public_metrics.retweet_count = "retweet_count",
    public_metrics.reply_count = "reply_count",
    public_metrics.like_count = "like_count",
    public_metrics.quote_count = "quote_count",
    media_keys = "media_id",
    poll_ids = "poll_id",
    place_id = "place_id",
    geo.coordinates.type = "place_coords_type",
    geo.coordinates.coordinates = "place_coords"
  )
  dat <- plyr::rename(dat, replace = main_names, warn_missing = report_missing)
}


# Data Cleaning -----------------------------------------------------------

## Rename dataset with predefined namekeys for each subdataset
# set report_missing to TRUE to check for variables currently not in our preselection
rename_vars <- function(d, report_missing = FALSE) {
  users_names <- c(
    protected = "user_protected",
    username = "user_name",
    pinned_tweet_id = "user_pinned_status_id",
    description = "user_description",
    verified = "user_is_verified",
    id = "user_id",
    created_at = "user_created_at",
    url = "user_url",
    location = "user_location",
    name = "user_fullname",
    profile_image_url = "user_profile_image",
    entities.url.urls = "entities.url.urls",
    entities.description.mentions = "entities.description.mentions",
    entities.description.urls = "entities.description.urls",
    entities.description.hashtags = "entities.description.hashtags",
    public_metrics.followers_count = "followers_count",
    public_metrics.following_count = "following_count",
    public_metrics.tweet_count = "tweet_count",
    public_metrics.listed_count = "listed_count"
  )
  d$users <- plyr::rename(d$users, replace = users_names, warn_missing = report_missing)
  
  tweets_names <- c(
    possibly_sensitive = "ref_possibly_sensitive",
    author_id = "ref_user_id",
    id = "ref_status_id",
    lang = "ref_lang",
    reply_settings = "ref_reply_settings",
    context_annotations = "ref_context_annotations",
    source = "ref_source",
    text = "ref_text",
    created_at = "ref_created_at",
    conversation_id = "ref_conversation_id",
    in_reply_to_user_id = "ref_in_reply_to_user_id",
    referenced_tweets = "ref_referenced_tweets",
    entities.urls = "entities.urls",
    entities.hashtags = "entities.hashtags",
    entities.mentions = "entities.mentions",
    entities.annotations = "ref_annotations",
    public_metrics.retweet_count = "ref_retweet_count",
    public_metrics.reply_count = "ref_reply_count",
    public_metrics.like_count = "ref_like_count",
    public_metrics.quote_count = "ref_quote_count",
    attachments.media_keys = "ref_media_id",
    attachments.poll_ids = "poll_ids",
    geo.place_id = "ref_place_id",
    geo.coordinates.type = "ref_place_coords_type",
    geo.coordinates.coordinates = "ref_place_coords"
  )
  d$tweets <- plyr::rename(d$tweets, replace = tweets_names, warn_missing = report_missing)
  
  media_names <- c(
    height = "media_height",
    preview_image_url = "media_preview_url",
    media_key = "media_id",
    type = "media_type",
    width = "media_width",
    url = "media_url",
    duration_ms = "media_duration",
    public_metrics.view_count = "media_views"
  )
  d$media <- plyr::rename(d$media, replace = media_names, warn_missing = report_missing)
  
  places_names <- c(
    country_code = "place_country",
    country = "place_country_full",
    place_type = "place_type",
    id = "place_id",
    full_name = "place_full_name",
    name = "place_name",
    geo.type = "place_bbox_type",
    geo.bbox = "place_bbox"
  )
  d$places <- plyr::rename(d$places, replace = places_names, warn_missing = report_missing)
  return(d)
}

# Reduce payload by removing obsolete variables
# Always use contains here! (edge case handling!)
preselect_vars <- function(d) {
  # d$main <- d$main %>% select(-contains("entities"), -contains("poll"), -contains("context_annotations"))
  d$users <- d$users %>% select(-contains("entities"), -contains("user_fullname"), -contains("user_url"), -contains("user_protected"), -contains("user_profile_image"), -contains("user_pinned_status_id"))
  d$tweets <- d$tweets %>% select(-contains("entities"), -contains("poll"), -contains("context_annotations"))
  d$media <- d$media %>% select(-contains("media_width"), -contains("media_height"))
  d$places <- d$places %>% select(-contains("place_country_full"))
  return(d)
}

# fill_missing_tibbles <- function(d) {
#   # -> place_id / media_id and ref_status_id are also not in $main if not in the included data
#   # empty values needed for distinct to work
#   if (nrow(d$tweets) == 0) {
#     d$main$referenced_tweets <- list(NULL)
#     d$tweets <- tibble(ref_status_id = NA_character_)
#   }
#   if (nrow(d$places) == 0) {
#     d$main$place_id <- NA_character_
#     d$places <- tibble(place_id = NA_character_)
#   }
#   if (nrow(d$media) == 0) {
#     d$main$media_id <- list(NULL)
#     d$media <- tibble(place_id = NA_character_)
#   }
#   return(d)
# }

# -> Identifier variables need to be distinct for correct joining!
drop_duplicates <- function(d) {
  # d$main <- d$main %>% distinct(status_id, .keep_all = TRUE)
  d$users <- d$users %>% distinct(user_id, .keep_all = TRUE)
  d$tweets <- d$tweets %>% distinct(ref_status_id, .keep_all = TRUE)
  d$places <- d$places %>% distinct(place_id, .keep_all = TRUE)
  return(d)
}

join_tables <- function(d) {
  d$main %>%
    left_join(d$users, by = "user_id") %>%
    left_join(d$places, by = "place_id")
}


wrangle_data <- function(d) {
  # unnest media keys list, lookup media data and nest again in data frame
  # Note: it's essentially a left_join but the primary keys are in a vector and are thus matched with a filter and rowbinded by map
  
  # d$main$referenced_media <- map(d$main$media_id, ~ map_dfr(.x, ~ d$media %>% filter(media_id == .x)))
  
  d$main$referenced_tweets <- map(d$main$referenced_tweets, function(ref) {
    if (is.null(ref)) {
      # return empty structure with same attributes for data consistency #tidy
      return(structure(list(), .Names = character(0), row.names = integer(0), class = c("data.frame")))
    } else {
      # join primary keys with variables of referenced_tweets
      all_ref <- left_join(ref, d$tweets, by = c("id" = "ref_status_id"), keep = TRUE)
      return(all_ref)
    }
  })
  
  return(d)
}

unnest_references <- function(d) {
  x <- d$referenced_tweets
  
  # Create index
  x <- imap(x, function(x, ind) {
    if (nrow(x)>0)
      x$index <- ind
    else
      x <- data.frame(index=ind, type='quoted') # needs some type for pivot
    x
  })
  
  cat('Index created!\n')
  
  # Concat all references
  x <- do.call(plyr::rbind.fill, x) %>% 
    select(-id)  # can be found in ref_status_id
  
  cat('References concatinated!\n')
  
  # Type to cols
  x <- pivot_wider(x, id_cols = index, 
                   names_from = type, values_from = starts_with('ref_'),
                   names_glue = "{type}_{.value}") %>% 
    arrange(index)  %>%  # double check index is invariant
    select(-index) 
  
  cat('References unnested!\n')
  
  # Clean names
  names(x) <- names(x) %>% 
    str_remove_all('ref_') %>% 
    str_replace_all('replied_to_', 'replied_')
  
  # re-assign
  d <- cbind(d %>% select(-referenced_tweets), x)
  
  cat('References re-assigned!\n')
  return(d)
}
