library(tidyverse)
source("R/add_variables.R")

# Read in all extracted conversations
dat <- readRDS("data/forward-replies-final.rds")


### Merge data

dat <- dat %>% add_reply_variables()

# test reply structure
dat %>%
  filter(conversation_id == "1124678870609866757") %>%
  view_replies() %>%
  View()
