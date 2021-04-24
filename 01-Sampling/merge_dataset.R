### Merge Sampled Datasets and export to csv for coding ###

### Read in data
basic_methods <- readRDS("../OUTPUTS/basic_methods.rds")
mathe_results <- readRDS("../OUTPUTS/mathe_results_all2.rds")
physik_results <- readRDS("../OUTPUTS/physik_results_all2.rds")


dat <- bind_rows(basic_methods, mathe_results, physik_results)

sample <- dat %>%
  group_by(status_id) %>%
  mutate(
    matched_subjects = paste(subject, collapse = ", "),
    methods = paste(method, collapse = ", ")
  ) %>%
  select(-subject, -method) %>%
  ungroup()


sample <- sample %>%
  filter(!duplicated(status_id)) %>%
  mutate(
    tweet_url = na_if(
      str_extract(text, regex("http\\S+|https\\S+|www.\\S+", ignore_case = TRUE)) %>% as.vector(),
      "character(0)"
    )
  ) %>%
  select(status_id, user_id, methods, matched_subjects, tweet_url, text) %>%
  mutate(
    fachlich_mathe = NA,
    fachlich_physik = NA
  )

# Unnest List Column to vector column
sample$tweet_url <- map_chr(sample$tweet_url, pluck(1))


# final <- sample %>%
#   arrange(matched_subjects, methods)

only_mathe <- sample %>%
  filter(!str_detect(matched_subjects, "Physik")) %>%
  rownames_to_column("Index")
only_physik <- sample %>%
  filter(!str_detect(matched_subjects, "Mathe")) %>%
  rownames_to_column("Index")

ambivalent <- anti_join(sample, bind_rows(only_mathe, only_physik)) %>%
  rownames_to_column("Index")


# Export Data -------------------------------------------------------------
write_csv2(only_mathe, paste0(here::here("data", "export"), "SAMPLE-Mathe-", nrow(only_mathe), "-JAN", ".csv"))
write_csv2(only_physik, paste0(here::here("data", "export"), "SAMPLE-Physik-", nrow(only_physik), "-JAN", ".csv"))
write_csv2(ambivalent, paste0(here::here("data", "export"), "SAMPLE-Mathe_Physik-", nrow(ambivalent), "-JAN", ".csv"))
