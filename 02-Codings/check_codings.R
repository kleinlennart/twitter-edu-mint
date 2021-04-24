### Read in and check codings of maths and physics tweets ###

library(tidyverse)

# FIXME: file paths have changed

### Mathe ---------------------------------------------------------------
# Uncoded, clean data
mathe_base <- read_csv2("export/SAMPLE-Mathe-54988-JAN.csv", col_types = cols(.default = "c")) %>%
  mutate(Index = Index %>% as.integer()) %>%
  select(Index, text)

# Coded data, with mixed coders
all_mathe <- bind_rows(
  readxl::read_xlsx("coded/codings/Xenia/SAMPLE-Mathe-54988-JAN.xlsx", col_types = "text"),
  readxl::read_xlsx("coded/codings/Katharina/SAMPLE-Mathe-54988-JAN.xlsx", col_types = "text"),
  readxl::read_xlsx("coded/codings/Alix/SAMPLE-Mathe-54988-JAN.xlsx", col_types = "text")
)

# Use only coded entries
mathe_results <- all_mathe %>%
  filter(fachlich_mathe != "NA") %>%
  select(Index, status_id, user_id, fachlich_mathe) %>%
  rename(status_id_anon = status_id, user_id_anon = user_id) %>%
  distinct(status_id_anon, .keep_all = TRUE) %>%
  mutate(Index = Index %>% as.integer()) %>%
  arrange(Index)

# Left join with clean data for correct text
mathe_results <- left_join(mathe_results, mathe_base, by = "Index") %>%
  filter(fachlich_mathe == "1")

saveRDS(mathe_results, "coded/coded_mathe.rds")

### Physik --------------------------------------------------------

physik_base <- read_csv2("export/SAMPLE-Physik-25821-JAN.csv", col_types = cols(.default = "c")) %>%
  mutate(Index = as.integer(Index)) %>%
  select(Index, text)

physik_coding <- readxl::read_xlsx("coded/codings/Xenia/SAMPLE-Physik-25821-JAN.xlsx", col_types = "text")

physik_results <- physik_coding %>%
  filter(fachlich_physik != "NA") %>%
  select(Index, status_id, user_id, fachlich_physik) %>%
  rename(status_id_anon = status_id, user_id_anon = user_id) %>%
  distinct(status_id_anon, .keep_all = TRUE) %>%
  mutate(Index = Index %>% as.integer()) %>%
  arrange(Index)


physik_results <- left_join(physik_results, physik_base, by = "Index") %>%
  filter(fachlich_physik == "1")

saveRDS(physik_results, "coded/coded_physik.rds")

### Mathe / Physik --------------------------------------------------------

mathe_physik_base <- read_csv2("export/SAMPLE-Mathe_Physik-15832-JAN.csv", col_types = cols(.default = "c")) %>%
  mutate(Index = as.integer(Index)) %>%
  select(Index, text)

mathe_physik_coding <- readxl::read_xlsx("coded/codings/Katharina/SAMPLE-Mathe_Physik-15832-JAN.xlsx", col_types = "text")

mathe_physik_results <- mathe_physik_coding %>%
  # filter(fachlich_physik != "NA") %>%
  select(Index, status_id, user_id, fachlich_physik, fachlich_mathe) %>%
  rename(status_id_anon = status_id, user_id_anon = user_id) %>%
  distinct(status_id_anon, .keep_all = TRUE) %>%
  mutate(Index = Index %>% as.integer()) %>%
  arrange(Index)


mathe_physik_results <- left_join(mathe_physik_results, mathe_physik_base, by = "Index") %>% 
  filter(fachlich_mathe == "1" | fachlich_physik == "1")

saveRDS(mathe_physik_results, "coded/coded_mathe_physik.rds")
