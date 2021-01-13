# custom convenience function for wider tables (with kableExtra)
View.tweets <- function(x) {
  kable(x) %>% kable_styling("striped", full_width = TRUE)
}
