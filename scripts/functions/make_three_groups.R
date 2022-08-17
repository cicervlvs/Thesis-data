make_three_groups <- function(df) {
  mutate(df,
    group = case_when(
    !is.na(glottocode) == TRUE ~ "Guaporé-Mamoré",
    is.na(glottocode) == TRUE &
    is.na(macroarea) == TRUE ~ "World",
    TRUE ~ "South America"
  ))
}
