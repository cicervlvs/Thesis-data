join_wals_SA <- function(df) {
  left_join(df, languages, by = "wals.code") %>%
    dplyr::filter(.$macroarea == "South America")
}
