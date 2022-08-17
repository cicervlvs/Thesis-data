get_pct <- function(df, group_value) {
  df %>%
    as.data.frame() %>%
    dplyr::filter(group == group_value) %>%
    select(ends_with(".pct")) %>%
    unlist()
}
