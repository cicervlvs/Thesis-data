get_n <- function(df, groupp = NULL) {
  df %>%
    as.data.frame() %>%
    dplyr::filter(group == groupp) %>%
    select(n)
}
