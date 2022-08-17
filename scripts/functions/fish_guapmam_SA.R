fish_guapmam_SA <- function(df) {
  guapmam_values <- get_n(df, "Guaporé-Mamoré") %>%
    unlist()

  control_values <- get_n(df, "South America") %>%
    unlist()

  fish_input <- table(
    guapmam_values,
    control_values
  )

  output <- fisher.test(fish_input)

  output
}
