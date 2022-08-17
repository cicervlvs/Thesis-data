fish_guapmam <- function(df, control) {
  guapmam_values <- get_n(df, "Guaporé-Mamoré") %>%
    unlist()

  control_values <- get_n(df, control) %>%
    unlist()

  fish_input <- table(
    guapmam_values,
    control_values
  )

  output <- fisher.test(fish_input)

  output
}
