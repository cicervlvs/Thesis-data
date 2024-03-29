fish_guapmam <- function(df, control_value) {
  guapmam_values <- get_n(df, "Guaporé-Mamoré") %>%
    unlist()

  control_values <- get_n(df, control_value) %>%
    unlist()

  fish_input <- table(
    guapmam_values,
    control_values
  )

  output <- fisher.test(fish_input)

  output
}
