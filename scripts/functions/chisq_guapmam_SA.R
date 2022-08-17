chisq_guapmam_SA <- function(df) {
  guapmam_values <- df %>%
    as.data.frame() %>%
    dplyr::filter(group == "Guaporé-Mamoré") %>%
    select(n) %>%
    unlist() %>%
    unname()

  control_values <- df %>%
    as.data.frame() %>%
    dplyr::filter(group == "South America") %>%
    select(ends_with(".pct")) %>%
    unlist() %>%
    unname()

  chisq.test(guapmam_values,
    p = control_values
  )

  # apa_style_report <- paste0(
  #   "$\\chi ^{2}$ ",
  #   "(", output$parameter,
  #   ", \\textit{N} = ",
  #   sum(output$observed),
  #   ") = ",
  #   round(output$statistic, 2),
  #   ", \\textit{p} =",
  #   round(output$p.value, 3)
  # )

  # returns <- list(
  #   "results" = output,
  #   "report" = unlist(apa_style_report)
  # )

  # returns
}
