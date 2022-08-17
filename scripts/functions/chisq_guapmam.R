chisq_guapmam <- function(df, control) {
  if (is.data.frame(df) == FALSE) {
    dfed <- as.data.frame(df)
  } else {
    dfed <- df
  }

  guapmam_values <- get_n(dfed, "Guaporé-Mamoré")

  control_values <- get_pct(dfed, control)

  output <- chisq.test(guapmam_values,
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
  output
}
