remove_var <- function(df) {
  for (col in 1:ncol(df)) {
    rename_with(
      df,
      sub(
        "^var_",
        ""
      ),
      names(df)
    )
  }
}
