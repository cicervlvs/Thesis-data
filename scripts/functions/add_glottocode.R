add_glottocode <- function(db) {
  db <- db %>%
    mutate_if(is.logical, as.character)

  db$glottocode_gathered <- NA

  name_column <- db$name

  code_list <- list()

  for (lang in name_column) {
    glotto_q <- glottosearch(
      search = lang,
      columns = "name",
      tolerance = 0.2
    )

    if (is.na(glotto_q) == TRUE) {
      glotto_q <- glottosearch(
        search = lang,
        columns = "name",
        tolerance = 0.3
      )
    } else if (substr(glotto_q, 1, 4) != substr(tolower(lang), 1, 4)) {
      glotto_q <- glottosearch(
        search = lang,
        columns = "name",
        tolerance = 0
      )
    } else {}


    code_list[[length(code_list) + 1]] <- glotto_q$glottocode[1]
  }
  print(code_list)
  .GlobalEnv$glottolist <- code_list
  .GlobalEnv$db$glottocode <- code_list
  print(.GlobalEnv$db$glottocode)
}
