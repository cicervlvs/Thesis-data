###################################################
#### DO NOT SOURCE THE ENTIRE FILE ALL AT ONCE ####
############# CONTAINS ERRORS #####################
###################################################

guap_data_dir <- here("data", "guapore_mamore")

# After modifying langTablemod.txt manually
langs <- read.xlsx(here(guap_data_dir, "langTablemod.xlsx"),
  rowNames = FALSE,
  skipEmptyRows = FALSE
)

sample <- read.xlsx("database_proposal.xlsx",
  rowNames = FALSE,
  skipEmptyRows = FALSE
)

# Joining with the sample i already have, removing the unknown languages
langs_join <- langs %>%
  full_join(sample,
    by = "name"
  ) %>%
  select(-"Country.y") %>%
  rename("Country" = "Country.x") %>%
  dplyr::filter(name != "(unknown)")

add_glottocode_name(langs_join)

langs_join$glottocode <- glottolist

write.xlsx(
  langs_join,
  "langs_join.xlsx"
)

# After some frustrating editing with excel

# langs_join <- read.xlsx("langs_join.xlsx",
#                       rowNames = FALSE,
#                       skipEmptyRows = FALSE)%>%
#            select(glottocode, everything())

# glottoLangsJoin <- glottojoin(langs_join,
#                              with = "glottospace")
