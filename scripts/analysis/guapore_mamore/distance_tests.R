source(here("scripts", "database_prep", "dataprep_oncecoded.R"))
# distance
distdata <- glottodist(glottodata = glottodata_prepped)

nmdstest <- glottonmds(
  glottodist = distdata,
  k = 2,
  row2id = "glottocode",
  na.rm = TRUE
)

# groups
nmdstest$scoresdata <- left_join(
  nmdstest$scoresdata,
  glottodata_prepped$sample,
  "glottocode"
)

distance_plot <- glottoplot(
  glottonmds = nmdstest,
  color = "group",
  ptsize = 4,
  preventoverlap = TRUE,
  filename = paste(images, "distance_plot")
)

glottoplot(
  glottodist = distdata,
  label = "name"
)

# permanova

pair <- glottostat_permanova(glottodata_prepped,
  comparison = "pairwise"
)

# tests but with one group for Brazil and Bolivia

glottodata_onegroup <- glottoget(here("data", "guapore_mamore", "langsJoin-onegroup.xlsx"),
  meta = TRUE
)
glottodata_onegroup <- glottoconvert(
  data = glottodata_onegroup,
  var = "var_",
  table = "glottodata",
  remark = "notes",
  ref = "source"
)

distdata_onegroup <- glottodist(glottodata = glottodata_onegroup)

nmdstest_onegroup <- glottonmds(
  glottodist = distdata,
  k = 2,
  row2id = "glottocode",
  na.rm = TRUE
)

pair_onegroup <- glottostat_permanova(glottodata_onegroup,
  comparison = "pairwise"
)
