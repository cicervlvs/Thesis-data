source(here("scripts", "database_prep", "dataprep_oncecoded.R"))

map <- glottomap(glottospace(
  glottosimplify(
    glottodata_guapmam
  ),
  method = "buffer",
  radius = 15
),
color = "family",
label = "name",
lbsize = .7,
alpha = .5,
rivers = TRUE,
filename = paste(images, "Guap_mam_map.png")
)

map_tone <- glottomap(glottospace(guapmam_simplified),
  color = "Has tone",
  label = "name",
  rivers = FALSE,
  lbsize = .7,
  filename = paste(images, "tone_map")
)

guapmam_simplified_nonostress <- guapmam_simplified %>%
  dplyr::filter(stress.type != "no.stress")

map_stresstype <- glottomap(glottospace(guapmam_simplified_nonostress),
  color = "Stress type",
  label = "name",
  rivers = FALSE,
  colorvec = colorvec,
  filename = paste(images, "stress_type_map")
)

sample_map <- glottomap(glottodata_plotting,
  color = "var_has.tone",
  label = "name",
  rivers = TRUE,
  colorvec = colorvec,
  filename = paste(images, "sample_map")
)
