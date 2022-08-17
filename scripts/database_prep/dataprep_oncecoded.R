sa_stats <- here("scripts", "analysis", "south_america")
source(here(sa_stats, "sa_stress.R"))
source(here(sa_stats, "sa_tone.R"))

# After coding more or less all the languages, add the ones with information on them
langsJoin_glottoprep <- (here("data", "guapore_mamore", "langsJoin-glottoprep.xlsx"))

glottodata <- read.xlsx(langsJoin_glottoprep,
  sheet = "glottodata"
) %>%
  subset(!is.na(var_has.tone)) %>% # (if has.tone column is blank, then there is no info)
  dplyr::select(
    glottocode,
    starts_with("var_"),
    notes,
    source
  )

glottostruc <- read.xlsx(langsJoin_glottoprep,
  sheet = "structure"
) %>%
  as.data.frame()
glottogroup <- read.xlsx(langsJoin_glottoprep,
  sheet = "sample"
) %>%
  as.data.frame()

write.xlsx(
  list(
    "glottodata" = glottodata,
    "structure" = glottostruc,
    "sample" = glottogroup
  ),
  here("data", "guapore_mamore", "langsJoin-ready.xlsx")
)

glottodata_prepped <- glottoget(here(
  "data", "guapore_mamore",
  "langsJoin-ready.xlsx"
),
meta = TRUE
)

glottodata_plotting <- glottojoin(glottodata_prepped$glottodata,
  with = "glottobase"
)

glottodata_prepped <- glottoconvert(
  data = glottodata_prepped,
  var = "var_",
  table = "glottodata",
  remark = "notes",
  ref = "source"
)

##add stress types
glottodata_prepped$glottodata <- glottodata_prepped$glottodata %>%
                          mutate(stress.type =
                                 (case_when(fixed.stress == "yes" ~ "fixed",
                                            lexically.determined.stress == "yes" ~ "lexically.determined",
                                            weight.stress == "yes" ~ "weight.sensitive",
                                            TRUE ~ "no.stress"))) %>%
                          mutate(fixed.stress.position =
                                   case_when(stress.type == "fixed" &
                                               bound.window == "left" &
                                               window.internal.orientation.left == "yes" &
                                               nonperiphery == "no" ~ "initial",
                                             stress.type == "fixed" &
                                               bound.window == "left" &
                                               window.internal.orientation.right == "yes" &
                                               nonperiphery == "no" ~ "second",
                                             stress.type == "fixed" &
                                               bound.window == "left" &
                                               window.internal.orientation.right == "yes" &
                                               nonperiphery == "yes" ~ "third",
                                             stress.type == "fixed" &
                                               bound.window == "right" &
                                               window.internal.orientation.left == "yes" &
                                               nonperiphery == "yes" ~ "antepenultimate",
                                             stress.type == "fixed" &
                                               bound.window == "right" &
                                               window.internal.orientation.left == "yes" &
                                               nonperiphery == "no" ~ "penultimate",
                                             stress.type == "fixed" &
                                               bound.window == "right" &
                                               window.internal.orientation.right == "yes" ~ "final")) %>%
                          mutate(weight.alignment =
                                   case_when(stress.type == "weight.sensitive" &
                                               bound.window == "left" ~ "left",
                                             stress.type == "weight.sensitive" &
                                               bound.window == "right" ~ "right",
                                             stress.type == "weight.sensitive" &
                                               bound.window == "none" ~ "none",
                                             stress.type == "weight.sensitive" &
                                               TRUE ~ "none")) %>%
                          mutate(fixed.alignment =
                                   case_when(stress.type == "fixed" &
                                               bound.window == "left" ~ "left",
                                             stress.type == "fixed" &
                                               bound.window == "right" ~ "right"))

##remove control languages
guapmam_langs <- glottodata_prepped$sample %>%
                dplyr::filter(.$group != "Control") %>%
                dplyr::select(glottocode)

glottodata_guapmam <- glottodata_prepped$glottodata %>%
  right_join(guapmam_langs,
    by = "glottocode") %>%
glottojoin(with = "glottobase") %>%
    mutate(stress.type.spaces =
    case_when(
      stress.type == "fixed" ~ "Fixed",
      stress.type == "lexically.determined" ~ "Lexically determined",
      stress.type == "no.stress" ~ "No stress",
      stress.type == "weight.sensitive" ~ "Weight-sensitive"))

guapmam_simplified <- glottosimplify(glottodata_guapmam) %>%
  rename("Has tone" = has.tone) %>%
  dplyr::mutate("Stress type" = case_when(
    .$stress.type == "weight.sensitive" ~ "Weight-sensitive",
    .$stress.type == "lexically.determined" ~ "Lexically determined",
    .$stress.type == "fixed" ~ "Fixed"
  ))
