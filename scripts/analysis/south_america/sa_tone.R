languages <- read.delim(here("data", "south_america", "languoid.txt"),
  fileEncoding = "UTF-8"
) %>% select(wals.code, macroarea)

tone_values <- read.delim(here("data", "south_america", "tone", "tone.txt"),
  fileEncoding = "UTF-8",
  skip = 7
) %>%
  left_join(languages, by = c("wals.code"))

tone_simple <- tone_values %>% mutate(
  hasTone =
    if_else(.$description == "Complex tone system" |
      .$description == "Simple tone system",
    "tone",
    "no tone"
    )
)

tone_sa <- tone_simple %>%
  dplyr::filter(.$macroarea == "South America")

tab_simp_comp_tone <- table(tone_values$description)

simp_comp_tone_sa <- as.data.frame(tone_values %>%
  dplyr::filter(.$macroarea == "South America"))
tab_sim_comp_sa <- table(simp_comp_tone_sa$description)
tab_sim_comp_sa_only_tone <- table(simp_comp_tone_sa$description,
  exclude = "No tones"
)

tone_freq_tab <- table(tone_sa$hasTone)
tone_prop_tab <- prop.table(tone_freq_tab)
