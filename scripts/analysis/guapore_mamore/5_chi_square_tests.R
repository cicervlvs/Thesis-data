source(here("scripts", "database_prep", "dataprep_oncecoded.R"))

####################
## Chi-square tests##
####################

# Stress types

chisq_stress_type_world  <- chisq_guapmam_world(glottodata_stress_type_grouped)

stress_type_n_for_table_world <- bind_rows(
  glottodata_guapmam_for_stress_type_plot,
  weight_for_stress_type_plot,
  weight_for_stress_type_plot %>%
    select(!macroarea)
) %>%
  make_three_groups() %>%
  dplyr::filter(group == "Guaporé-Mamoré" |
    group == "World")

stress_type_n_for_table_SA <- bind_rows(
  glottodata_guapmam_for_stress_type_plot,
  weight_for_stress_type_plot,
  weight_for_stress_type_plot %>%
    select(!macroarea)
) %>%
  make_three_groups() %>%
  dplyr::filter(group == "Guaporé-Mamoré" |
    group == "South America")

fish_test_stress_types_world <- fisher.test(
  stress_type_n_for_table_world$stress.type.spaces,
  stress_type_n_for_table_world$group
)

fish_test_stress_types_SA <- fisher.test(
  stress_type_n_for_table_SA$stress.type.spaces,
  stress_type_n_for_table_SA$group
)

chisq_stress_types_SA <- chisq_guapmam_SA(
  glottodata_stress_type_grouped
)

fish_test_stress_types_SA <- fish_guapmam(
  glottodata_stress_type_grouped,
  "South America"
)

# Tone

chisq_tone_world <- chisq_guapmam_world(
  glottodata_tone_grouped
)

chisq_tone_SA <- chisq_guapmam_SA(
  glottodata_tone_grouped
)
# Boundedness

chisq_boundedness_world <- chisq_guapmam_world(
  glottodata_boundedness_grouped
)

chisq_boundedness_SA <- chisq_guapmam_SA(
  glottodata_boundedness_grouped
)
