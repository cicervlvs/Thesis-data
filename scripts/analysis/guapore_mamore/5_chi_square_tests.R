source(here("scripts", "database_prep", "dataprep_oncecoded.R"))

####################
## Chi-square tests##
####################

# Stress types

guapmam_values <- glottodata_stress_type_grouped %>%
  as.data.frame() %>%
  dplyr::filter(group == "Guaporé-Mamoré") %>%
  select(n)

control_values <- glottodata_stress_type_grouped %>%
  as.data.frame() %>%
  dplyr::filter(group == "World") %>%
  select(ends_with(".pct"))

chisq.test(guapmam_values, p = control_values)

stress_type_n_for_table_world <- bind_rows(
  glottodata_guapmam_for_stress_type_plot,
  weight_for_stress_type_plot,
  weight_for_stress_type_plot %>%
    join_wals_SA()
) %>%
  make_three_groups() %>%
  dplyr::filter(group == "Guaporé-Mamoré" |
    group == "World")

stress_type_n_for_table_SA <- bind_rows(
  glottodata_guapmam_for_stress_type_plot,
  weight_for_stress_type_plot,
  weight_for_stress_type_plot %>%
    join_wals_SA()
) %>%
  make_three_groups() %>%
  filter(group == "Guaporé-Mamoré" |
    group == "South America")

fish_test_stress_types_world <- fisher.test(
  stress_type_n_for_table_world$stress.type.spaces,
  stress_type_n_for_table_world$group
)

fish_test_stress_types_SA <- fisher.test(
  stress_type_n_for_table_SA$stress.type.spaces,
  stress_type_n_for_table_SA$group
)

chisq_stress_types_SA <- chisq_guapmam(
  glottodata_stress_type_grouped,
  "South America"
)

fish_test_stress_types_SA <- fish_guapmam(
  glottodata_stress_type_grouped,
  "South America"
)

# Tone

chisq_tone_world <- chisq_guapmam(
  glottodata_tone_grouped,
  "World"
)

chisq_tone_SA <- chisq_guapmam(
  glottodata_tone_grouped,
  "South America"
)
# Boundedness

chisq_boundedness_world <- chisq_guapmam(
  glottodata_boundedness_grouped,
  "World"
)

chisq_boundedness_SA <- chisq_guapmam(
  glottodata_boundedness_grouped,
  "South America"
)
