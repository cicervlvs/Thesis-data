source(here("scripts", "database_prep", "dataprep_oncecoded.R"))

languages <- read.delim(here("data", "south_america", "languoid.txt"),
  fileEncoding = "UTF-8"
) %>%
  select(wals.code, macroarea)

#####################
##grouped bar plots##
#####################

#Fixed stress position
fixed_for_grouping <- fixed_simple %>%
                      rename(fixed.stress.position = description) %>%
                      dplyr::filter(!is.na(fixed.stress.position) &
                                    fixed.stress.position != "No fixed stress")
  
glottodata_fixed_grouped <- bind_rows(glottodata_guapmam %>%
                                      dplyr::filter(!is.na(fixed.stress.position)) %>%
                                      mutate(fixed.stress.position =
                                               case_when(
                                                 fixed.stress.position == "initial" ~ "Initial",
                                                 fixed.stress.position == "second" ~ "Second",
                                                 fixed.stress.position == "third" ~ "Third",
                                                 fixed.stress.position == "antepenultimate" ~ "Antepenultimate",
                                                 fixed.stress.position == "penultimate" ~ "Penultimate",
                                                 fixed.stress.position == "final" ~ "Ultimate")),
                                   fixed_for_grouping,
                                   fixed_for_grouping %>%
                                     dplyr::filter(.$macroarea == "South America")) %>%
                                 make_three_groups() %>%
                            count(fixed.stress.position, group) %>%
                            group_by(group) %>%
                            mutate(fixed.stress.position.pct =
                                     n / sum(n)) %>%
                            as.data.frame()
                            
glottodata_fixed_grouped$fixed.stress.position <- factor(
                                                  glottodata_fixed_grouped$fixed.stress.position,
                                                  levels = c("Initial", "Second", "Third",
                                                              "Antepenultimate", "Penultimate", "Ultimate"))

plot_fixed_position_grouped <- ggplot(glottodata_fixed_grouped,
                                      aes(x = fixed.stress.position,
                                          y = fixed.stress.position.pct,
                                          fill = group)) +
                              geom_col(position = "dodge") +
                              labs(title = "Fixed stress positions in languages of Guaporé-Mamoré, South America,\n and the world",
                                   x = "Fixed stress position",
                                   y = "Rate") +
                              scale_fill_manual(values = cb_palette) +
                              theme_minimal()
save_to_images("position_fixed_stress_grouped")

#Presence of tone
tone_for_grouping <- tone_simple %>%
                        rename(has.tone = hasTone) %>%
                        mutate(has.tone = case_when(
                                 has.tone == "tone" ~ "yes",
                                 has.tone == "no tone" ~ "no"))

glottodata_tone_grouped <- bind_rows(glottodata_guapmam,
                                     tone_for_grouping,
                                     tone_for_grouping %>%
                            dplyr::filter(.$macroarea == "South America")) %>%
                            make_three_groups() %>%
                            count(has.tone, group) %>%
                            group_by(group) %>%
                            mutate(has.tone.pct =
                                     n / sum(n))

glottodata_tone_grouped$has.tone <- factor(glottodata_tone_grouped$has.tone,
                                           levels = c("yes", "no"))

plot_has_tone_grouped <- ggplot(glottodata_tone_grouped,
                                      aes(x = has.tone,
                                          y = has.tone.pct,
                                          fill = group)) +
                              geom_col(position = "dodge") +
                              labs(title = "Presence of tone in languages of Guaporé-Mamoré, South America, and the world",
                                   x = "Has tone",
                                   y = "Rate") +
                              scale_fill_manual(values = cb_palette)
save_to_images("tone_guap_mam_grouped")

#Boundedness
fixed_and_weight_for_plotting <- fixed_and_weight %>%
                                 rename(bound.window = alignment)
  
glottodata_guapmam_for_boundedness_plot <- glottodata_guapmam %>%
                                              mutate(bound.window =
                                                       case_when(
                                                         bound.window == "left" ~ "Left",
                                                         bound.window == "right" ~ "Right"
                                                       )) %>%
                                              dplyr::filter(!is.na(.$bound.window))

glottodata_boundedness_grouped <- bind_rows(glottodata_guapmam_for_boundedness_plot,
                                            fixed_and_weight_for_plotting,
                                            fixed_and_weight_for_plotting %>%
                                dplyr::filter(.$macroarea == "South America")) %>%
                                make_three_groups() %>%
                                 count(bound.window, group) %>%
                                 group_by(group) %>%
                                 mutate(bound.window.pct =
                                     n / sum(n))
   
plot_boundedness_grouped <- ggplot(glottodata_boundedness_grouped,
                                      aes(x = bound.window,
                                          y = bound.window.pct,
                                          fill = group)) +
                              geom_col(position = "dodge") +
                              labs(title = "Window-boundedness in languages of Guaporé-Mamoré and the world with\n bound windows",
                                   x = "Binding",
                                   y = "Rate") +
                              scale_fill_manual(values = cb_palette)
save_to_images("alignment_grouped")
                                  

#Stress type

weight_for_stress_type_plot <- weight %>%
                                mutate(stress.type.spaces =
                                        case_when(
                                         description == "Not predictable" ~ "Lexically determined",
                                         description == "Fixed stress (no weight-sensitivity)" ~ "Fixed",
                                         TRUE ~ "Weight-sensitive"))

glottodata_guapmam_for_stress_type_plot <- glottodata_guapmam %>%
                                          dplyr::filter(stress.type.spaces != "No stress")

glottodata_stress_type_grouped <- bind_rows(glottodata_guapmam_for_stress_type_plot,
                                            weight_for_stress_type_plot,
                                            weight_for_stress_type_plot %>%
                                     dplyr::filter(.$macroarea == "South America")) %>%
                                 make_three_groups() %>%
                                 count(stress.type.spaces, group) %>%
                                 group_by(group) %>%
                                 mutate(stress.type.spaces.pct =
                                     n / sum(n))
                                  
plot_stress_type_grouped <- ggplot(glottodata_stress_type_grouped,
                                      aes(x = stress.type.spaces,
                                          y = stress.type.spaces.pct,
                                          fill = group)) +
                              geom_col(position = "dodge") +
                              labs(title = "Stress types in languages of Guaporé-Mamoré, South-America,\n and the world with bound windows",
                                   x = "Stress type",
                                   y = "Rate") +
                              scale_fill_manual(values = cb_palette)
save_to_images("stress_type_guap_mam_grouped")
