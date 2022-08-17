source(here("scripts", "database_prep", "dataprep_oncecoded.R"))

#PLOTS FOR FEATURES

plot_has_tone <- ggplot(glottodata_guapmam,
                               aes(has.tone)) +
  geom_bar(position =  "dodge", fill = "maroon") +
  labs(title = "Presence of tone in the languages of Guaporé-Mamoré",
       x = "Has tone") +
  theme_minimal() +
  scale_fill_manual(values = cb_palette)
save_to_images("tone_guap_mam")

glottodata_nona <- glottodata_guapmam %>%
                  drop_na(tone.type)

plot_tone_type <- ggplot(glottodata_nona,
                               aes(tone.type)) +
  geom_bar(position =  "dodge",
           fill = "maroon") +
  labs(title = "Tone system complexity in languages with tone of Guaporé-Mamoré",
       x = "Tone type") +
  theme_minimal() +
  scale_fill_manual(values = cb_palette)
  
save_to_images("tone_type_guap_mam")

plot_fixed_vs_weight <- ggplot(glottodata_guapmam,
                               aes(stress.type.spaces)) +
  geom_bar(position = "dodge", fill = "maroon") +
  labs(title = "Stress types in the languages of Guaporé-Mamoré",
       x = "Stress type") +
  scale_fill_manual(values = cb_palette)
save_to_images("stress_type_guap_mam")

plot_fixed_alignment_guapmam <- ggplot(subset(glottodata_guapmam, stress.type == "fixed"),
                               aes(fixed.alignment)) +
  geom_bar(position = "dodge", fill = "maroon") +
  labs(title = "Boundedness in languages with fixed stress of Guaporé-Mamoré",
       x = "Binding") +
  scale_fill_manual(values = cb_palette)
save_to_images("alignment_fixed_stress")

glottodata_guapmam$fixed.stress.position <- factor(glottodata_guapmam$fixed.stress.position,
                                                   levels = c("initial", "second", "third",
                                                              "antepenultimate", "penultimate", "final"))

plot_fixed_position <- ggplot(subset(glottodata_guapmam, stress.type == "fixed"),
                               aes(fixed.stress.position)) +
  geom_bar(position = "dodge") +
  labs(title = "Position of fixed stress in languages of Guaporé-Mamoré",
       x = "Fixed stress position") +
  scale_fill_manual(values = cb_palette)
save_to_images("position_fixed_stress")

glottodata_guapmam$weight.alignment <- factor(glottodata_guapmam$fixed.stress.position,
                                                   levels = c("left", "right", "none"))

plot_weight_alignment <- ggplot(subset(glottodata_guapmam,
                                       stress.type == "weight.sensitive"),
                               aes(weight.alignment)) +
  geom_bar(position = "dodge", fill = "maroon") +
  labs(title = "Alignment in languages with weight-sensitive stress of Guaporé-Mamoré") +
  scale_fill_manual(values = cb_palette)
save_to_images("alignment_weight_stress")
