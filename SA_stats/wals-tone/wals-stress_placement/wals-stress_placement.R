library(dplyr)

languages <- read.delim("../languoid.txt",
                        fileEncoding = "UTF-8") %>% select(wals.code, macroarea)

fixed <- read.delim("fixed.txt",
                         fileEncoding = "UTF-8",
                         skip = 7) %>%
                         select(name, wals.code, description, family)
                         
weight <- read.delim("weight.txt",
                    fileEncoding = "UTF-8",
                    skip = 7) %>% select(name, wals.code, description, family)

fixed_simple <- fixed %>% mutate(alignment =
                                case_when(
                                  description == "Ultimate" |
                                  description == "Penultimate" |
                                  description == "Antepenultimate" ~ "Right",
                                  description == "Initial" |
                                  description == "Second" |
                                  description == "Third" ~ "Left"
                                ))

weight_simple <- weight %>% mutate(alignment =
                                case_when(
                                  description == "Left-edge: First or second" |
                                  description == "Left-oriented: One of the first three" |
                                  description == "Combined: Right-edge and unbounded"
                                  ~ "Right",
                                  description == "Right-edge: Ultimate or penultimate" |
                                  description == "Right-oriented: One of the last three"
                                  ~ "Left",
                                  description == "Unbounded: Stress can be anywhere"
                                  ~ "Unbounded"
                                ))


fixed_and_weight <- bind_rows(weight_simple, fixed_simple) %>%
                  left_join(languages, by = "wals.code") %>%
                  filter(.$alignment == "Left" |
                          .$alignment == "Right")

freq_tab_world <- table(fixed_and_weight$alignment)

# Proportions of alignment (left or right of the word) in languages of South America

alignment_sa <- fixed_and_weight %>% filter(.$macroarea == "South America")

freq_tab_sa <- table(alignment_sa$alignment)

prop_tab_sa <- prop.table(freq_tab_sa)

# Proportions of placement within the window (trochaic or iambic)

fixed_with_macro  <- fixed %>%
                  left_join(languages, by = "wals.code")

fixed_window <- fixed_with_macro %>% mutate(rhythm =
                                case_when(
                                  description == "Initial" |
                                  description == "Penultimate" |
                                  description == "Antepenultimate" ~ "Trochaic",
                                  description == "Ultimate" |
                                  description == "Second" |
                                  description == "Third" ~ "Iambic",
                                  description == "No Fixed Stress" ~ "No Fixed stress"
                                ))

fixed_window_sa <- fixed_window %>% filter(.$macroarea == "South America")

rhythm_tab_sa <- table(fixed_window_sa$rhythm)

rhythm_prop_sa <- prop.table(rhythm_tab_sa)

# Proportions of stress types by family in SA

fixed_by_family_sa <- fixed %>%
                  left_join(languages, by = "wals.code") %>%
                  filter(.$macroarea == "South America") %>%
                  filter(.$description != "No fixed stress")

fixed_by_family_tab_sa <- table(fixed_by_family_sa$family)

fixed_by_family_sa_unique <- length(unique(fixed_by_family_sa$family))

weight_by_family_sa <- weight %>%
                  left_join(languages, by = "wals.code") %>%
                  filter(.$macroarea == "South America") %>%
                  filter(.$description != "Not predictable") %>%
                  filter(.$description != "Fixed stress (no weight-sensitivity)")


weight_by_family_tab_sa <- table(weight_by_family_sa$family)

weight_by_family_sa_unique <- length(unique(weight_by_family_sa$family))

unpredict_by_family_sa <- weight %>%
                  left_join(languages, by = "wals.code") %>%
                  filter(.$macroarea == "South America") %>%
                  filter(.$description == "Not predictable")
