image_folder <- here("scripts", "analysis", "images")

languages <- read.delim(here("data", "south_america", "languoid.txt"),
                        fileEncoding = "UTF-8"
                        ) %>% select(wals.code, macroarea)

fixed <- read.delim(here("data", "south_america", "stress", "fixed.txt"),
                         fileEncoding = "UTF-8",
                         skip = 7) %>%
                         select(name, wals.code, description, family) %>%
                         left_join(languages, by = "wals.code")
                         
weight <- read.delim(here("data", "south_america", "stress", "weight.txt"),
                    fileEncoding = "UTF-8",
                    skip = 7) %>%
                    select(name, wals.code, description, family) %>%
                    left_join(languages, by = "wals.code")

fixed_simple <- fixed %>% mutate(alignment =
                                case_when(
                                  description == "Ultimate" |
                                  description == "Penultimate" |
                                  description == "Antepenultimate" ~ "Right",
                                  description == "Initial" |
                                  description == "Second" |
                                  description == "Third" ~ "Left"
                                )) %>%
                            drop_na() %>%
                            as.data.frame()

plot_fixed_alignment <- ggplot(fixed_simple,
  aes(alignment)) +
  geom_bar(position =  "dodge", fill = "cornflowerblue") +
  labs(title = "Left vs right bound fixed stress systems in the languages of the world",
       x = "Bind position") +
  theme_minimal()
ggsave("alignment_fixed.png",
       path = image_folder)

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
                                )) %>%
                            drop_na()

fixed_and_weight <- bind_rows(weight_simple,
                              fixed_simple) %>%
                  dplyr::filter(.$alignment == "Left" |
                         .$alignment == "Right" |
                         .$alignment == "Ubounded")

plot_alignment <- ggplot(fixed_and_weight,
  aes(alignment)) +
  geom_bar(position =  "dodge", fill = "cornflowerblue") +
  labs(title = "Left vs. right bound stress systems in the languages of the world",
       x = "Bind position") +
  theme_minimal()
ggsave("alignment_world.png",
       path = image_folder)


freq_tab_world <- table(fixed_and_weight$alignment)

# Proportions of alignment (left or right of the word) in languages of South America

alignment_sa <- dplyr::filter(fixed_and_weight,
                             macroarea == "South America")

freq_tab_sa <- table(alignment_sa$alignment)

prop_tab_sa <- prop.table(freq_tab_sa)

# Proportions of placement within the window (trochaic or iambic)

fixed_window <- fixed %>% mutate(rhythm =
                                case_when(
                                  description == "Initial" |
                                  description == "Penultimate" |
                                  description == "Antepenultimate" ~ "Trochaic",
                                  description == "Ultimate" |
                                  description == "Second" |
                                  description == "Third" ~ "Iambic",
                                  description == "No fixed stress" ~ "No fixed stress"
                                ))

fixed_window_forplot <- fixed_window %>%
                             dplyr::filter(rhythm != "No fixed stress") %>%
                        mutate(rhythm_for_plot =
                                 case_when(
                                   rhythm == "Iambic" ~ "Left",
                                   rhythm == "Trochaic" ~ "Right"))
                       

plot_window <- ggplot(fixed_window_forplot,
  aes(rhythm_for_plot)) +
  geom_bar(position =  "dodge", fill = "cornflowerblue") +
  labs(title = "Left vs. right internal window alignment in languages of the world with fixed stress",
       x = "Window-internal alignment") +
  theme_minimal()
ggsave("window_internal_alignment_world.png",
       path = image_folder)

fixed_window_costa <- data.frame(Orientation = c(rep("Left", 55),
                                               rep("Right", 45)))

plot_window_costa <- ggplot(fixed_window_costa,
  aes(Orientation)) +
  geom_bar(position =  "dodge", fill = "cornflowerblue") +
  labs(title = "Internal window alignment in languages of South America with fixed stress") +
  theme_minimal()
ggsave("window_internal_alignment_sa.png",
       path = image_folder)

fixed_window_sa <- fixed_window %>% dplyr::filter(.$macroarea == "South America")

rhythm_tab_sa <- table(fixed_window_sa$rhythm)

rhythm_prop_sa <- prop.table(rhythm_tab_sa)

# Proportions of stress types by family in SA

fixed_by_family_sa <- fixed %>%
                  dplyr::filter(.$macroarea == "South America") %>%
                  dplyr::filter(.$description != "No fixed stress")

fixed_by_family_tab_sa <- table(fixed_by_family_sa$family)

fixed_by_family_sa_unique <- length(unique(fixed_by_family_sa$family))

weight_by_family_sa <- weight %>%
                  dplyr::filter(.$macroarea == "South America") %>%
                  dplyr::filter(.$description != "Not predictable") %>%
                  dplyr::filter(.$description != "Fixed stress (no weight-sensitivity)")


weight_by_family_tab_sa <- table(weight_by_family_sa$family)

weight_by_family_sa_unique <- length(unique(weight_by_family_sa$family))

unpredict_by_family_sa <- weight %>%
                  dplyr::filter(.$macroarea == "South America") %>%
                  dplyr::filter(.$description == "Not predictable")
