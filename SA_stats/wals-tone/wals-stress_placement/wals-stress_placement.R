library(dplyr)

languages <- read.delim("languoid.txt",
                        fileEncoding = "UTF-8") %>% select(wals.code, macroarea)

fixed <- read.delim("wals-stress_placement/fixed.txt",
                         fileEncoding = "UTF-8",
                         skip = 7) %>% select(name, wals.code, description)

weight <- read.delim("wals-stress_placement/weight.txt",
                    fileEncoding = "UTF-8",
                    skip = 7) %>% select(name, wals.code, description)



fixedSimple <- fixed %>% mutate(alignment =
                                case_when(
                                  description == "Ultimate" |
                                  description == "Penultimate" |
                                  description == "Antepenultimate" ~ 'Right',
                                  description == "Initial" |
                                  description == "Second" |
                                  description == "Third" ~ 'Left'
                                ))

weightSimple <- weight %>% mutate(alignment =
                                case_when(
                                  description == "Left-edge: First or second" |
                                  description == "Left-oriented: One of the first three" |
                                  description == "Combined: Right-edge and unbounded"
                                  ~ 'Right',
                                  description == "Right-edge: Ultimate or penultimate" |
                                  description == "Right-oriented: One of the last three"
                                  ~ 'Left',
                                  description == "Unbounded: Stress can be anywhere"
                                  ~ 'Unbounded'
                                ))


fixedAndWeight <- bind_rows(weightSimple, fixedSimple) %>%
                  left_join(languages, by = "wals.code") %>%
                  filter(.$alignment == "Left" |
                          .$alignment == "Right")

freqTabWorld <- table(fixedAndWeight$alignment)

# Proportions of alignment (left or right of the word) in languages of South America

alignmentSA <- fixedAndWeight %>% filter(.$macroarea == "South America")

freqTabSA <- table(alignmentSA$alignment)

propTabSA <- prop.table(freqTab)

# Proportions of placement within the window (trochaic or iambic)

fixedWindow <- fixedWithMacro %>% mutate(rhythm =
                                case_when(
                                  description == "Initial" |
                                  description == "Penultimate" |
                                  description == "Antepenultimate" ~ 'Trochaic',
                                  description == "Ultimate" |
                                  description == "Second" |
                                  description == "Third" ~ 'Iambic',
                                  description == "No Fixed Stress" ~ 'No Fixed stress'
                                ))

fixedWindowSA <- fixedWindow %>% filter(.$macroarea == "South America") 

rhythmTabSA <- table(fixedWindowSA$rhythm)

rhythmPropSA <- prop.table(rhythmTabSA)