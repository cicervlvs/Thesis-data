library(tidyverse)
library(glottospace)
library(stringr)
library(openxlsx)
library(rgdal)
library(ggplot2)

#After coding more or less all the languages, add the ones with information on them 
glottodata <- subset(read.xlsx("langsJoin-glottoprep.xlsx"),
                     !is.na(var_has.tone)) %>% #(if has.tone column is blank, then there is no info)
                     select("glottocode", 
                              starts_with("var_"),
                            "notes",
                            "source"
                            )%>%
                    as.data.frame()

glottostruc <- read.xlsx("langsJoin-glottoprep.xlsx",
                         sheet = "structure")
glottogroup <- read.xlsx("langsJoin-glottoprep.xlsx",
                         sheet = "sample")

glottocheck(glottodata)

glottodata <- glottojoin(glottodata, with = "glottobase")

write.xlsx(list("glottodata" = glottodata,
                "structure"  = glottostruc,
                "sample" = glottogroup),
           "langsJoin-ready.xlsx")

#map plotting
map <- glottomap(glottospace(glottodata,
                             method = "buffer",
                             radius = 15),
                 label = "name",
                 lbsize = 1,
                 alpha = 1,
                 rivers = TRUE,
                 filename = "Guap_mam_map.png")

#distance
glottodata_prepped <- glottoget("langsJoin-ready.xlsx",
                                meta = TRUE)

glottodata_prepped <- glottoconvert(data = glottodata_prepped,
              var = "var_",
              table = "glottodata",
              remark = "notes",
              ref = "source")

distdata <- glottodist(glottodata = glottodata_prepped)

nmdstest <- glottonmds(glottodist = distdata,
                       k = 2,
                       row2id = "glottocode",
                       rm.na = TRUE)
#groups
nmdstest$scoresdata <- dplyr::left_join(nmdstest$scoresdata,
                                 glottodata_prepped$sample,
                                 "glottocode")
colorvec <- c("no" = "red", 
              "yes" = "blue")

glottoplot(glottonmds = nmdstest,
           color = "group",
           ptsize = 10,
           #colorvec = colorvec,
           preventoverlap = TRUE)

glottoplot(glottodist = distdata,
           label = "name")

#permanova

pair<-glottostat_permanova(glottodata_prepped,
                           comparison = "pairwise")

#plots for features
glottodata_simplified <- glottosimplify(glottodata_prepped)

glottomap(glottodata_simplified,
         # group = "Bolivia",
          color = "has.tone",
          label = "name",
          rivers = TRUE,
          colorvec = colorvec)
#overview without control languages

#remove control languages
guapmam_langs <- glottodata_prepped$sample %>%
                filter(group != "Control") %>%
                select(glottocode)
#add stress types
glottodata_prepped$glottodata <- glottodata_prepped$glottodata %>%
                          mutate(stress.type =
                                 (case_when(fixed.stress == "yes" ~ "fixed",
                                            weight.stress == "yes" ~ "weight.sensitive",
                                            lexically.determined.stress == "yes" ~ "lexically.determined",
                                            TRUE ~ "no.stress")))%>%
                          mutate(fixed.stress.position =
                                   case_when(stress.type == "fixed" &
                                               bound.window == "left" &
                                               window.internal.orientation.left == "yes"&
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
                                               window.internal.orientation.right == "yes" ~ "final"))%>%
                          mutate(weight.alignment = 
                                   case_when(stress.type == "weight.sensitive" &
                                               bound.window == "left" ~ "left",
                                             stress.type == "weight.sensitive" &
                                               bound.window == "right" ~ "right",
                                             stress.type == "weight.sensitive" &
                                               bound.window == "none" | TRUE ~ "none"))%>%
                          mutate(fixed.alignment = 
                                   case_when(stress.type == "fixed" &
                                               bound.window == "left" ~ "left",
                                             stress.type == "fixed" &
                                               bound.window == "right" ~ "right"))
                          



glottodata_guapmam <- glottodata_prepped$glottodata %>%
                    right_join(guapmam_langs,
                               by = "glottocode")



theme_set(theme_minimal())

plot_has_tone <- ggplot(glottodata_guapmam,
                               aes(has.tone))+
  geom_bar(position =  "dodge", fill = "maroon")+
  labs(title = "Presence of tone in the languages of Guaporé-Mamoré")+
  theme_minimal()

glottodata_nona <- glottodata_guapmam %>%
                  drop_na(tone.type)

plot_tone_type <- ggplot(glottodata_nona,
                               aes(tone.type))+
  geom_bar(position =  "dodge",
           fill = "maroon")+
  labs(title = "Presence of tone in the languages of Guaporé-Mamoré")+
  theme_minimal()

plot_fixed_vs_weight <- ggplot(glottodata_guapmam,
                               aes(stress.type))+
  geom_bar(position = "dodge", fill = "maroon")+
  labs(title = "Stress types in the languages of Guaporé-Mamoré")

plot_fixed_alignment <- ggplot(subset(glottodata_guapmam, stress.type == "fixed"),
                               aes(fixed.alignment))+
  geom_bar(position = "dodge", fill = "maroon")+
  labs(title = "Alignment in languages with fixed stress of Guaporé-Mamoré")

plot_weight_alignment <- ggplot(subset(glottodata_guapmam, stress.type == "weight.sensitive"),
                               aes(weight.alignment))+
  geom_bar(position = "dodge", fill = "maroon")+
  labs(title = "Alignment in languages with fixed stress of Guaporé-Mamoré")
