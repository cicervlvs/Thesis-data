library(tidyverse)
library(glottospace)
library(stringr)
library(openxlsx)
library(rgdal)
library(ggplot2)
library(svglite)

#After coding more or less all the languages, add the ones with information on them 
glottodata <- subset(read.xlsx("langsJoin-glottoprep.xlsx",
                               sheet = "glottodata"),
                     !is.na(var_has.tone)) %>% #(if has.tone column is blank, then there is no info)
                     select("glottocode", 
                              starts_with("var_"),
                            "notes",
                            "source")

glottostruc <- read.xlsx("langsJoin-glottoprep.xlsx",
                         sheet = "structure")%>%
                    as.data.frame()
glottogroup <- read.xlsx("langsJoin-glottoprep.xlsx",
                         sheet = "sample")%>%
                    as.data.frame()

write.xlsx(list("glottodata" = glottodata,
                "structure"  = glottostruc,
                "sample" = glottogroup),
           "langsJoin-ready.xlsx")


#distance
glottodata_prepped <- glottoget("langsJoin-ready.xlsx",
                                meta = TRUE)
glottocheck(glottodata_prepped)

glottodata_plotting <- glottojoin(glottodata_prepped$glottodata,
                                             with = "glottobase")

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
nmdstest$scoresdata <- left_join(nmdstest$scoresdata,
                                 glottodata_prepped$sample,
                                 "glottocode")

distance_plot <- glottoplot(glottonmds = nmdstest,
           color = "group",
           ptsize = 4,
           preventoverlap = TRUE,
           filename = "../Escrit/Images/distance_plot")

glottoplot(glottodist = distdata,
           label = "name")

#permanova

pair<-glottostat_permanova(glottodata_prepped,
                           comparison = "pairwise")

#plots for features
glottodata_simplified <- glottosimplify(glottodata_prepped)


sample_map <- glottomap(glottodata_plotting,
         # group = "Bolivia",
          color = "var_has.tone",
          label = "name",
          rivers = TRUE,
          colorvec = colorvec,
          filename = "../Escrit/Images/sample_map")
#overview without control languages

#remove control languages
guapmam_langs <- glottodata_prepped$sample %>%
                filter(group != "Control") %>%
                select(glottocode)



#add stress types
glottodata_prepped$glottodata <- glottodata_prepped$glottodata %>%
                          mutate(stress.type =
                                 (case_when(fixed.stress == "yes" ~ "fixed",
                                            lexically.determined.stress == "yes" ~ "lexically.determined",
                                            weight.stress == "yes" ~ "weight.sensitive",
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
                                               bound.window == "none" ~ "none",
                                             stress.type == "weight.sensitive" &
                                               TRUE ~ "none"))%>%
                          mutate(fixed.alignment = 
                                   case_when(stress.type == "fixed" &
                                               bound.window == "left" ~ "left",
                                             stress.type == "fixed" &
                                               bound.window == "right" ~ "right"))
                          



glottodata_guapmam <- glottodata_prepped$glottodata %>%
                    right_join(guapmam_langs,
                               by = "glottocode")

glottodata_guapmam <- glottojoin(glottodata_guapmam,
                                 with = "glottobase")

theme_set(theme_minimal())


#map plotting

map <- glottomap(glottospace(
                 glottosimplify(
                 glottodata_guapmam),
                 method = "buffer",
                 radius = 15),
                 label = "name",
                 lbsize = 1,
                 alpha = 1,
                 rivers = TRUE,
                 filename = "Guap_mam_map.png")


plot_has_tone <- ggplot(glottodata_guapmam,
                               aes(has.tone))+
  geom_bar(position =  "dodge", fill = "maroon")+
  labs(title = "Presence of tone in the languages of Guaporé-Mamoré")+
  theme_minimal()
ggsave("tone_guap_mam.png",
       path = file.path("..", "Escrit", "Images"))

glottodata_nona <- glottodata_guapmam %>%
                  drop_na(tone.type)

plot_tone_type <- ggplot(glottodata_nona,
                               aes(tone.type))+
  geom_bar(position =  "dodge",
           fill = "maroon")+
  labs(title = "Tone system complexity in languages with tone of Guaporé-Mamoré")+
  theme_minimal()
ggsave("tone_type_guap_mam.png",
       path = file.path("..", "Escrit", "Images"))

plot_fixed_vs_weight <- ggplot(glottodata_guapmam,
                               aes(stress.type))+
  geom_bar(position = "dodge", fill = "maroon")+
  labs(title = "Stress types in the languages of Guaporé-Mamoré")
ggsave("stress_type_guap_mam.png",
       path = file.path("..", "Escrit", "Images"))

plot_fixed_alignment <- ggplot(subset(glottodata_guapmam, stress.type == "fixed"),
                               aes(fixed.alignment))+
  geom_bar(position = "dodge", fill = "maroon")+
  labs(title = "Alignment in languages with fixed stress of Guaporé-Mamoré")
ggsave("alignment_fixed_stress.png",
       path = file.path("..", "Escrit", "Images"))

glottodata_guapmam$fixed.stress.position <- factor(glottodata_guapmam$fixed.stress.position,
                                                   levels = c("initial", "second", "third",
                                                              "antepenultimate", "penultimate", "final"))

plot_fixed_position <- ggplot(subset(glottodata_guapmam, stress.type == "fixed"),
                               aes(fixed.stress.position))+
  geom_bar(position = "dodge", fill = "maroon")+
  labs(title = "Position of fixed stress in languages of Guaporé-Mamoré")
ggsave("position_fixed_stress.png",
       path = file.path("..", "Escrit", "Images"))


glottodata_guapmam$weight.alignment <- factor(glottodata_guapmam$fixed.stress.position,
                                                   levels = c("left", "right", "none"))

plot_weight_alignment <- ggplot(subset(glottodata_guapmam, stress.type == "weight.sensitive"),
                               aes(weight.alignment))+
  geom_bar(position = "dodge", fill = "maroon")+
  labs(title = "Alignment in languages with weight-sensitive stress of Guaporé-Mamoré")
ggsave("alignment_weight_stress.png",
       path = file.path("..", "Escrit", "Images"))

guapmam_simplified <- glottosimplify(glottodata_guapmam)
glottomap(glottospace(guapmam_simplified),
         # group = "Bolivia",
          color = "has.tone",
          label = "name",
          rivers = FALSE,
          lbsize = .5)
