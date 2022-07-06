library(tidyverse)
library(glottospace)
library(stringr)
library(openxlsx)
library(rgdal)
library(ggplot2)
library(svglite)
library(xtable)

if (getwd() == "C:/Users/aleja/OneDrive - Universiteit Leiden/Documentos"){
  setwd("../Estudis/MA Linguistics/TFM-Typo/Thesis-data/")
}


source(file.path("SA_stats", "wals-stress_placement", "wals-stress_placement.R"))
source(file.path("SA_stats", "wals-tone", "wals-tone-SA.R"))

save_to_images <- function(name){
  image_folder <- file.path("..", "Escrit", "Images")
  
  ggsave(paste0(name, ".png"),
         path = image_folder)
}

make_three_groups <- function(df){
  mutate(df,
    group = case_when(
    !is.na(glottocode) == TRUE ~ "Guaporé-Mamoré",
    is.na(glottocode) == TRUE &
    is.na(macroarea) == TRUE ~ "World",
    TRUE ~ "South America"
  ))
}

join_wals_SA <- function(df){
  left_join(df, languages, by = "wals.code") %>%
  filter(macroarea == "South America")
}

get_n <- function(df, group_value){
  df %>%
    as.data.frame() %>%
    filter(group == group_value)%>%
    select(n)
}

get_pct <- function(df, group_value){
  df %>%
    as.data.frame() %>%
    filter(group == group_value)%>%
    select(ends_with(".pct"))%>%
    unlist()
}

chisq_guapmam <- function(df, control){
  guapmam_values <- get_n(df, "Guaporé-Mamoré")
  
  control_values <- get_pct(df, control)
  
output <- chisq.test(guapmam_values,
            p = control_values)

apa_style_report <- paste0("$\\chi ^{2}$ ",
                           "(", output$parameter,
                           ", \\textit{N} = ",
                           sum(output$observed), 
                           ") = ",
                           round(output$statistic, 2),
                           ", \\textit{p} =",
                           round(output$p.value, 3))

returns <- list("results" = output,
                "report" = unlist(apa_style_report))

returns
}

remove_var <- function(df){
  for (col in 1:ncol(df)){
   rename_with(df, 
              sub("^var_",
                  ""),
              names(df))   
  }
}

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
#glottocheck(glottodata_prepped)

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

#tests but with one group for Brazil and Bolivia

glottodata_onegroup <- glottoget("langsJoin-onegroup.xlsx",
                                meta = TRUE)
glottodata_onegroup <- glottoconvert(data = glottodata_onegroup,
              var = "var_",
              table = "glottodata",
              remark = "notes",
              ref = "source")

distdata_onegroup <- glottodist(glottodata = glottodata_onegroup)

nmdstest_onegroup <- glottonmds(glottodist = distdata,
                       k = 2,
                       row2id = "glottocode",
                       rm.na = TRUE)

pair_onegroup <- glottostat_permanova(glottodata_onegroup,
                           comparison = "pairwise")

#plots for features
glottodata_simplified <- glottosimplify(glottodata_prepped)


sample_map <- glottomap(glottodata_plotting,
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

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

map <- glottomap(glottospace(
                 glottosimplify(
                 glottodata_guapmam),
                 method = "buffer",
                 radius = 15),
                 color = "family",
                 label = "name",
                 lbsize = .7,
                 alpha = .5,
                 rivers = TRUE,
                 filename = "Guap_mam_map.png")


plot_has_tone <- ggplot(glottodata_guapmam,
                               aes(has.tone))+
  geom_bar(position =  "dodge", fill = "maroon")+
  labs(title = "Presence of tone in the languages of Guaporé-Mamoré",
       x = "Has tone")+
  theme_minimal()+
  scale_fill_manual(values=cbPalette)
save_to_images("tone_guap_mam")

glottodata_nona <- glottodata_guapmam %>%
                  drop_na(tone.type)

plot_tone_type <- ggplot(glottodata_nona,
                               aes(tone.type))+
  geom_bar(position =  "dodge",
           fill = "maroon")+
  labs(title = "Tone system complexity in languages with tone of Guaporé-Mamoré",
       x = "Tone type")+
  theme_minimal()+
  scale_fill_manual(values=cbPalette)
  
save_to_images("tone_type_guap_mam")

glottodata_guapmam <- glottodata_guapmam %>%
                    mutate(stress.type.spaces =
                           case_when(
                             stress.type == "fixed" ~ "Fixed",
                             stress.type == "lexically.determined" ~ "Lexically determined",
                             stress.type == "no.stress" ~ "No stress",
                             stress.type == "weight.sensitive" ~ "Weight-sensitive"
                           ))

plot_fixed_vs_weight <- ggplot(glottodata_guapmam,
                               aes(stress.type.spaces))+
  geom_bar(position = "dodge", fill = "maroon")+
  labs(title = "Stress types in the languages of Guaporé-Mamoré",
       x = "Stress type")+
  scale_fill_manual(values=cbPalette)
save_to_images("stress_type_guap_mam")

plot_fixed_alignment_guapmam <- ggplot(subset(glottodata_guapmam, stress.type == "fixed"),
                               aes(fixed.alignment))+
  geom_bar(position = "dodge", fill = "maroon")+
  labs(title = "Boundedness in languages with fixed stress of Guaporé-Mamoré",
       x = "Binding")+
  scale_fill_manual(values=cbPalette)
save_to_images("alignment_fixed_stress")

glottodata_guapmam$fixed.stress.position <- factor(glottodata_guapmam$fixed.stress.position,
                                                   levels = c("initial", "second", "third",
                                                              "antepenultimate", "penultimate", "final"))

plot_fixed_position <- ggplot(subset(glottodata_guapmam, stress.type == "fixed"),
                               aes(fixed.stress.position))+
  geom_bar(position = "dodge")+
  labs(title = "Position of fixed stress in languages of Guaporé-Mamoré",
       x = "Fixed stress position")+
  scale_fill_manual(values=cbPalette)
save_to_images("position_fixed_stress")

glottodata_guapmam$weight.alignment <- factor(glottodata_guapmam$fixed.stress.position,
                                                   levels = c("left", "right", "none"))

plot_weight_alignment <- ggplot(subset(glottodata_guapmam,
                                       stress.type == "weight.sensitive"),
                               aes(weight.alignment))+
  geom_bar(position = "dodge", fill = "maroon")+
  labs(title = "Alignment in languages with weight-sensitive stress of Guaporé-Mamoré")+
  scale_fill_manual(values=cbPalette)
save_to_images("alignment_weight_stress")

guapmam_simplified <- glottosimplify(glottodata_guapmam) %>%
                      rename("Has tone" = has.tone) %>%
                      mutate("Stress type" = case_when(
                              stress.type == "weight.sensitive" ~ "Weight-sensitive",
                              stress.type == "lexically.determined" ~ "Lexically determined",
                              stress.type == "fixed" ~ "Fixed"
                      ))
                      

map_tone<- glottomap(glottospace(guapmam_simplified),
          color = "Has tone",
          label = "name",
          rivers = FALSE,
          lbsize = .7,
          filename = "../Escrit/Images/tone_map")


guapmam_simplified_nonostress <- guapmam_simplified %>%
                                  filter(stress.type != "no.stress")

map_stresstype <- glottomap(glottospace(guapmam_simplified_nonostress),
          color = "Stress type",
          label = "name",
          rivers = FALSE,
          colorvec = colorvec,
          filename = "../Escrit/Images/stress_type_map")


#####################
##grouped bar plots##
#####################




#Fixed stress position
fixed_for_grouping <- fixed_simple %>%
                      rename(fixed.stress.position = description) %>%
                      filter(!is.na(fixed.stress.position) &
                                    fixed.stress.position != "No fixed stress")
  
glottodata_fixed_grouped <- bind_rows(glottodata_guapmam %>%
                                      filter(!is.na(fixed.stress.position))%>%
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
                                     join_wals_SA())%>%
                                 make_three_groups() %>%
                            count(fixed.stress.position, group) %>%
                            group_by(group) %>%
                            mutate(fixed.stress.position.pct =
                                     n / sum(n))
                            
glottodata_fixed_grouped$fixed.stress.position <- factor(
                                                  glottodata_fixed_grouped$fixed.stress.position,
                                                  levels = c("Initial", "Second", "Third",
                                                              "Antepenultimate", "Penultimate", "Ultimate"))

plot_fixed_position_grouped <- ggplot(glottodata_fixed_grouped,
                                      aes(x = fixed.stress.position,
                                          y = fixed.stress.position.pct,
                                          fill = group))+
                              geom_col(position = "dodge")+
                              labs(title = "Fixed stress positions in languages of Guaporé-Mamoré, South America,\n and the world",
                                   x = "Fixed stress position",
                                   y = "Rate")+
                              scale_fill_manual(values=cbPalette)+
                              theme_minimal()
save_to_images("position_fixed_stress_grouped")


#Presence of tone

tone_for_grouping <- tone_simple %>%
                        rename(has.tone = hasTone) %>%
                        mutate(has.tone = case_when(
                                 has.tone == "tone" ~ "yes",
                                 has.tone == "no tone" ~ "no")) %>%
                        mutate(macroarea = NULL)

glottodata_tone_grouped <- bind_rows(glottodata_guapmam,
                                     tone_for_grouping,
                                     tone_for_grouping %>%
                                      join_wals_SA())%>%
                               make_three_groups()%>% 
                            count(has.tone, group) %>%
                            group_by(group) %>%
                            mutate(has.tone.pct =
                                     n / sum(n))

glottodata_tone_grouped$has.tone <- factor(glottodata_tone_grouped$has.tone,
                                           levels = c("yes", "no"))

plot_has_tone_grouped <- ggplot(glottodata_tone_grouped,
                                      aes(x = has.tone,
                                          y = has.tone.pct,
                                          fill = group))+
                              geom_col(position = "dodge")+
                              labs(title = "Presence of tone in languages of Guaporé-Mamoré, South America, and the world",
                                   x = "Has tone",
                                   y = "Rate")+
                              scale_fill_manual(values=cbPalette)
save_to_images("tone_guap_mam_grouped")

#Boundedness

fixed_and_weight_for_plotting <- fixed_and_weight %>%
                                 rename(bound.window = alignment)%>%
                                 mutate(macroarea = NULL)
  
glottodata_guapmam_for_boundedness_plot <- glottodata_guapmam %>%
                                              mutate(bound.window =
                                                       case_when(
                                                         bound.window == "left" ~ "Left",
                                                         bound.window == "right" ~ "Right"
                                                       ))%>%
                                              filter(!is.na(.$bound.window))

glottodata_boundedness_grouped <- bind_rows(glottodata_guapmam_for_boundedness_plot,
                                            fixed_and_weight_for_plotting,
                                            fixed_and_weight_for_plotting %>%
                                              join_wals_SA())%>%
                                make_three_groups()%>% 
                                 count(bound.window, group) %>%
                                 group_by(group) %>%
                                 mutate(bound.window.pct =
                                     n / sum(n))
   
plot_boundedness_grouped <- ggplot(glottodata_boundedness_grouped,
                                      aes(x = bound.window,
                                          y = bound.window.pct,
                                          fill = group))+
                              geom_col(position = "dodge")+
                              labs(title = "Window-boundedness in languages of Guaporé-Mamoré and the world with\n bound windows",
                                   x = "Binding",
                                   y = "Rate")+
                              scale_fill_manual(values=cbPalette)
save_to_images("alignment_grouped")
                                  

#Stress type

weight_for_stress_type_plot <- weight %>%
                                mutate(stress.type.spaces =
                                        case_when(
                                         description == "Not predictable" ~ "Lexically determined",
                                         description == "Fixed stress (no weight-sensitivity)" ~ "Fixed",
                                         TRUE ~ "Weight-sensitive"))

glottodata_guapmam_for_stress_type_plot <- glottodata_guapmam %>%
                                          filter(stress.type.spaces != "No stress")

glottodata_stress_type_grouped <- bind_rows(glottodata_guapmam_for_stress_type_plot,
                                            weight_for_stress_type_plot,
                                            weight_for_stress_type_plot %>%
                                             join_wals_SA()) %>% 
                                 make_three_groups()%>% 
                                 count(stress.type.spaces, group) %>%
                                 group_by(group) %>%
                                 mutate(stress.type.spaces.pct =
                                     n / sum(n))
                                  
plot_stress_type_grouped <- ggplot(glottodata_stress_type_grouped,
                                      aes(x = stress.type.spaces,
                                          y = stress.type.spaces.pct,
                                          fill = group))+
                              geom_col(position = "dodge")+
                              labs(title = "Stress types in languages of Guaporé-Mamoré, South-America,\n and the world with bound windows",
                                   x = "Stress type",
                                   y = "Rate")+
                              scale_fill_manual(values=cbPalette)
save_to_images("stress_type_guap_mam_grouped")

####################
##Chi-square tests##                                                       
####################


#Stress types

chisq_stress_types_world <- chisq_guapmam(glottodata_stress_type_grouped,
                                          "World")

chisq_stress_types_SA <- chisq_guapmam(glottodata_stress_type_grouped,
                                      "South America")

#Tone

chisq_tone_world <- chisq_guapmam(glottodata_tone_grouped,
                                  "World")

chisq_tone_SA <- chisq_guapmam(glottodata_tone_grouped,
                                  "South America")
#Boundedness

chisq_boundedness_world <- chisq_guapmam(glottodata_boundedness_grouped,
                                         "World")

chisq_boundedness_SA <- chisq_guapmam(glottodata_boundedness_grouped,
                                         "South America")
#Exporting dataset for appendix

glottodata_for_appendix <- glottodata %>%
           map(remove_var())
           left_join(glottodata_plotting %>%
                       select(name, glottocode, country),
                     by = "glottocode")%>%
           select(!notes &
                  !geometry)%>%
           remove_var()

print.xtable(
  xtable(glottodata_for_appendix,
         type = "latex",
         tabular.environment = "longtable",
         file = "dataset.tex")
)
