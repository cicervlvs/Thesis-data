library(dplyr)
library(glottospace)
library(stringr)
library(openxlsx)
library(rgdal)

#After coding more or less all the languages, add the ones with information on them (if has.tone column is blank, then there is no info)

glottodata <- subset(glottoget("langsJoin.xlsx"),
                     !is.na(has.tone)) %>%
                     select(-one_of("Number",
                                    "name",
                                    "Classification",
                                    "Country",
                                    "Family",
                                    "wals.code",
                                    "iso.code",
                                    "Latitude",
                                    "Longitude",
                                    "notes",
                                    "source"
))

glottocheck(glottodata)

glottodata <- glottojoin(glottodata, with = "glottobase")

map <- glottomap(glottospace(glottodata,
                             method = "buffer",
                             radius = 15),
                 color = "name",
                 alpha = 1,
                 rivers = TRUE,
                 filename = "Guap_mam_map.png")

map
