library(dplyr)
library(glottospace)
library(stringr)
library(openxlsx)
library(rgdal)

glottodata_prepped <- glottoget("langsJoin-glottoprep.xlsx",
                                meta = TRUE)

glottodata_prepped <- glottoconvert(data = glottodata_prepped,
              var = "var_",
              table = "glottodata",
              remark = "notes",
              ref = "source")

distdata <- glottodist(glottodata = glottodata_prepped)
