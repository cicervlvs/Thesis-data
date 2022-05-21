###################################################
#### DO NOT SOURCE THE ENTIRE FILE ALL AT ONCE ####
############# CONTAINS ERRORS #####################
###################################################

library(dplyr)
library(glottospace)
library(stringr)
library(openxlsx)

addGlottocodeName <- function(db){
  
  db <- db %>%
    mutate_if(is.logical, as.character)
  
  db$glottopenis <- NA
  
  nameColumn <- db$name
  
  codeList <- list() 
  
  for (lang in nameColumn){
    
    glottoQ <-  glottosearch(search = lang,
                             columns = "name",
                             tolerance = 0.2)
    
    if (is.na(glottoQ) == TRUE){
    glottoQ <- glottosearch(search = lang,
                   columns = "name",
                   tolerance = 0.3)
    
    } else if (substr(glottoQ, 1, 4) != substr(tolower(lang), 1, 4)){
      glottoQ <- glottosearch(search = lang,
                              columns = "name",
                              tolerance = 0)
      
    } else{}
    
    
    codeList[[length(codeList) + 1]] <- glottoQ$glottocode[1]
    
  }
  print(codeList)
  .GlobalEnv$glottolist <- codeList
  .GlobalEnv$db$glottocode <- codeList
  print(.GlobalEnv$db$glottocode)
}

langs <- read.delim("langTable.txt")%>%
  select(Number, Language, Classification, Country)

  
#langs <- langs %>%
 # str_replace_all(langs, ",","")

write.table(langs, 
            file = "langTablemod.txt",
            row.names = FALSE,
            sep = "\t",
            fileEncoding = "UTF-8")

#after modifying langTablemod.txt manually
langs <- read.xlsx("langTablemod.xlsx",
          rowNames = FALSE,
          skipEmptyRows = FALSE)

sample <- read.xlsx("database_proposal.xlsx",
          rowNames = FALSE,
          skipEmptyRows = FALSE)

#Joining with the sample i already have, removing the unknown languages
langsJoin <- langs %>%
            full_join(sample,
                      by = "name")%>%
            select(-"Country.y")%>%
            rename("Country" = "Country.x")%>%
           filter(name != "(unknown)")
             
#langsJoin$glottopenis <- NA
  
addGlottocodeName(langsJoin)

langsJoin$glottocode <- glottolist

write.xlsx(langsJoin,
           "langsJoin.xlsx")

# After some frustrating editing with excel

#langsJoin <- read.xlsx("langsJoin.xlsx",
#                       rowNames = FALSE,
#                       skipEmptyRows = FALSE)%>%
#            select(glottocode, everything())

#glottoLangsJoin <- glottojoin(langsJoin,
#                              with = "glottospace")