library(dplyr)

languages <- read.delim("languoid.txt",
                        fileEncoding = "UTF-8") %>% select(wals.code, macroarea)

toneValues <- read.delim("tone.txt",
                         fileEncoding = "UTF-8",
                         skip = 7) %>%
              left_join(languages, by = c("wals.code"))

toneSimple <- toneValues %>% mutate(hasTone =
                           if_else(.$description == "Complex tone system" |
                                   .$description == "Simple tone system",
                                     'tone',
                                    'no tone'))

tone_SA <- toneSimple %>%
          filter(.$macroarea == "South America")

tabSimpCompTone <- table(toneValues$description)

simpCompToneSA <- toneValues %>% filter(.$macroarea == "South America") 
tabSimCompSA <- table(simpCompToneSA$description)
tabSimCompSAOnlyTone <- table(simpCompToneSA$description,
                              exclude = "No tones")

toneFreqTab <- table(tone_SA$hasTone)
tonePropTab <- prop.table(freqTab)