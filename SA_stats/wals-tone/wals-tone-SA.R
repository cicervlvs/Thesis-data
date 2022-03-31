library(dplyr)

languages <- read.delim("languoid.txt",
                        fileEncoding = "UTF-8") %>% select(wals.code, macroarea)

toneValues <- read.delim("tone.txt",
                         fileEncoding = "UTF-8",
                         skip = 7)

toneWithMacro <- toneValues %>% left_join(languages, by = c("wals.code"))


toneSimple <- toneWithMacro %>% mutate(hasTone =
                           if_else(.$description == "Complex tone system" | .$description == "Simple tone system",
                                   'tone',
                                   'no_tone'))

tone_SA <- toneSimple %>% filter(.$macroarea == "South America")

freqTab <- table(tone_SA$hasTone)
prop.table(freqTab)
