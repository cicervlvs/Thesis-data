install.packages("glottospace")
library(glottospace)

#Read data into R using glottoget()

name1 <-glottoget("filename.ext")

#Convert into glottospace format using glottoconvert

name1 <- glottoconvert(data = name, var = "var_")

#Check for consistency with glottocheck

glottocheck(name1)

#Create a distance matrix with glottodist

name2 <- glottodist(glottodata = name1)

#nmds plot

colorvec <- c("Y"="red", "N"="blue")
name3 <- glottonmds(name2, k = 2, row2id = "glottocode", rm.na = TRUE)
glottoplot(glottonmds = name3, color = "varname", ptsize = 1, colorvec = colorvec)

#add groups that you have defined, after this, you can use the column name for the argument color =

name3$scoresdata<-dplyr::left_join(name3$scoresdata, name1$sample, "glottocode")

#Permanova of groups
pair<-glottostat_permanova(name1, comparison = "pairwise")
glottosave(pair, filename = "filename")

#Permanova of subgroups ("group" and "subgroup" are column names in your sample tab)
name1[["sample"]][,"group"] <- name1[["sample"]][,"subgroup"]
pair<-glottostat_permanova(name1, comparison = "pairwise")
glottosave(pair, filename = "filename")

#Making quick maps of individual feature distributions:

name4<-glottosimplify(name1)
glottomap(name4, color="varname", label="name")



