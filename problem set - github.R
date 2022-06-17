db <- `GEIH_sample1 (3)`
db<- import("https://gitlab.com/Lectures-R/bd-meca-2022-summer/lecture-01/-/raw/main/data/GEIH_sample1.Rds")
library(pacman)
p_load(rio)
p_load(tidyverse)
p_load(e1071)
p_load(EnvStats)
p_load(tidymodels)
p_load(ggplot2)
p_load(scales)
p_load(ggpubr)
p_load(knitr)
p_load(kableExtra)
p_load(foreing)
educ <- db$p6210
db2<-subset(db, select = c(ingtot, pet, mes, age, sex,ocu) )
View(db2)
exp <- floor(c(db$p6426/12))
view(exp)
dbcompleta <- cbind(db2, exp, educ)
view(dbcompleta)
nrow(dbcompleta)
ncol(dbcompleta)
dim(dbcompleta)
head(dbcompleta)
tail(dbcompleta)
variables_categoricas<- dbcompleta %>%mutate_at(.vars = c("ocu", "pet", "sex", "educ"),
                                        .funs = factor)
dbcompleta <- variables_categoricas
str(dbcompleta)
#Observar cuantos missing values en mis variables
is.na(dbcompleta)
cantidad_na <-colSums(is.na(dbcompleta))
class(cantidad_na)
#Necesito transformarla a data.frame 
cantidad_na<- data.frame(cantidad_na)%>%
rownames_to_column("Variables")
#Ordenamos de mayor cantidad de NAs a menor para conocer las variables que cuentan con estos NAs
cantidad_na %>% arrange(desc(cantidad_na))
#Se analizará porcentualmente
cantidad_na$porcentaje_na <- cantidad_na$cantidad_na/nrow(dbcompleta)