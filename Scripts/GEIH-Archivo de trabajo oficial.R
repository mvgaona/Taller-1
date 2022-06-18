# Valeria Gaona - 202214418
# Andrea Beleño - 200620739
#Con este script se realizará el scraping de las bases de datos que serán usada 
#para el desarrollo del Problem Set, específicamente puntos 1 y 2.
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
p_load(skimr)
p_load(rvest)
p_load(caret)
#BASE DE DATOS GEIH
#Se importará la base de datos cada base de datos y se volverá data.frame para poder tenerlo en matriz.
Base1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")%>%
  html_table()
Base1 <- data.frame(Base1)
Base2 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html")%>% 
  html_table()
Base2 <- data.frame(Base2)
Base3 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html")%>% 
  html_table()
Base3 <- data.frame(Base3)
Base4 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html")%>% 
  html_table()
Base4 <- data.frame(Base4)
Base5 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html")%>% 
  html_table()
Base5 <- data.frame(Base5)
Base6 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html")%>% 
  html_table()
Base6 <- data.frame(Base6)
Base7 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html")%>% 
  html_table()
Base7 <- data.frame(Base7)
Base8 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html")%>% 
  html_table()
Base8 <- data.frame(Base8)
Base9 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html")%>% 
  html_table()
Base9 <- data.frame(Base9)
Base10 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html")%>% 
  html_table()
Base10<- data.frame(Base10)
#Al observar que cada una de las bases de datos si pudo ser importada, se procede a unir cada base de datos
#Con la fusión de todas las bases de datos, tendremos oficialmente los Datos completos de la GEIH de 2018
DatosGEIH<- rbind(Base1, Base2, Base3, Base4, Base5, Base6, Base7, Base8, Base9, Base10)
#Ahora, procederemos a realizar la clasificación de variables.
DGEIH<-subset(DatosGEIH, select = c( directorio,ingtot, pet, mes, age, sex,ocu) )
View(DGEIH)
exp <- floor(c(DatosGEIH$p6426/12))
view(exp)
educ <- DatosGEIH$p6210
View(educ)
OfGEIH <- cbind(DGEIH, exp, educ)
View(OfGEIH)
#La base de datos completa se llamará OfGEIH
view(OfGEIH)
nrow(OfGEIH)
ncol(OfGEIH)
dim(OfGEIH)
head(OfGEIH)
tail(OfGEIH)


