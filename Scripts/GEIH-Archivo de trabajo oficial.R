# Valeria Gaona - 202214418
# Andrea Beleño - 200620739
#Con este script se realizará el scraping de las bases de datos que serán usada 
#para el desarrollo del Problem Set, específicamente puntos 1 y 2.
install.packages("pacman") #Instalar librería si no cuenta con esta 
library(pacman) #Llamar librería
p_load(rio, #Instalar librerías que falten
       tidyverse,
       e1071,
       EnvStats,
       tidymodels,
       ggplot2,
       scales,
       ggpubr,
       knitr,
       kableExtra,
       foreign,
       skimr,
       rvest,
       caret,
       stringr,
       stargazer,
       recipes)

#####BASE DE DATOS GEIH Punto 1.1.1
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
saveRDS(DatosGEIH, file = "Datos_GEIH.rds") #Crea el archivo RDS en el directorio de

#trabajo de Rstudio
#####Punto 1.2
DatosGEIH_18<-DatosGEIH[DatosGEIH$age>=18,] #Se realizará el análisis para individuos 
#con edad mayor o igual a 18 años
#Ahora, procederemos a realizar la clasificación de variables.
exp <- floor(c(DatosGEIH_18$p6426/12)) #Se anualiza la variable relacionada con la experiencia
view(exp)
educ <- DatosGEIH_18$p6210 #Se asigna la variable educación
View(educ)
DGEIH<-subset(DatosGEIH_18, select = c( "directorio","ingtot", "pet", "mes", "age", "sex","ocu") ) #Hacer un subset con las variables a usar
DGEIH<-cbind(DGEIH, exp, educ) #Incluir las variables calculadas
View(DGEIH)
DGEIH<- DGEIH[DGEIH$ingtot>0,]
#####Punto 1.2.2

DGEIH <- DGEIH %>% #Se vuelven categóricas las variables que así lo sean en la BD
  mutate_at(.vars = c(
    "directorio", "pet","sex", "ocu", "educ"),
    .funs = factor)

summary(DGEIH) #Se hace una inspección general de esta base de datos

cantidad_na <- sapply(DGEIH, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(DGEIH)
porcentaje_na <-porcentaje_na*100
porcentaje_na #Visualizo el porcentaje de los datos que tienen NA

DGEIH[is.na(DGEIH)] = 0 #Se asigna 0 a las NA (Ver documento para explicación)
DGEIH %>% subset(ingtot <= 2*iqr | is.na(ingtot)==T)
summary(DGEIH) #Se verifica que no existan NAs

#####Punto 1.2.3

View(DGEIH)
nrow(DGEIH)
ncol(DGEIH)
dim(DGEIH)
head(DGEIH)
tail(DGEIH)

summary(DGEIH) #Se realiza el análisis descriptivo de las variables a tener en cuenta

## data + mapping
ggplot(data = DGEIH , mapping = aes(x = age , y = ingtot))

## + geometry
ggplot(data = DGEIH , mapping = aes(x = age , y = exp)) +
  geom_point(col = "red" , size = 0.5)

## by group
ggplot(data = DGEIH , 
       mapping = aes(x = age , y =ingtot , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()



#Ahora, procederemos a realizar la clasificación de variables.
#DGEIH<-subset(DatosGEIH, select = c( directorio,ingtot, pet, mes, age, sex,ocu) )
#View(DGEIH)
#exp <- floor(c(DatosGEIH$p6426/12))
#view(exp)
#educ <- DatosGEIH$p6210
#View(educ)
#OfGEIH <- cbind(DGEIH, exp, educ)
#View(OfGEIH)
#La base de datos completa se llamará OfGEIH
##
#view(OfGEIH)
#nrow(OfGEIH)
#ncol(OfGEIH)
#dim(OfGEIH)
#head(OfGEIH)
#tail(OfGEIH)
##
#view(OfGEIH)
#nrow(OfGEIH)
#ncol(OfGEIH)
#dim(OfGEIH)
#head(OfGEIH)
#tail(OfGEIH)

#####PUNTO 1.3.1
#Se va a analizar la variable que describe el ingreso
#Primero decido analizar el ingreso total, ingreso total imputado y el observado.
View(subset(DatosGEIH, select = c(ingtot, ingtotes, ingtotob)))
#Por lo tanto, el ingreso total sería la suma del ingreso observado y el imputado.
#Sin embargo, es pertinente analizar las demás variables que pueden componer el ingreso, siendo estos, los ingresos por intereses, por ayudas, monetarios, arriendos, especie y monetarios: 
View(subset(DatosGEIH, select = c(ingtot, iof1 , iof1es, iof2, iof2es, iof3h, iof3hes, iof6, iof6es, isa, isaes, ie, iees, imdies , impa, impaes)))
#Al observar que efectivamente algunas de estas variables de ingreso suman en algunas observaciones el ingreso total (ingtot), se asume que efectivamente, la variable que describe todo el ingreso.
#Ahora, se comparará con las primas
View(subset(DatosGEIH, select = c(ingtot, y_primas_m, y_primaVacaciones_m	, y_primaServicios_m, y_primaNavidad_m	, y_subEducativo_m , y_subFamiliar_m)))
#Algunas variables son parte del ingreso total, por lo tanto, se compararán todas las variables contra Ingreso total para hacer la última verificación acerca de la variable ingtot
#De acuerdo a todas las verificaciones anteriores, se comprueba que la variable que describe el ingreso es igntot (Ingreso total), ya que esta contiene todas las demás variables acerca del ingreso que se encuentran en la base de datos de la GEIH

###PUNTO 1.3.2
#Se realizará la creación de la variable Age^2 para proceder con la estimación del modelo
lningtot<- log(DGEIH$ingtot)
DGEIH_AGE2<- DGEIH %>% mutate(Age2=age^2)
view(DGEIH_AGE2)
DGEIH_AGE2 <- cbind(DGEIH_AGE2, lningtot)
View(DGEIH_AGE2)
#Estimación del modelo:
require(tidyverse)
ggplot(DGEIH_AGE2)+ geom_point(aes(x= age, y = lningtot)) #Gráfica de los datos para la observación de la relación visual de la variable age e ingtot
modelo1 <- lm(lningtot~age + Age2, data = DGEIH_AGE2)
summary(modelo1)
require("stargazer")
stargazer(modelo1,type="text")
#Se observa que tanto los residuales se ajustan a la recta de la regresión. 
ggplot(DGEIH_AGE2)+ geom_point(aes(x= modelo1$residuals, y = lningtot))
#####PUNTO 1.4.1



