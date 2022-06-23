# Valeria Gaona - 202214418
# Andrea Beleño - 200620739
#### PROBLEM SET 1 #####
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
       boot,
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
#####Punto 1.1.2
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
## data + mapping
ggplot(data = DGEIH , mapping = aes(x = age , y = ingtot))

## + geometry
ggplot(data = DGEIH , mapping = aes(x = age , y = exp)) +
  geom_point(col = "red" , size = 0.5)

## by group
ggplot(data = DGEIH , 
       mapping = aes(x = age , y =ingtot , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()
#Descripción age (Edad)
Edad<- DGEIH$age
class(Edad)
plot(hist(Edad))
mean(Edad)
min(Edad)
max(Edad)
mean(Edad)
modeEdad(Edad)
modeEdad <- function(Edad){
  return(as.numeric(names(which.max(table(Edad)))))
}
modeEdad(Edad)
#Descripción PET
PET <- DGEIH$pet
View(DGEIH$pet)
class(PET)
levels(PET)
summary(PET)
#Educ
plot(hist(educ))
class(educ)
mean(educ)
modeEduc(educ)
modeEduc <- function(educ){
  return(as.numeric(names(which.max(table(educ)))))
}
modeEduc(educ)
#Descripción Ocupación
ocu<- DGEIH$ocu
class(ocu)
levels(ocu)
summary(ocu)
table(ocu)
pie(table(ocu))#generando un gráfico de torta
#Genero
sex<- DGEIH$sex
class(sex)
levels(sex)
summary(sex)
table(sex)
barplot(table(sex))
#Descripción de Experiencia
expp<- DGEIH$exp
View(expp)
class(expp)
plot(hist(expp))
mean(expp)
min(expp)
max(expp)
modeExp(expp)
modeExp <- function(expp){
  return(as.numeric(names(which.max(table(expp)))))
}
modeExp(expp)
#Descripción Directorio
direc<- DGEIH$directorio
View(direc)
class(direc)
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
#Punto 1.3.3 Se analiza la predicción y su respectiva gráfica
predingtot<- 12.881+0.048*DGEIH_AGE2$age - 0.0005* DGEIH_AGE2$Age2
predingtot <- c(predingtot)
predingtot<- data.frame(predingtot)
View(predingtot)
ggplot(predingtot) + geom_point(aes (x= DGEIH_AGE2$age, y = predingtot))
#Punto 1.3.4 Bootstrap
install.packages(boot)
p_load(boot)
require("tidyverse")
set.seed(10101)
R<- 1000
est_modelo1 <- rep(0,R)
for(i in 1: R){
  ingtot_sam<- sample_frac(DGEIH_AGE2, size = 1,replace = TRUE)
  modf<- lm(lningtot~age + Age2, data = ingtot_sam)
  coefic<- modf$coefficients
  est_modelo1[i]<- coefic[2]
}

plot(hist(est_modelo1))
#Este vector está centrado al de rededor de 0.0475. Además, el coeficiente en cuestión, está dado por 0.048, centrado al rededor de ese valor. 
#Tiene una forma aproximadamente normal.
mean(est_modelo1)
sqrt(var(est_modelo1))
quantile(est_modelo1, c(0.025, 0.975))
#Como ya tenemos un modelo complejo, nos dispondremos a dividir los coeficientes
coeficientes<- modelo1$coefficients
b0<-coeficientes[1]
b0
sqrt<- modelo1$
  b1<-coeficientes[2]
b1
b2<-coeficientes[3]
b2
estimboot <- function(DGEIH_AGE2, index){
  coef(lm(lningtot~age+ Age2, data = DGEIH_AGE2, subset = index))}
resultado<- boot(DGEIH_AGE2, statistic = estimboot, R = 1000)
resultado
#Ahora, vamos a maximizar la función y obtener el error estándar
#Para contar con la maximización y los coeficientes 
age_bar<- mean(DGEIH_AGE2$age)
age2_bar<- mean(DGEIH_AGE2$Age2)
maxmod1<- b1+2*b2*age2_bar
maxmod1
#Ahora, se hará en múltipes muestras, usando la función
n<- length(DGEIH_AGE2$lningtot)
est_mod<- function(DGEIH_AGE2, index, 
                   age_bar= mean(DGEIH_AGE2$Age2)){
  fun<- lm(lningtot~ age + Age2, DGEIH_AGE2, subset = index)
  coefsfun<-fun$coefficients
  beta1<-coefsfun[2]
  beta2<-coefsfun[3]
  maxage<- beta1+ beta2*age2_bar*2
  return(maxage)
}
est_mod(DGEIH_AGE2, 1:n)
#El muestreo que generaré será el mismo tamaño de la muestra original:
library(boot)
results <- boot(data= DGEIH_AGE2, est_mod,R=1000)
results
#Edad pico
PeakAge <-((-Beta1)/Beta2)*(1/2)
PeakAge
#####PUNTO 1.4.1



