# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#Con este script se realizará el scraping de las bases de datos que serán usadas
#para el desarrollo del Problem Set, específicamente puntos 1 y 2.

##SCRAPING

install.packages("pacman")## Llamar librerías del Script
require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       rvest,
       stringr,#handles HTML
       caret) # Classification And Regression Training

#Luego de revisar el código de la página del Problem Set, se observa que la página
#no contiene los datos de la base de datos, sino que estos se encuentran en el enlace
#web: https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_X.html,
#donde esa X toma un número entre 1 y 10. De dichas páginas se extraerán los datos
#para crear la base de datos a usar en el presente Problem Set.

myURL<-c('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html', #Guardamos enlaces de páginas
         'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html', #de la base de datos
         'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html',
         'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html',
         'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html',
         'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html',
         'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html',
         'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html',
         'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html',
         'https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html')

mytable<-c('000') #Crear variable

for (X in 1:10){ #Descargar datos de los 10 chunks
   mytable[X]<-read_html(myURL[X]) %>%
    html_table()
 }

#Construir la base de datos

df1<-mytable[[1]] #Se obtienen los data frames de la lista
df2<-mytable[[2]]
df3<-mytable[[3]]
df4<-mytable[[4]]
df5<-mytable[[5]]
df6<-mytable[[6]]
df7<-mytable[[7]]
df8<-mytable[[8]]
df9<-mytable[[9]]
df10<-mytable[[10]]

db<-df1 %>%  #Se unen los data frames en 1 base de datos
  bind_rows(df2) %>%
  bind_rows(df3) %>%
  bind_rows(df4) %>%
  bind_rows(df5) %>%
  bind_rows(df6) %>%
  bind_rows(df7) %>%
  bind_rows(df8) %>%
  bind_rows(df9) %>%
  bind_rows(df10) 

saveRDS(db, file = "Base_Datos_PS1.rds") #Crea el archivo RDS en el directorio de
                                        #trabajo de Rstudio


