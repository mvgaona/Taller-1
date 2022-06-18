# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#Con este script se realizará el scraping de las bases de datos que serán usadas
#para el desarrollo del Problem Set, específicamente puntos 1 y 2.

install.packages("pacman")## Llamar librerías del Script
require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       rvest, #handles HTML
       caret) # Classification And Regression Training

