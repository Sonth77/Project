##LIBRERIAS (NO TODAS SON NECESARIAS PERO ES RECOMENDABLE CARGARLAS)
library(pacman)
library(readxl)
library(dplyr)
pacman::p_load(raster, rgdal,
               rgeos, stringr, sf,
               tidyverse, RColorBrewer, 
               cowplot, ggpubr, ggspatial, 
               rnaturalearth, rnaturalearthdata,
               viridisLite, viridis)

## ES NECESARIO TENER EL ARCHIVO SHP Y CSV EN LA MISMA CARPETA 
## ESTABLECER DIRECTORIO DE TRABAJO, ES NECESARIO PARA QUE EL PROGRAMA FUNCIONE
## PARA ESTABLECERLA SE DEBE IR A SESSION, LUEGO A SET WORKING DIRECTORY
## Y CHOOSE DIRECTORY Y BUSCAR LA CARPETA DONDE TENEMOS LOS ARCHIVOS
## Y LE DAMOS OPEN

setwd("C:/Users/HP/Desktop/depto.shp/")

###TABLA CSV,SE NECESITA CARGAR PARA UNA CORRECTA MUESTRA DE LOS DATOS 
## AL EJECUTARLO SE MOSTRARA LA TABLA DE VALORES NUMERICOS 

cv <- read.csv("Precios_Areas_Prod.csv")

View(cv)

#MAPA COLOMBIA CON CULTIVOS DE CAFE DEL AÑO 2002

Mapa_col <- st_read("depto.shp")
Mapa_col$AR2002<-cv$X2002
ggplot()+
  geom_sf(data=Mapa_col, aes(fill=AR2002))+
  scale_fill_viridis(option = "viridis", direction=-1)+
  labs(title = "Area por miles de hectareas cultivadas 
       en cafe por departamento en 2002",
       caption = "https://sites.google.com/site/seriescol/shapes
       Datos tomados de las estadisticas presentadas por el comité cafetero
       (2021)",
       fill="Area cultivada")

#MAPA COLOMBIA CON CULTIVOS DE CAFE DEL AÑO 2012

Mapa_col <- st_read("depto.shp")
Mapa_col$AR2012<-cv$X2012
ggplot()+
  geom_sf(data=Mapa_col, aes(fill=AR2012))+
  scale_fill_viridis(option = "viridis", direction=-1)+
  labs(title = "Area por miles de hectareas cultivadas 
       en cafe por departamento en 2012",
       caption = "https://sites.google.com/site/seriescol/shapes
       Datos tomados de las estadisticas presentadas por el comité cafetero
       (2021)",
       fill="Area cultivada")

#MAPA COLOMBIA CON CULTIVOS DE CAFE DEL AÑO 2021

Mapa_col <- st_read("depto.shp")
Mapa_col$AR2021<-cv$X2021
ggplot()+
  geom_sf(data=Mapa_col, aes(fill=AR2021))+
  scale_fill_viridis(option = "viridis", direction=-1)+
  labs(title = "Area por miles de hectareas cultivadas 
       en cafe por departamento en 2021",
       caption = "https://sites.google.com/site/seriescol/shapes
       Datos tomados de las estadisticas presentadas por el comité cafetero
       (2021)",
       fill="Area cultivada")
