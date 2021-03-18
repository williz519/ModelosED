library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(leaflet)
library(RColorBrewer)
library(dplyr)

library(readxl)
library(xlsx)
library(sqldf)
require(reshape)
require(dplyr)
require(psych)
require(GGally)
library(caret)

#Cargas las Bases de Datos
rm(list = ls())

#data = read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Database/DBCompleta_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

## Se carga DBCompleta y se eligen 39 viajes para validación
#write.xlsx(data, 
#            file="/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Database/DBMuestraArt3.xlsx")

### Cargamos la BD ya con los viajes corregidos

DB <- read_xlsx("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBMuestraArt3_VF.xlsx")

# Eliminamos las distancias y tiempos CERO

table(DB$DISTAlt1 == 0)
table(DB$DISTAlt2 == 0)
table(DB$DISTAlt3 == 0)

table(DB$TIEMPOAlt1 == 0)
table(DB$TIEMPOAlt2 == 0)
table(DB$TIEMPOAlt3 == 0)
table(DB$TIEMPOEC == 0)



for (i in 1:nrow(DB)){
  if (DB$DISTAlt2[i] == 0){
    DB$DISTAlt2[i] = max(DB$DISTAlt1[i],DB$DISTAlt3[i])+0.3}
  if (DB$DISTAlt3[i] == 0){
    DB$DISTAlt3[i] = max(DB$DISTAlt1[i],DB$DISTAlt2[i])+0.3}
  if (DB$TIEMPOAlt2[i] == 0){
    DB$TIEMPOAlt2[i] = max(DB$TIEMPOAlt1[i],DB$TIEMPOAlt3[i])+3}
  if (DB$TIEMPOAlt3[i]==0){
    DB$TIEMPOAlt3[i] = max(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i])+3}
}


#VARIABLE SEMAFOROS

for (i in 1:nrow(DB)){
  if (DB$Semaf_A2[i] == 999){
    DB$Semaf_A2[i] = mean(DB$Semaf_A1[i],DB$Semaf_EC[i])}
  if (DB$Semaf_A3[i] == 999){
    DB$Semaf_A3[i] = mean(DB$Semaf_A1[i],DB$Semaf_A2[i], DB$Semaf_EC[i])}
  
  if (DB$ZER_A2[i] == 999){
    DB$ZER_A2[i] = min(DB$ZER_A1[i],DB$ZER_A3[i])+1}
  if (DB$ZER_A3[i] == 999){
    DB$ZER_A3[i] = min(DB$ZER_A1[i],DB$ZER_A2[i])+1}
}


# CREACION DE NUEVAS VARIABLES

for (i in 1:nrow(DB)) {
  DB$SEM_A1_km[i] = DB$Semaf_A1[i]/DB$DISTAlt1[i]
  DB$SEM_A2_km[i] = DB$Semaf_A2[i]/DB$DISTAlt2[i] 
  DB$SEM_A3_km[i] = DB$Semaf_A3[i]/DB$DISTAlt3[i]
  DB$SEM_EC_km[i] = DB$Semaf_EC[i]/DB$DISTEC[i] 
  
  DB$Panel_A1_km[i] = DB$Paneles_A1[i]/DB$DISTAlt1[i] 
  DB$Panel_A2_km[i] = DB$Paneles_A2[i]/DB$DISTAlt2[i] 
  DB$Panel_A3_km[i] = DB$Paneles_A3[i]/DB$DISTAlt3[i] 
  DB$Panel_EC_km[i] = DB$Paneles_EC[i]/DB$DISTEC[i] 
  
  DB$ZER_A1_km[i] = DB$ZER_A1[i]/DB$DISTAlt1[i]
  DB$ZER_A2_km[i] = DB$ZER_A2[i]/DB$DISTAlt2[i]
  DB$ZER_A3_km[i] = DB$ZER_A3[i]/DB$DISTAlt3[i]
  DB$ZER_EC_km[i] = DB$ZER_EC[i]/DB$DISTEC[i]
}

# Normalización de las variables tiempo
for (i in 1:nrow(DB)){ 
  DB$T_Alt_1[i] = (DB$TIEMPOAlt1[i]- (min(c(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-1)))/((max(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])+0.2)-((min(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-1)))
  DB$T_Alt_2[i] = (DB$TIEMPOAlt2[i]- (min(c(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-1)))/((max(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])+0.2)-((min(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-1)))
  DB$T_Alt_3[i] = (DB$TIEMPOAlt3[i]- (min(c(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-1)))/((max(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])+0.2)-((min(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-1)))
  DB$T_Alt_4[i] = (DB$TIEMPOEC[i]- (min(c(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-1)))/((max(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])+0.2)-((min(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-1)))
  
  # Normalización de la variable distancia
  DB$D_Alt_1[i] = (DB$DISTAlt1[i]- (min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1)))/((max(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i]))+0.05)-((min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1))))
  DB$D_Alt_2[i] = (DB$DISTAlt2[i]- (min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1)))/((max(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i]))+0.05)-((min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1))))
  DB$D_Alt_3[i] = (DB$DISTAlt3[i]- (min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1)))/((max(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i]))+0.05)-((min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1))))
  DB$D_Alt_4[i] = (DB$DISTEC[i]- (min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1)))/((max(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i]))+0.05)-((min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1))))
  
  if(DB$HPICOHVALLE[i] == 1){
    DB$Vel_fl_Alt1[i] = 32/58;
    DB$Vel_fl_Alt2[i] = 32/58;
    DB$Vel_fl_Alt3[i] = 32/58;
    DB$Vel_fl_Alt4[i] = 32/58}
  else{DB$Vel_fl_Alt1[i] = 24/58;
  DB$Vel_fl_Alt2[i] = 24/58;
  DB$Vel_fl_Alt3[i]= 24/58; 
  DB$Vel_fl_Alt4[i]= 24/58}
  
  DB$T_fl_Alt1[i] = DB$DISTAlt1[i]/DB$Vel_fl_Alt1[i]
  DB$T_fl_Alt2[i] = DB$DISTAlt2[i]/DB$Vel_fl_Alt2[i]
  DB$T_fl_Alt3[i] = DB$DISTAlt3[i]/DB$Vel_fl_Alt3[i]
  DB$T_fl_Alt4[i] = DB$DISTEC[i]/DB$Vel_fl_Alt4[i]
  
  DB$CG_Alt_1[i] = DB$TIEMPOAlt1[i]/DB$T_fl_Alt1[i]
  DB$CG_Alt_2[i] = DB$TIEMPOAlt2[i]/DB$T_fl_Alt2[i]
  DB$CG_Alt_3[i] = DB$TIEMPOAlt3[i]/DB$T_fl_Alt3[i]
  DB$CG_Alt_4[i] = DB$TIEMPOEC[i]/DB$T_fl_Alt4[i]
  
  DB$T_rf_Alt1[i] = (DB$T_fl_Alt1[i]-(min(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-1)))/((max(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i]))+0.2)-(min(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-1)))
  DB$T_rf_Alt2[i] = (DB$T_fl_Alt2[i]-(min(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-1)))/((max(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i]))+0.2)-(min(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-1)))
  DB$T_rf_Alt3[i] = (DB$T_fl_Alt3[i]-(min(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-1)))/((max(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i]))+0.2)-(min(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-1)))
  DB$T_rf_Alt4[i] = (DB$T_fl_Alt4[i]-(min(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-1)))/((max(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i]))+0.2)-(min(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-1)))
  
}

DB$UsoCel_Poco <- (ifelse((DB$UsoCel == 1), 1,0))
#DB$UsoCel_AlgVec <- (ifelse((DB$UsoCel == 3 ), 1,0))
DB$UsoCel_Frec <- (ifelse((DB$UsoCel > 1 ), 1,0))


## Se ELIMINA LA RUTA 3

DB <- DB %>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(CHOICE %in% c("3")))

prop.table(table(DB$CHOICE))
#registros <- createDataPartition(DB$CHOICE, p = 0.70, list = FALSE)
#DB_Muestra <- DB[registros,]
#DB_test <- DB[-registros,]



DB_Muestra <- DB %>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(MUESTRA %in% c("0")))


## Se guarda la DB para usar en los modelos
write.table(DB_Muestra, 
            file="/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBMuestraArt3_3Rutas.csv", sep="\t", dec=".")


# DB Validacion

DB_test <- DB %>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(MUESTRA %in% c("1")))

write.table(DB_test, 
            file="/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBValidacion_3Rutas.csv", sep="\t", dec=".")


