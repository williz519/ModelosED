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

DB$DISTAlt2
for (i in 1:nrow(DB)){
  if (DB$DISTAlt2[i] == 0){
    DB$DISTAlt2[i] = mean(DB$DISTAlt1[i],DB$DISTAlt3[i])}
  else {DB$DISTAlt2[i]}
  if (DB$DISTAlt3[i] == 0){
    DB$DISTAlt3[i] = mean(DB$DISTAlt1[i],DB$DISTAlt2[i])}
  else {DB$DISTAlt3[i]}
  
  if (DB$TIEMPOAlt2[i] == 0){
    DB$TIEMPOAlt2[i] = mean(DB$TIEMPOAlt1[i],DB$TIEMPOAlt3[i])}
  else {DB$TIEMPOAlt2[i]}
  if (DB$TIEMPOAlt3[i]==0){
    DB$TIEMPOAlt3[i] = mean(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i])}
  else {DB$TIEMPOAlt3[i]}
}


#VARIABLE SEMAFOROS

for (i in 1:nrow(DB)){
  if (DB$Semaf_A1[i] == 999){
    DB$Semaf_A1[i] = mean(DB$Semaf_A2[i],DB$Semaf_A3[i])}
  else {DB$Semaf_A1[i]}
  if (DB$Semaf_A2[i] == 999){
    DB$Semaf_A2[i] = mean(DB$Semaf_A1[i],DB$Semaf_A3[i])}
  else {DB$Semaf_A2[i]}
  if (DB$Semaf_A3[i] == 999){
    DB$Semaf_A3[i] = mean(DB$Semaf_A1[i],DB$Semaf_A2[i])}
  else {DB$Semaf_A3[i]}
  if (DB$Semaf_EC[i] == 999){
    DB$Semaf_EC[i] = mean(DB$Semaf_A1[i],DB$Semaf_A2[i], DB$Semaf_A3[i])}
  else {DB$Semaf_EC[i]}
  
  if (DB$ZER_A2[i] == 999){
    DB$ZER_A2[i] = min(DB$ZER_A1[i],DB$ZER_A3[i])}
  if (DB$ZER_A3[i] == 999){
    DB$ZER_A3[i] = min(DB$ZER_A1[i],DB$ZER_A2[i])}
  else{DB$ZER_A3[i]}
  if (DB$ZER_EC[i] == 999){
    DB$ZER_EC[i] = min(DB$ZER_A1[i],DB$ZER_A2[i],DB$ZER_A3[i])}
  else{DB$ZER_EC[i]}
}

# VARIABLE PANELES

for (i in 1:nrow(DB)) {
  if (DB$Paneles_A2[i] == 999){
    DB$Paneles_A2[i] = mean(DB$Paneles_A1[i],DB$Paneles_A3[i])}
  else{
    DB$Paneles_A2[i]
  }
  if (DB$Paneles_A3[i] == 999){
    DB$Paneles_A3[i] = mean(DB$Paneles_A1[i],DB$Paneles_A3[i])}
  else{
    DB$Paneles_A3[i]
  }
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
  DB$T_Alt_1[i] = (DB$TIEMPOAlt1[i]- (min(c(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-3)))/(max(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-(min(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-3))
  DB$T_Alt_2[i] = (DB$TIEMPOAlt2[i]- (min(c(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-3)))/(max(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-(min(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-3))
  DB$T_Alt_3[i] = (DB$TIEMPOAlt3[i]- (min(c(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-3)))/(max(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-(min(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-3))
  DB$T_Alt_4[i] = (DB$TIEMPOEC[i]- (min(c(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-3)))/(max(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-(min(DB$TIEMPOAlt1[i],DB$TIEMPOAlt2[i],DB$TIEMPOAlt3[i],DB$TIEMPOEC[i])-3))
  
  # Normalización de la variable distancia
  DB$D_Alt_1[i] = (DB$DISTAlt1[i]- (min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1)))/(max(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i]))-(min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1)))
  DB$D_Alt_2[i] = (DB$DISTAlt2[i]- (min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1)))/(max(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i]))-(min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1)))
  DB$D_Alt_3[i] = (DB$DISTAlt3[i]- (min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1)))/(max(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i]))-(min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1)))
  DB$D_Alt_4[i] = (DB$DISTEC[i]- (min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1)))/(max(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i]))-(min(c(DB$DISTAlt1[i],DB$DISTAlt2[i],DB$DISTAlt3[i],DB$DISTEC[i])-0.1)))
  
  if(DB$HPICOHVALLE[i] == 1){
    DB$Vel_fl_Alt1[i] = 28/50;
    DB$Vel_fl_Alt2[i] = 28/50;
    DB$Vel_fl_Alt3[i] = 28/50;
    DB$Vel_fl_Alt4[i] = 28/50}
  else{DB$Vel_fl_Alt1[i] = 22/50;
  DB$Vel_fl_Alt2[i] = 22/50;
  DB$Vel_fl_Alt3[i]= 22/50; 
  DB$Vel_fl_Alt4[i]= 22/50}
  
  DB$T_fl_Alt1[i] = DB$DISTAlt1[i]/DB$Vel_fl_Alt1[i]
  DB$T_fl_Alt2[i] = DB$DISTAlt2[i]/DB$Vel_fl_Alt2[i]
  DB$T_fl_Alt3[i] = DB$DISTAlt3[i]/DB$Vel_fl_Alt3[i]
  DB$T_fl_Alt4[i] = DB$DISTEC[i]/DB$Vel_fl_Alt4[i]
  
  DB$CG_Alt_1[i] = DB$TIEMPOAlt1[i]/DB$T_fl_Alt1[i]
  DB$CG_Alt_2[i] = DB$TIEMPOAlt2[i]/DB$T_fl_Alt2[i]
  DB$CG_Alt_3[i] = DB$TIEMPOAlt3[i]/DB$T_fl_Alt3[i]
  DB$CG_Alt_4[i] = DB$TIEMPOEC[i]/DB$T_fl_Alt4[i]
  
  DB$T_rf_Alt1[i] = (DB$T_fl_Alt1[i]-(min(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-3)))/(max(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-3))
  DB$T_rf_Alt2[i] = (DB$T_fl_Alt2[i]-(min(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-3)))/(max(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-3))
  DB$T_rf_Alt3[i] = (DB$T_fl_Alt3[i]-(min(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-3)))/(max(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-3))
  DB$T_rf_Alt4[i] = (DB$T_fl_Alt4[i]-(min(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-3)))/(max(c(DB$T_fl_Alt1[i],DB$T_fl_Alt2[i],DB$T_fl_Alt3[i],DB$T_fl_Alt4[i])-3))
  
}

DB$UsoCel_Poco <- (ifelse((DB$UsoCel == 1), 1,0))
#DB$UsoCel_AlgVec <- (ifelse((DB$UsoCel == 3 ), 1,0))
DB$UsoCel_Frec <- (ifelse((DB$UsoCel > 1 ), 1,0))


## Se ELIMINA LA RUTA 3

DB <- DB %>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(CHOICE %in% c("3")))

table(DB$CHOICE)
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


