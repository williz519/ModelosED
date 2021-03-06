
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

# Limpiar Entorno de trabajo
rm(list = ls())


DB = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBCompleta_ModeloLogitVL.csv", sep="\t", dec=".")

names(DB)
summary(DB)

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

summary(DB)
names(DB)

#DB Completa

# Guardar los datos para alimentar la WebApp
saveRDS(DB, 
            file="/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBCOMPLETA.rds")


# DB Muestra

#MUESTRA <- DB %>%
#  # Filtrar viajes Eliminados en la primera revisión
#  filter(!(MUESTRA %in% c("0")))

write.table(DB, 
            file="/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv", sep="\t", dec=".")


# DB Validacion

VALIDACION <- DB %>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(MUESTRA %in% c("1")))

write.table(VALIDACION, 
            file="/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBValidacion.csv", sep="\t", dec=".")


  

