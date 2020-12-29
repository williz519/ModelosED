# ################################################################# #
#### CARGAR BIBLIOTECA                                           ####
# ################################################################# #

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


## Modelo sin la ruta 3
### Limpiar memoria
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/Modelos de Prueba/ICVL_MCond_4F"
setwd(workingDirectory)

# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

#Reemplazar la ruta 3 en la ruta 2
#database$CHOICE[database$CHOICE == 3 ]<-2
#database$CHOICE[database$CHOICE == 4]<- 3

### Create new variable with time

#for (i in 1:nrow(database)) {
#  database$TIEMPOAlt23[i] = (database$TIEMPOAlt2[i]+ database$TIEMPOAlt3[i])/2
#  database$DISTAlt23[i] = database$DISTAlt2[i]}

names(database)
# CREAR NUEVA DATABASE CON VARIABLES QUE SE REQUIEREN PARA LA EVALUACION

db <- select(database, c(ViajeId,CONG_AB_A1, CONG_CD_A1, CONG_EF_A1,
                         CONG_AB_A2, CONG_CD_A2,CONG_EF_A2, 
                         CONG_AB_A3, CONG_CD_A3,CONG_EF_A3, 
                         CONG_AB_EC, CONG_CD_EC, CONG_EF_EC, 
                         Acc_A1, Acc_A2, Acc_A3, Acc_EC, 
                         CamFD_A1, CamFD_A2,CamFD_A3, CamFD_EC, 
                         SEM_A1_km, SEM_A2_km, SEM_A3_km, SEM_EC_km, 
                         SI_PANEL_A1, SI_PANEL_A2, SI_PANEL_A3, SI_PANEL_EC ,  
                         SI_ZER_A1, SI_ZER_A2, SI_ZER_A3, SI_ZER_EC, CHOICE))

# Calificación Congestion
R1_Cong <- c(); R2_Cong <- c(); R3_Cong <- c() ; R4_Cong <- c()
#Calificacion Accidentes
R1_Acc <- c(); R2_Acc <- c(); R3_Acc <- c(); R4_Acc <- c()
# Camara de Fotodetección
R1_CamFD <- c(); R2_CamFD <- c(); R3_CamFD <- c(); R4_CamFD <- c()
# Semaforos
R1_SEM <- c(); R2_SEM <- c(); R3_SEM <- c(); R4_SEM <- c()
# Paneles
R1_PANEL <- c(); R2_PANEL <- c(); R3_PANEL <- c(); R4_PANEL <- c()
# Zona de Estacionamientos Regularos
R1_ZER <- c(); R2_ZER <- c(); R3_ZER <- c(); R4_ZER <- c()


for (i in 1:nrow(db)){
  #Congestión
  if(db$CONG_AB_A1[i] == 1){R1_Cong[i] <- 5}
  else{if(db$CONG_CD_A1[i] == 1){
    R1_Cong[i] <- 3}
    else{R1_Cong[i] <- 1}}

  if(db$CONG_AB_A2[i] == 1){R2_Cong[i] <- 5}
  else{if(db$CONG_CD_A2[i] == 1){
    R2_Cong[i] = 3}
    else{R2_Cong[i] = 1}}
  
  if(db$CONG_AB_A3[i] == 1){R3_Cong[i] = 5}
  else{if(db$CONG_CD_A3[i] == 1){
    R3_Cong[i] = 3}
    else{R3_Cong[i] = 1}}
  
  if(db$CONG_AB_EC[i] == 1){R4_Cong[i] = 5}
  else{if(db$CONG_CD_EC[i] == 1){
    R4_Cong[i] = 3}
    else{R4_Cong[i] = 1}}

  # Accidentes
  if (db$Acc_A1[i] == 0){
    R1_Acc[i] = 5}
  else{ if(db$Acc_A1[i] == 999){
    R1_Acc[i] = 2}
    else{if (db$Acc_A1[i] == 1){
      R1_Acc[i] = 3}
      else{if (db$Acc_A1[i] >= 2){
        R1_Acc[i] = 2}
        else{R1_Acc[i] = 1}
      }}}
  
  if (db$Acc_A2[i] == 0){
    R2_Acc[i] = 5}
  else{ if(db$Acc_A2[i] == 999){
    R2_Acc[i] = 2}
    else{if (db$Acc_A2[i] == 1){
      R2_Acc[i] = 3}
      else{if (db$Acc_A2[i] >= 2){
        R2_Acc[i] = 2}
        else{R2_Acc[i] = 1}
      }}}
  
  if (db$Acc_A3[i] == 0){
    R3_Acc[i] = 5}
  else{ if(db$Acc_A3[i] == 999){
    R3_Acc[i] = 2}
    else{if (db$Acc_A3[i] == 1){
      R3_Acc[i] = 3}
      else{if (db$Acc_A3[i] >= 2){
        R3_Acc[i] = 2}
        else{R3_Acc[i] = 1}
      }}}
  
  if (db$Acc_EC[i] == 0){
    R4_Acc[i] = 5}
  else{ if(db$Acc_EC[i] == 999){
    R4_Acc[i] = 2}
    else{if (db$Acc_EC[i] == 1){
      R4_Acc[i] = 3}
      else{if (db$Acc_EC[i] >= 2){
        R4_Acc[i] = 2}
        else{R2_Acc[i] = 1}
      }}}

  # Camaras de Fotodetección
  if (db$CamFD_A1[i] == 0){
    R1_CamFD[i] = 5}
  else{ if(db$CamFD_A1[i] == 1){
    R1_CamFD[i] = 4}
    else{ if(db$CamFD_A1[i] == 2){
      R1_CamFD[i] = 3}
      else{ if(db$CamFD_A1[i]==3 | db$CamFD_A1[i]== 4 | db$CamFD_A1[i]==5){
        R1_CamFD[i] = 2}
        else{R1_CamFD[i] = 1}
      }}}
  
  if (db$CamFD_A2[i] == 0){
    R2_CamFD[i] = 5}
  else{if (db$CamFD_A2[i] == 1){
      R2_CamFD[i] = 4}
      else{ if(db$CamFD_A2[i] == 2){
        R2_CamFD[i] = 3}
        else{ if(db$CamFD_A2[i]==3 | db$CamFD_A2[i]== 4 | db$CamFD_A2[i]==5){
          R2_CamFD[i] = 2}
          else{R2_CamFD[i] = 1}
        }}}
  
  if (db$CamFD_A3[i] == 0){
    R3_CamFD[i] = 5}
  else{if (db$CamFD_A3[i] == 1){
    R3_CamFD[i] = 4}
    else{ if(db$CamFD_A3[i] == 2){
      R3_CamFD[i] = 3}
      else{ if(db$CamFD_A3[i]==3 | db$CamFD_A3[i]== 4 | db$CamFD_A3[i]==5){
        R3_CamFD[i] = 2}
        else{R3_CamFD[i] = 1}
      }}}
  
  if (db$CamFD_EC[i] == 0){
    R4_CamFD[i] = 5}
  else{if (db$CamFD_EC[i] == 1){
    R4_CamFD[i] = 4}
    else{ if(db$CamFD_EC[i] == 2){
      R4_CamFD[i] = 3}
      else{ if(db$CamFD_EC[i]==3 | db$CamFD_EC[i]== 4 | db$CamFD_EC[i]==5){
        R4_CamFD[i] = 2}
        else{R4_CamFD[i] = 1}
      }}}
  }

for (i in 1:nrow(db)){
  if (db$SEM_A1_km[i] <= 1 ){
    R1_SEM[i] = 5}
  else{if (db$SEM_A1_km[i] <= 2){
    R1_SEM[i] = 4}
    else{ if(db$SEM_A1_km[i] <= 3){
      R1_SEM[i] = 3}
      else{ if(db$SEM_A1_km[i]<= 4){
        R1_SEM[i] = 2}
        else{R1_SEM[i] = 1}
      }}}
  
  if (db$SEM_A2_km[i] <= 1 ){
    R2_SEM[i] = 5}
  else{if (db$SEM_A2_km[i] <= 2){
    R2_SEM[i] = 4}
    else{ if(db$SEM_A2_km[i] <= 3){
      R2_SEM[i] = 3}
      else{ if(db$SEM_A2_km[i]<= 4){
        R2_SEM[i] = 2}
        else{R2_SEM[i] = 1}
      }}}
  
  if (db$SEM_A3_km[i] <= 1 ){
    R3_SEM[i] = 5}
  else{if (db$SEM_A3_km[i] <= 2){
    R3_SEM[i] = 4}
    else{ if(db$SEM_A3_km[i] <= 3){
      R3_SEM[i] = 3}
      else{ if(db$SEM_A3_km[i]<= 4){
        R3_SEM[i] = 2}
        else{R3_SEM[i] = 1}
      }}}
  
  if (db$SEM_EC_km[i] <= 1 ){
    R4_SEM[i] = 5}
  else{if (db$SEM_EC_km[i] <= 2){
    R4_SEM[i] = 4}
    else{ if(db$SEM_EC_km[i] <= 3){
      R4_SEM[i] = 3}
      else{ if(db$SEM_EC_km[i]<= 4){
        R4_SEM[i] = 2}
        else{R4_SEM[i] = 1}
      }}}
  
  if (db$SI_ZER_A1[i] == 1){
    R1_ZER[i] = 2}
  else{R1_ZER[i] = 1}
  
  if (db$SI_ZER_A2[i] == 1){
    R2_ZER[i] = 2}
  else{R2_ZER[i] = 1}
  
  if (db$SI_ZER_A3[i] == 1){
    R3_ZER[i] = 2}
  else{R3_ZER[i] = 1}
  
  if (db$SI_ZER_EC[i] == 1){
    R4_ZER[i] = 2}
  else{R4_ZER[i] = 1}
  
  if (db$SI_PANEL_A1[i] == 1){
    R1_PANEL[i] = 2}
  else{R1_PANEL[i] = 1}
  
  if (db$SI_PANEL_A2[i] == 1){
    R2_PANEL[i] = 2}
  else{R2_PANEL[i] = 1}
  
  if (db$SI_PANEL_A3[i] == 1){
    R3_PANEL[i] = 2}
  else{R3_PANEL[i] = 1}
  
  if (db$SI_PANEL_EC[i] == 1){
    R4_PANEL[i] = 2}
  else{R4_PANEL[i] = 1}
}



## Rankings
R1<-c();R2<-c();R3<-c();R4<-c(); Eleccion <-c(); Ruta <-c()

for (i in 1:nrow(db)){
  R1[i] = R1_Cong[i] * R1_Acc[i] * R1_CamFD[i] * R1_SEM[i] #* R1_ZER[i] * R1_PANEL[i]
  R2[i] = R2_Cong[i] * R2_Acc[i] * R2_CamFD[i] * R2_SEM[i] #* R2_ZER[i] * R2_PANEL[i]
  R3[i] = R3_Cong[i] * R3_Acc[i] * R3_CamFD[i] * R3_SEM[i] #* R3_ZER[i] * R3_PANEL[i]
  R4[i] = R4_Cong[i] * R4_Acc[i] * R4_CamFD[i] * R4_SEM[i] #* R4_ZER[i] * R4_PANEL[i]
}

R <- cbind(R1,R2,R3,R4)

for (i in 1:nrow(db)){
  if(max(R1[i],R2[i],R3[i],R4[i])==R1[i]){Ruta[i] = 1}
  else{if(max(R2[i],R3[i],R4[i])== R2[i]){Ruta[i] = 2}
    else{if(max(R3[i],R4[i])==R3[i]){Ruta[i] = 3}
      else{Ruta[i]= 4}}}
  
  if(Ruta[i]==db$CHOICE[i]){Eleccion[i] = "SI"}
  else{Eleccion[i] = "NO"}
}
  
Ranking <- cbind(R, Ruta, db$CHOICE, Eleccion)

table(Eleccion)


