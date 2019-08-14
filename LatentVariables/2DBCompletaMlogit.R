#Installing the Psych package and loading it
#install.packages("psych")

library(psych)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(leaflet)
library(RColorBrewer)
library(sqldf)
require(reshape)
require(dplyr)
require(psych)
require(GGally)
library(corrplot)
library(corrr)

#Leer la dataset 

#Leer los dataset Personalidad y Modo Conduccion

DBPersonalidad <- readRDS(file="/Users/williz/Desktop/ModelosED/Database/DBPersonalidad.rds")

DBModoConduccion <- readRDS(file="/Users/williz/Desktop/ModelosED/Database/DBModoConduccion.rds")

names(DBPersonalidad)

names(DBModoConduccion)

# Creación Base de datos Completa para modelo Logit
# DBmodlog1.rds es creada a partir del script DBModeloLogit.R

DBMLogit <- readRDS("/Users/williz/Desktop/ModelosED/Database/DBModLog1.rds")
head(DBMLogit)
str(DBMLogit)

DBML <- DBMLogit %>%
  select_all() %>%
  # Base de datos Conjunta
  inner_join(DBPersonalidad %>%
               select_all(),
             by = "ViajeId") %>%
  inner_join(DBModoConduccion %>%
               select_all(),
             by = "ViajeId")

#view(DBML)  
names(DBML)

head(DBML)
str(DBML)


names(DBML)

DBML$TIEMPO_PROFESION[DBML$TIEMPO_PROFESION <= 0] <- 0.5
DBML$HORASTRABAJO[DBML$HORASTRABAJO <= 0] <- 0.5



#Eliminar Variables que no importan
DBML <- DBML[ ,!colnames(DBML)=="IdViaje"]
DBML <- DBML[ ,!colnames(DBML)=="CODVIAJE"]
DBML <- DBML[ ,!colnames(DBML) =="NivelEducativo"]


names(DBML)

# Ordenar las variables
#DBML = DBML[ , c(1,2,14,3,4,5,6,7,8,9,10,11,12,13,30,31,32,33,34,35,36,37,38,
#                 39,40,41,42,43,44,45,46,47,48,49,50,51,52,15,16,17,18,
#                 19,20,21,22,23,24,25,26,27,28,29)]

names(DBML)

DBModLogitVL <- DBML[ ,!colnames(DBML)=="ViajeId"]

names(DBModLogitVL)


str(DBModLogitVL)

DBModLogitVL<- data.frame(DBModLogitVL %>%
                            #Filtrar los puntos a excluir
                            filter(CHOICE!=0))

view(DBModLogitVL)

table(DBModLogitVL$GENERO)

# Base de datos con sin Cambiar escala
#write.table(DBModLogitVL,file="/Users/williz/Dropbox/Doctorado/Resultados Tesis/BasesDatos/DBModLogitVL2.csv", sep="\t", dec=".")

# Base de datos con Cambio de escala
#write.table(DBModLogitVL,file="/Users/williz/Dropbox/Doctorado/Resultados Tesis/BasesDatos/DBModLogitVL.csv", sep="\t", dec=".")

saveRDS(DBModLogitVL, file="/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL.rds")





