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

DBMLogit <- readRDS("/Users/williz/Desktop/ModelosED/Database/DBModLog1.rds")
head(DBMLogit)
head(DB)
str(DBMLogit)

DBML <- DBMLogit %>%
  select_all() %>%
  # Base de datos Conjunta
  inner_join(DB %>%
               select(ViajeId:UsoCelular),
             by = "ViajeId") 
view(DBML)  


saveRDS(DBML, 
        file="/Users/williz/Dropbox/Doctorado/Resultados Tesis/BasesDatos/DBMLConjunto.rds")




