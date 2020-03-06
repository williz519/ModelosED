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

#Cargas las Bases de Datos

Rutas <-read.csv("/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVLCE.csv",sep="\t", header=TRUE)

names(Rutas)

Rutas1 <- Rutas %>%
  select("ViajeId","DISTAlt1", "TIEMPOAlt1","CONG_A1",                        
         "DISTAlt2","TIEMPOAlt2","CONG_A2" ,                       
         "DISTAlt3", "TIEMPOAlt3", "CONG_A3",
         "DISTEC","TIEMPOEC","CONGESTION",
         "CHOICE")

names(Rutas1)

write.table(Rutas1, 
            file="/Users/williz/Desktop/ModelosED/DBRutas.csv", sep="\t", dec=".")

Rutas2 <-read.csv("/Users/williz/Desktop/ModelosED/DBRutas.csv",sep="\t", header=TRUE)

fix(Rutas2)
