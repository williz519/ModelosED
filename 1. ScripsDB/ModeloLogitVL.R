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

Google <-read_xlsx("/Users/williz/Desktop/ModelosED/Database/DBRutasGoogle.xlsx")

RutasGoogle <- Google %>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(ViajeId %in% c("{3E49E5E4-D7D2-E811-8FB7-74867AD5B714}",
                          "{FBDD1371-1AC1-E811-914C-74867AD5B714}")))

Rutas200 <-readRDS("/Users/williz/Desktop/ModelosED/Database/RutasVelocidades/Rutas200.rds")

#Base de datos creada en DBModoConduccion.r Escala 1-3
#DBModoConduccion <- readRDS("/Users/williz/Desktop/ModelosED/Database/DBModoConduccion.rds")

#Base de datos creada en DBModoConduccion.r Escala 1-5
DBModoConduccion <- readRDS("/Users/williz/Desktop/ModelosED/Database/DBModoCondCE.rds")


#Base de datos creada en DBPersonalidad.r
DBPersonalidad <- readRDS("/Users/williz/Desktop/ModelosED/Database/DBPersonalidad.rds")


names(Rutas200)
names(RutasGoogle)
names(DBModoConduccion)
names(DBPersonalidad)


RutasGoogle <- rename(RutasGoogle, replace = c("A1_Distancia Km" ="DISTAlt1", 
                      "A1_Tiempo Min"="TIEMPOAlt1",
                      A1_Color="CONG_A1", 
                      "A2_Distancia Km" ="DISTAlt2", 
                      "A2_Tiempo Min" ="TIEMPOAlt2", 
                      A2_Color="CONG_A2", 
                      "A3_Distancia Km"="DISTAlt3", 
                      "A3_Tiempo Min" ="TIEMPOAlt3",
                      A3_Color="CONG_A3", 
                      AT_Distancia="DISTEC", 
                      AT_Tiempo="TIEMPOEC", 
                      Costo="COSTO", 
                      Choice="CHOICE"))

RutasGoogle$CONG_A1 <- factor(RutasGoogle$CONG_A1,
                    levels = c("SinInf","Verde","Amarillo","Rojo","Negro"),
                    labels = c('0','1','2','3','4'))
RutasGoogle$CONG_A2 <- factor(RutasGoogle$CONG_A2,
                              levels = c("SinInf","Verde","Amarillo","Rojo","Negro"),
                              labels = c('0','1','2','3','4'))
RutasGoogle$CONG_A3 <- factor(RutasGoogle$CONG_A3,
                              levels = c("SinInf","Verde","Amarillo","Rojo","Negro"),
                              labels = c('0','1','2','3','4'))



Datos <- RutasGoogle %>%
  select(ViajeId,"DISTAlt1", "TIEMPOAlt1","CONG_A1",
         "DISTAlt2", "TIEMPOAlt2", "CONG_A2", 
         "DISTAlt3", "TIEMPOAlt3","CONG_A3", 
         "DISTEC", "TIEMPOEC",
         "COSTO", "CHOICE") %>%
  # Rutas Google
  inner_join(Rutas200 %>%
               select(ViajeId,Clima,Congestion,Pavimento,Incidente,
                      Meridiano,Horario),
             by = "ViajeId") %>%
  # Modo conducción
  inner_join(DBModoConduccion %>%
               select(ViajeId:DispMob),
             by = "ViajeId") %>%
  # Personalidad conductor
  inner_join(DBPersonalidad %>%
               select(ViajeId:Experiencia),
             by = "ViajeId")

names(Datos)

#Eliminamos 
Datos <- Datos[ ,!colnames(Datos)=="geometry"]
names(Datos)

#Cambiar Nombres de las Variables

Datos <- rename(Datos, replace = c(Clima="CLIMA", 
                               Congestion ="CONGESTION",
                               Pavimento="PAVIMENTO", 
                               Incidente="INCIDENTE", 
                               Meridiano = "MERIDIANO",
                               Horario ="HPICOHVALLE",
                               TiempoProf = "TIEMPO_PROFESION",
                               Edad = "EDAD",
                               NivEdu = "NIVEL_EDUCATIVO",
                               HorTrDia = "HORAS_TRABAJO",
                               Genero = "GENERO"))

names(Datos)

# Ordenamos la Base de datos

head(Datos)

#Convertir Factores en Variables Numericas
str(Datos$HPICOHVALLE)

Datos$CLIMA <- factor(Datos$CLIMA,
                      levels = c("Despejado", "Lluvioso"),
                      labels = c('1','2'))

Datos$CONGESTION <- factor(Datos$CONGESTION,
                           levels = c("Nivel A: flujo libre",
                                      "Nivel B: flujo libre con restricciones",
                                      "Nivel C: flujo medio",
                                      "Nivel D: flujo estable",
                                      "Nivel E: flujo bajo",
                                      "Nivel F: flujo congestionado"),
                           labels = c('1','2','3','4','5','6'))

Datos$PAVIMENTO <- factor(Datos$PAVIMENTO,
                          levels = c("Óptimas condiciones",
                                     "Regular estado",
                                     "Mal estado"),
                          labels = c('1','2','3'))

Datos$INCIDENTE <- factor(Datos$INCIDENTE,
                          levels = c("No", "Si"),
                          labels = c('1','2'))

Datos$MERIDIANO <- factor(Datos$MERIDIANO,
                          levels = c("AM","PM"),
                          labels = c('1','2'))

Datos$HPICOHVALLE <- factor(Datos$HPICOHVALLE,
                            levels = c("Valle","Pico"),
                            labels = c('1','2'))


names(Datos)

#ordenamos la base de datos

DBModelo <- Datos %>%
  select("ViajeId", "GENERO", "TIEMPO_PROFESION", "HORAS_TRABAJO","NIVEL_EDUCATIVO",
         "EDAD","DISTAlt1", "TIEMPOAlt1","CONG_A1",                        
         "DISTAlt2","TIEMPOAlt2","CONG_A2" ,                       
         "DISTAlt3", "TIEMPOAlt3", "CONG_A3",
         "DISTEC","TIEMPOEC","INFOTRAFICO","CLIMA","CONGESTION","PAVIMENTO",
         "INCIDENTE", "MERIDIANO", "HPICOHVALLE","COSTO", "CHOICE",
         "Experiencia", "PasoPeaton", "UsoPito",
         "CinSeg","FRbr", "UsoDirec",
         "EnfCond", "AFrSem", "CulFr",
         "OmLmVel" , "IgPare", "UsoCel",
         "ComVrb", "Ans", "ComAfec", "PrPer", "AmbTr","StrC","ConCl","DispMob")

names(DBModelo)
view(DBModelo)

#Eliminar Variables en el DB

write.table(DBModelo, 
            file="/Users/williz/Desktop/ModelosED/Database/ModeloLogitVL.csv", sep="\t", dec=".")

# EL ANALISIS CONTINUA EN EL SCRIPT AFACTORIALCONJUNTO

