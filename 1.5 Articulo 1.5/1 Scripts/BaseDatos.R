
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


workingDirectory="/Users/williz/Desktop/ModelosED/1.5 Articulo 1.5/3 Resultados"
setwd(workingDirectory)

# Cargar datos
Datos   <- readRDS("/Users/williz/Desktop/ModelosED/1.5 Articulo 1.5/2 Database/DBCOMPLETA.rds")
Rutas   <- readRDS("/Users/williz/Desktop/ModelosED/1.5 Articulo 1.5/2 Database/Rutas.rds")

db   <- Datos %>%
  select(ViajeId:USOCINTURON) %>%
  # Rutas 
  inner_join(Rutas %>%
               select(ViajeId, Duracion, Distancia, V_promedio, V_total),
             by = "ViajeId") 


# GENERAR FACORES DE LAS VARIABLES CUALITATIVAS

db <- db %>%
  mutate(
  GENERO        = factor(GENERO,
                         levels = 1:3,
                         labels = c("Masculino","Femenino","Otro")),
  
  CLIMA         = factor(CLIMA,
                         levels = 1:2,
                         labels = c("Despejado",
                                    "Lluvioso")),
  CONGESTION    = factor(CONGESTION,
                         levels = 1:6,
                         labels = c("Nivel A",
                                    "Nivel B",
                                    "Nivel C",
                                    "Nivel D",
                                    "Nivel E",
                                    "Nivel F")),
  PAVIMENTO     = factor(PAVIMENTO,
                         levels = 1:3,
                         labels = c("Óptimas condiciones",
                                    "Regular estado",
                                    "Mal estado")),
  INCIDENTE     = factor(INCIDENTE,
                         levels = 1:2,
                         labels = c("No","Si")),
  
  MERIDIANO     = factor(MERIDIANO, 
                         levels = 1:2,
                         labels = c("AM", "PM")),
  
  HPICOHVALLE   = factor(HPICOHVALLE,
                         levels = 1:2,
                         labels = c("Valle","Pico")),
  
  EDAD          = factor(EDAD, 
                          levels = 1:4,
                          labels = c("18-29", "30-40", "41-60", "Mayor 60")),
  
  NIVEL_EDUCATIVO = factor(NIVEL_EDUCATIVO,
                            levels = 0:4,
                            labels = c("Primaria", "Bachiller", "Tecnología", "Profesional", "NR")),
  
  Experiencia = factor(Experiencia,
                        levels = 1:5,
                        labels = c("Menos 2 años", "De 2 a 5 años", "De 5 a 8 años", "De 8 a 12 años", "Más de 12 años")),
  
  PasoPeaton = factor(PasoPeaton,
                       levels = 1:5,
                       labels = c("Nunca", "Casi Nunca", "Ocasionalmente", "Casi siempre", "Siempre")),
  
  UsoPito = factor(UsoPito,
                    levels = 1:5,
                    labels = c("Nunca", "Casi Nunca", "Ocasionalmente", "Casi siempre", "Siempre")),
  
  CinSeg = factor(CinSeg,
                   levels = 1:2,
                   labels = c("No", "Si")),
  
  FRbr = factor(FRbr,
                 levels = 1:5,
                 labels = c("Nunca", "Casi Nunca", "Ocasionalmente", "Casi siempre", "Siempre")),
  
  UsoDirec = factor(UsoDirec,
                     levels = 1:5,
                     labels = c("Nunca", "Casi Nunca", "Ocasionalmente", "Casi siempre", "Siempre")),
  
  EnfCond = factor(EnfCond,
                    levels = 1:5,
                    labels = c("Nunca", "Casi Nunca", "Ocasionalmente", "Casi siempre", "Siempre")),
  
  AFrSem = factor(AFrSem,
                   levels = 1:5,
                   labels = c("Nunca", "Casi Nunca", "Ocasionalmente", "Casi siempre", "Siempre")),
  
  CulFr = factor(CulFr,
                  levels = 1:5,
                  labels = c("Nunca", "Casi Nunca", "Ocasionalmente", "Casi siempre", "Siempre")),
  
  OmLmVel = factor(OmLmVel,
                    levels = 1:5,
                    labels = c("Nunca", "Casi Nunca", "Ocasionalmente", "Casi siempre", "Siempre")),
  
  IgPare = factor(IgPare,
                levels = 1:5,
                labels = c("Nunca", "Casi Nunca", "Ocasionalmente", "Casi siempre", "Siempre")),
  
  UsoCel = factor(UsoCel,
                   levels = 1:5,
                   labels = c("Nunca", "Casi Nunca", "Ocasionalmente", "Casi siempre", "Siempre")),
  
  ComVrb = factor(ComVrb,
                   levels = 1:5,
                   labels = c("Muy Serio", "Serio", "Indiferente", "Conversador", "Muy Conversador")),
  
  Ans = factor(Ans,
                levels = 1:5,
                labels = c("Relajado", "Algo relajado", "Neutro", "Algo tenso", "Tenso")),
  
  ComAfec = factor(ComAfec,
                    levels = 1:5,
                    labels = c("Muy Amable", "Amable", "Neutro", "Antipatico", "Muy Antipatico")),
  
  PrPer = factor(PrPer,
                 levels = 1:5,
                 labels = c("Muy Ordenado", "Ordenado", "Neutro", "Desordenado", "Muy desordenado")),
  
  AmbTr = factor(AmbTr,
                  levels = 1:5,
                  labels = c("Muy Ordenado",
                             "Ordenado",
                             "Neutro",
                             "Desordenado",
                             "Muy desordenado")),
  
  StrC = factor(StrC,
                 levels = 1:5,
                 labels = c("Muy Tranquilo",
                            "Tranquilo",
                            "Neutro",
                            "Tensionado",
                            "Muy Tensionado")),
  
  ConCl = factor(ConCl,
                  levels = 1:5,
                  labels = c("Muy Respetuoso",
                             "Respetuoso",
                             "Indiferente",
                             "Irrespetuoso",
                             "Muy Irrespetuoso")),
  
  DispMob = factor(DispMob,
                    levels = 1:6,
                    labels = c("Ninguno",
                               "Tappsy",
                               "EasyTaxi",
                               "Coopebombas",
                               "Tax Individual",
                               "Otra")))


# Guardar los datos para alimentar Analizar en Estadisticas

saveRDS(db, "/Users/williz/Desktop/ModelosED/1.5 Articulo 1.5/2 Database/DataBase.rds")


#######################################################################################

