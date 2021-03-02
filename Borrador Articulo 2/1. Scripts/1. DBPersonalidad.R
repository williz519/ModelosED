#Installing the Psych package and loading it
install.packages("psych")
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
library("ggplot2")


# Borrar Ambiente de trabajo
rm(list = ls())

#Leer la dataset

DB_file <- "/Users/williz/Desktop/ModelosED/Database/DB_Viajes.xlsx"

DB <- DB_file %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_xlsx, path = DB_file)

DBPersonalidad <- DB$Viajes %>%
  select(ViajeId) %>%
  # Caracterización taxista
  inner_join(DB$CaracterizacionTaxista %>%
               select(ViajeId, HorasAlDia, Edad, TiempoProfesion, 
                      NivelEducativo),
             by = "ViajeId") %>%
  # Personalidad conductor
  inner_join(DB$PersonalidadConductor %>%
               select(ViajeId,SerioConservador:AmbienteOrdenadoDesordenado),
             by = "ViajeId") %>%
  # Modo de Conduccion
  inner_join(DB$ModoConduccion %>%
               select(ViajeId,TranquilaTensionada,RespetuosaIrrespetuosa),
             by = "ViajeId")

#View(DBPersonalidad)
names(DBPersonalidad)


DBPersonalidad <- rename(DBPersonalidad, replace =c(SerioConservador = "ComVrb",
                                                    RelajadoTenso = "Ans",
                                                    AmableAntipatico = "ComAfec",
                                                    OrdenadoDesordenado = "PrPer",
                                                    AmbienteOrdenadoDesordenado = "AmbTr",
                                                    TranquilaTensionada = "StrC",
                                                    RespetuosaIrrespetuosa = "ConCl",
                                                    NivelEducativo = "NivEdu",
                                                    TiempoProfesion = "TiempoProf",
                                                    HorasAlDia = "HorTrDia"))

names(DBPersonalidad)



DBPersonalidad <- DBPersonalidad%>%
  # Filtrar viajes Eliminados en la primera revisión
  filter(!(ViajeId %in% c("{3F7B556D-577A-E811-B124-74867AD5B714}",
                          "{C7437B00-427A-E811-B124-74867AD5B714}",
                          "{7CBCE319-CD3D-E811-9CE5-74867AD5B714}",
                          "{6C895145-D23D-E811-9CE5-74867AD5B714}",
                          "{DC497116-05A7-E811-BDF0-74867AD5B714}",
                          "{D17203CB-4ACE-E811-914C-74867AD5B714}",
                          "{0FB33E62-4ECE-E811-914C-74867AD5B714}",
                          "{3C43C21E-76DB-E811-8FB7-74867AD5B714}",
                          "{3D43C21E-76DB-E811-8FB7-74867AD5B714}",
                          "{39877BE2-6B1D-E811-9CE5-74867AD5B714}")))

#view(DBPersonalidad)
names(DBPersonalidad)

 
#Reemplazar Valores Faltantes

DBPersonalidad$ComVrb[is.na(DBPersonalidad$ComVrb)] <- 5
DBPersonalidad$Ans[is.na(DBPersonalidad$Ans)] <- 6
DBPersonalidad$ComAfec[is.na(DBPersonalidad$ComAfec)] <- 6
DBPersonalidad$PrPer[is.na(DBPersonalidad$PrPer)] <- 6
DBPersonalidad$AmbTr[is.na(DBPersonalidad$AmbTr)] <- 6
DBPersonalidad$StrC[is.na(DBPersonalidad$StrC)] <- 6
DBPersonalidad$ConCl[is.na(DBPersonalidad$ConCl)] <- 6
DBPersonalidad$HorTrDia[is.na(DBPersonalidad$HorTrDia)] <- 8
DBPersonalidad$TiempoProf[is.na(DBPersonalidad$TiempoProf)] <- 0.5


#CAMBIO DE ESCALA

# Edad
DBPersonalidad$Edad[DBPersonalidad$Edad == 0] <- 2

# Tiempo Profesion
DBPersonalidad$TiempoProf[DBPersonalidad$TiempoProf == 0] <- 0.5

# Horas de trabajo al dia
DBPersonalidad$HorTrDia[DBPersonalidad$HorTrDia == 0 ] <- 6 


# Comunicacion Verbal
DBPersonalidad$ComVrb[DBPersonalidad$ComVrb == 1 | DBPersonalidad$ComVrb == 2] <- 1
DBPersonalidad$ComVrb[DBPersonalidad$ComVrb == 3 | DBPersonalidad$ComVrb == 4] <- 2
DBPersonalidad$ComVrb[DBPersonalidad$ComVrb == 5 | DBPersonalidad$ComVrb == 6] <- 3
DBPersonalidad$ComVrb[DBPersonalidad$ComVrb == 7 | DBPersonalidad$ComVrb == 8] <- 4
DBPersonalidad$ComVrb[DBPersonalidad$ComVrb == 9 | DBPersonalidad$ComVrb == 10] <- 5

table(DBPersonalidad$ComVrb)

#Stress al Conducir
DBPersonalidad$StrC[DBPersonalidad$StrC == 1] <- 1
DBPersonalidad$StrC[DBPersonalidad$StrC == 2] <- 2
DBPersonalidad$StrC[DBPersonalidad$StrC == 3 | DBPersonalidad$StrC == 4  ] <- 3
DBPersonalidad$StrC[DBPersonalidad$StrC == 5 | DBPersonalidad$StrC == 6  ] <- 4
DBPersonalidad$StrC[DBPersonalidad$StrC == 7 | DBPersonalidad$StrC == 8 | DBPersonalidad$StrC == 9 | DBPersonalidad$StrC == 10] <- 5

table(DBPersonalidad$StrC)

 #Comunicacion Afectiva
DBPersonalidad$ComAfec[DBPersonalidad$ComAfec == 1 ] <- 1
DBPersonalidad$ComAfec[DBPersonalidad$ComAfec == 2 | DBPersonalidad$ComAfec == 3 ] <- 2
DBPersonalidad$ComAfec[DBPersonalidad$ComAfec == 4 | DBPersonalidad$ComAfec == 5 ] <- 3
DBPersonalidad$ComAfec[DBPersonalidad$ComAfec == 6 | DBPersonalidad$ComAfec == 7 ] <- 4
DBPersonalidad$ComAfec[DBPersonalidad$ComAfec == 8 | DBPersonalidad$ComAfec == 9 | DBPersonalidad$ComAfec == 10] <- 5

table(DBPersonalidad$ComAfec)

#Presentacion Personal
DBPersonalidad$PrPer[DBPersonalidad$PrPer == 1 ] <- 1
DBPersonalidad$PrPer[DBPersonalidad$PrPer == 2 ] <- 2
DBPersonalidad$PrPer[DBPersonalidad$PrPer == 3 ] <- 3
DBPersonalidad$PrPer[DBPersonalidad$PrPer == 4 | DBPersonalidad$PrPer == 5 ] <- 4
DBPersonalidad$PrPer[DBPersonalidad$PrPer > 5 ] <- 5

table(DBPersonalidad$PrPer)

# Ambiente de Trabajo
DBPersonalidad$AmbTr[DBPersonalidad$AmbTr == 1 ] <- 1
DBPersonalidad$AmbTr[DBPersonalidad$AmbTr == 2 ] <- 2
DBPersonalidad$AmbTr[DBPersonalidad$AmbTr == 3 ]  <- 3
DBPersonalidad$AmbTr[DBPersonalidad$AmbTr == 4 | DBPersonalidad$AmbTr == 5] <- 4
DBPersonalidad$AmbTr[DBPersonalidad$AmbTr > 5] <- 5

table(DBPersonalidad$AmbTr)

# Ansiedad
DBPersonalidad$Ans[DBPersonalidad$Ans == 1 ] <- 1
DBPersonalidad$Ans[DBPersonalidad$Ans == 2 ] <- 2
DBPersonalidad$Ans[DBPersonalidad$Ans == 3 ]  <- 3
DBPersonalidad$Ans[DBPersonalidad$Ans == 4 | DBPersonalidad$Ans == 5 ] <- 4
DBPersonalidad$Ans[DBPersonalidad$Ans > 5] <-5

table(DBPersonalidad$Ans)

#Consideracion Cliente
DBPersonalidad$ConCl[DBPersonalidad$ConCl == 1 ] <- 1
DBPersonalidad$ConCl[DBPersonalidad$ConCl == 2 ] <- 2
DBPersonalidad$ConCl[DBPersonalidad$ConCl == 3 ] <- 3
DBPersonalidad$ConCl[DBPersonalidad$ConCl == 4 ] <- 4
DBPersonalidad$ConCl[DBPersonalidad$ConCl >= 5 ]<- 5

table(DBPersonalidad$ConCl)

#Creacion Variable Experiencia
DBPersonalidad$Experiencia <- DBPersonalidad$TiempoProf

DBPersonalidad$Experiencia[DBPersonalidad$Experiencia <= 2] <- 1
DBPersonalidad$Experiencia[DBPersonalidad$Experiencia > 2 & DBPersonalidad$Experiencia <= 5] <- 2
DBPersonalidad$Experiencia[DBPersonalidad$Experiencia > 5 & DBPersonalidad$Experiencia <= 8] <- 3
DBPersonalidad$Experiencia[DBPersonalidad$Experiencia > 8 & DBPersonalidad$Experiencia <= 12] <- 4
DBPersonalidad$Experiencia[DBPersonalidad$Experiencia > 12] <- 5


summary(DBPersonalidad)

#DBPersonalidad[c("HorTrDia","Edad","TiempoProf", "NivEdu")]<- NULL


saveRDS(DBPersonalidad, file="/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBPersonalidad.rds")


names(DBPersonalidad)

reliability(cov(DBPersonalidad))


DBPersonalidad$Edad = factor(DBPersonalidad$Edad,
                       levels = 1:4,
                       labels = c("18-29",
                                  "30-40",
                                  "41-60",
                                  "Mayor de 60"))
table(DBPersonalidad$Edad)
tab.Edad <-as.data.frame(prop.table(table(DBPersonalidad$Edad))*100)
tab.Edad

str(DBPersonalidad$Edad)

summary(DBPersonalidad$HorTrDia)
summary(DBPersonalidad$TiempoProf)

ggplot(DBPersonalidad, aes( y = DBPersonalidad$TiempoProf)) + 
  geom_boxplot() +
  xlab("Tiempo de Profesión") + 
  ylab("Años") + ggtitle("Distribución Tiempo de profesión") 

ggplot(DBPersonalidad, aes(x = DBPersonalidad$Edad, y = DBPersonalidad$HorTrDia)) + 
  geom_boxplot() +
  xlab("Edad") + ylab("Horas de trabajo al día") + labs(fill = "Rango de Edad") +
  ggtitle("Horas de trabajo por rango de edad") 


DBPersonalidad$ComVrb = factor(DBPersonalidad$ComVrb,
                             levels = 1:5,
                             labels = c("Muy Serio",
                                        "Serio",
                                        "Indiferente",
                                        "Conversador",
                                        "Muy Conversador"))

table(DBPersonalidad$ComVrb)
tab.ComVrb<-as.data.frame(prop.table(table(DBPersonalidad$ComVrb))*100)
tab.ComVrb

DBPersonalidad$StrC = factor(DBPersonalidad$StrC,
                               levels = 1:5,
                               labels = c("Muy Tranquilo",
                                          "Tranquilo",
                                          "Neutro",
                                          "Tensionado",
                                          "Muy Tensionado"))

table(DBPersonalidad$StrC)
tab.StrC<-as.data.frame(prop.table(table(DBPersonalidad$StrC))*100)
tab.StrC

DBPersonalidad$ComAfec = factor(DBPersonalidad$ComAfec,
                               levels = 1:5,
                               labels = c("Muy Amable",
                                          "Amable",
                                          "Neutro",
                                          "Antipatico",
                                          "Muy Antipatico"))


table(DBPersonalidad$ComAfec)
tab.ComAfec<-as.data.frame(prop.table(table(DBPersonalidad$ComAfec))*100)
tab.ComAfec

DBPersonalidad$PrPer = factor(DBPersonalidad$PrPer,
                                levels = 1:5,
                                labels = c("Muy Ordenado",
                                           "Ordenado",
                                           "Neutro",
                                           "Desordenado",
                                           "Muy desordenado"))

table(DBPersonalidad$PrPer)
tab.PrPer<-as.data.frame(prop.table(table(DBPersonalidad$PrPer))*100)
tab.PrPer

DBPersonalidad$AmbTr = factor(DBPersonalidad$AmbTr,
                              levels = 1:5,
                              labels = c("Muy Ordenado",
                                         "Ordenado",
                                         "Neutro",
                                         "Desordenado",
                                         "Muy desordenado"))


table(DBPersonalidad$AmbTr)
tab.AmbTr <-as.data.frame(prop.table(table(DBPersonalidad$AmbTr))*100)
tab.AmbTr


DBPersonalidad$Ans = factor(DBPersonalidad$Ans,
                              levels = 1:5,
                              labels = c("Relajado",
                                         "Algo relajado",
                                         "Neutro",
                                         "Algo tenso",
                                         "Tenso"))

table(DBPersonalidad$Ans)
tab.Ans <-as.data.frame(prop.table(table(DBPersonalidad$Ans))*100)
tab.Ans

DBPersonalidad$ConCl = factor(DBPersonalidad$ConCl,
                              levels = 1:5,
                              labels = c("Muy Respetuoso",
                                         "Respetuoso",
                                         "Indiferente",
                                         "Irrespetuoso",
                                         "Muy Irrespetuoso"))

table(DBPersonalidad$ConCl)
tab.ConCl <-as.data.frame(prop.table(table(DBPersonalidad$ConCl))*100)
tab.ConCl



summary(DBPersonalidad)

str(DBPersonalidad)

