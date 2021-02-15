
library("ggplot2")
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
library(gridExtra)
library(MASS)

# Limpiar Entorno de trabajo
rm(list = ls())


# Cargar Base de datos

db   <- readRDS("/Users/williz/Desktop/ModelosED/1.5 Articulo 1.5/2 Database/DataBase.rds")


#### Estadisticas descriptivas

# Genero
table(db$GENERO)
prop.table(table(db$GENERO))*100

# Tiempo Profesión
ggplot(db, aes(x = HORAS_TRABAJO)) + 
  geom_bar(fill="tomato3", width = 0.5) +
  xlab("Horas") + 
  ylab("Conductores") + ggtitle("Distribución de Horas de trabajo") 

# Horas da trabajo al día
table(db$HORAS_TRABAJO > 11)

# Nivel Educativo
table(db$NIVEL_EDUCATIVO)
prop.table(table(db$NIVEL_EDUCATIVO))*100


# Edad
table(db$EDAD)
prop.table(table(db$EDAD))*100

# Tabla contingencia EDAD vs Nivel educativo
x<- table(db$EDAD, db$NIVEL_EDUCATIVO)
prop.table(x,1)*100 #Proporciones de fila
prop.table(x,2)*100 #Proporciones de Columna

# Clima
prop.table(table(db$CLIMA))*100

# Meridiano
prop.table(table(db$MERIDIANO))*100

# 
table(db$EDAD, db$NIVEL_EDUCATIVO)

#Congestión
x<- table(db$MERIDIANO, db$CONGESTION)
prop.table(x,1)*100 #Proporciones de fila
prop.table(x,2)*100 #Proporciones de Columna

#Congestión
Y<- table(db$HPICOHVALLE, db$CONGESTION)
prop.table(Y,1)*100 #Proporciones de fila

#Congestión
z<- table(db$HPICOHVALLE, db$CONGESTION, db$MERIDIANO)
prop.table(z,2)*100 #Proporciones de fila

# Pavimento
prop.table(table(db$PAVIMENTO))*100

# Incidente
INC <-table(db$INCIDENTE, db$MERIDIANO)
prop.table(INC,1)*100 #Proporciones de fila

#Hora Pico - Valle
prop.table(table(db$HPICOHVALLE))*100

# Dispositivos moviles
table(db$DispMob)
prop.table(table(db$DispMob))*100

table(db$Satisf_DispMob)

# Experiencia
table(db$Experiencia)
prop.table(table(db$Experiencia))*100



##################################################################################

### PERSONALIDAD

# COMUNICACION VERBAL
table(db$ComVrb)
tab.ComVrb<-as.data.frame(prop.table(table(db$ComVrb))*100)
tab.ComVrb

# ESTRES AL CONDUCIR
table(db$StrC)
tab.StrC<-as.data.frame(prop.table(table(db$StrC))*100)
tab.StrC

#COMUNICACION AFECTIVA
table(db$ComAfec)
tab.ComAfec<-as.data.frame(prop.table(table(db$ComAfec))*100)
tab.ComAfec

# PRESENTACION PERSONAL
table(db$PrPer)
tab.PrPer<-as.data.frame(prop.table(table(db$PrPer))*100)
tab.PrPer

# AMBIENTE LABORAL
table(db$AmbTr)
tab.AmbTr <-as.data.frame(prop.table(table(db$AmbTr))*100)
tab.AmbTr

# ANSIEDAD
table(db$Ans)
tab.Ans <-as.data.frame(prop.table(table(db$Ans))*100)
tab.Ans

# CONSIDERACION CON EL CLIENTE
table(db$ConCl)
tab.ConCl <-as.data.frame(prop.table(table(db$ConCl))*100)
tab.ConCl

#################################################################################

####  FORMA DE CONDUCIR

# UTILIZA EL CINTURON DE SEGURIDAD
table(db$CinSeg)
prop.table(table(db$CinSeg))*100

# DA PASO A PEATONES
table(db$PasoPeaton)
prop.table(table(db$PasoPeaton))*100

# USO FRECUENTE DEL PITO
table(db$UsoPito)
prop.table(table(db$UsoPito))*100

# FRENA Y ACELERA CON BRUSQUEDAD
table(db$FRbr)
prop.table(table(db$FRbr))*100

# HACE USO DE LAS DIRECCIONALES PARA REALIZAR GIROS
table(db$UsoDirec)
prop.table(table(db$UsoDirec))*100

# SE ENFADA CON OTROS CONDUCTORES
table(db$EnfCond)
prop.table(table(db$EnfCond))*100

# ACELERA O FRENA CON BRUSQUEDAD EN UN SEMAFORO
table(db$AFrSem)
prop.table(table(db$AFrSem))*100

# CAMBIO DE CARRIL CON FRECUENCIA 
table(db$CulFr)
prop.table(table(db$CulFr))*100

# OMITE LOS LIMITES DE VELOCIDAD
table(db$OmLmVel)
prop.table(table(db$OmLmVel))*100

# IGNORA LAS SEÑALES DE PARE O ALTO
table(db$IgPare)
prop.table(table(db$IgPare))*100

# HACE USO DEL CELULAR MIENTRAS CONDUCE
table(db$UsoCel)
prop.table(table(db$UsoCel))*100

# NUMERO DE VIAJES
table(db$MERIDIANO)
prop.table(table(db$MERIDIANO))*100

table(db$MERIDIANO, db$HPICOHVALLE)
prop.table(table(db$MERIDIANO, db$HPICOHVALLE),1)


# INCIDENTES DE TRANSITO

y = table(db$MERIDIANO, db$INCIDENTE)
prop.table(y,1)

z = table(db$CLIMA, db$INCIDENTE)
prop.table(z,1)

p = table(db$PAVIMENTO, db$INCIDENTE)
prop.table(p,1)


# NIVEL DE CONGESTION

table(db$MERIDIANO, db$CONGESTION)
prop.table(table(db$MERIDIANO, db$CONGESTION), 1)

table(db$HPICOHVALLE, db$CONGESTION)
prop.table(table(db$HPICOHVALLE, db$CONGESTION), 1)

#####################################################################################

# Horas de trabajo por Nivel educativo
p1 = ggplot(db, aes(x = NIVEL_EDUCATIVO, y = HORAS_TRABAJO)) + 
  geom_boxplot(alpha = 0.7, color = "darkblue") +
  xlab("Nivel Educativo") + ylab("Horas de trabajo al día")  +
  ggtitle("Horas de trabajo vs Nivel educativo")

# "Horas de trabajo por Experiencia"
p2 = ggplot(db, aes(x = Experiencia, y = HORAS_TRABAJO)) + 
  geom_violin(fill='orange', alpha=0.5)+
  geom_boxplot(color="white", fill="black",
               lwd=0.8, width=0.2 )+
  xlab("Experiencia") + ylab("Horas de trabajo al día") + labs(fill = "Rango de Edad") +
  ggtitle("Horas de trabajo vs Experiencia") 

# Edad por Horas de trabajo
p3 = ggplot(db, aes(x = EDAD, y = HORAS_TRABAJO)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.5, color = "blue") +
  xlab("Rango de edad") + ylab("Horas de trabajo al día") + labs(fill = "Rango de Edad") +
  ggtitle("Horas de trabajo vs Rango de edad")

grid.arrange(p1, p2, p3, ncol=3)


ggplot(db, aes(x = CONGESTION, y = Duracion)) + 
  geom_boxplot() +
  facet_wrap(~MERIDIANO)+
  xlab("Congestión") + ylab("Tiempos de viaje") + 
  ggtitle("Nivel de congestión vs Meridiano")

ggplot(db, aes(x = CONGESTION, y = Duracion)) + 
  geom_boxplot() +
  facet_wrap(~CLIMA)+
  xlab("Nivel de Congestión") + ylab("Tiempos de viaje") + 
  ggtitle("Nivel de congestión vs Clima")



ggplot(db, aes(x = HPICOHVALLE, y = Duracion)) + 
  geom_boxplot() +
  facet_wrap(~CLIMA)+
  xlab("Hora Pico / Hora Valle") + ylab("Tiempos de viaje") + 
  ggtitle("Clima vs Hora Pico/Hora valle")



ggplot(db, aes(x = INCIDENTE, y = Duracion)) + 
  geom_violin(alpha = 0.7, color = "red") +
  geom_jitter(alpha = 0.7, color = "blue")+
  facet_wrap(~MERIDIANO)+
  xlab("Incidentes") + ylab("Tiempos de viaje") + 
  ggtitle("Incidentes de tránsito vs Meridiano")


ggplot(db, aes(x = HPICOHVALLE, y = INCIDENTE)) + 
  geom_jitter(aes(color=interaction(factor(HPICOHVALLE), INCIDENTE)),
              alpha=0.5)+
  facet_wrap(~MERIDIANO)+
  # Nombramos al eje
  xlab("Hora pico/Hora valle") + ylab("Incidentes")+
  # Sacamos la leyenda
  theme(legend.position = "none")+
  # mejoramos los colores
  scale_color_brewer(palette = "Set2")+
  # Titulo
  ggtitle("Incidentes por Hora pico/Hora valle")




