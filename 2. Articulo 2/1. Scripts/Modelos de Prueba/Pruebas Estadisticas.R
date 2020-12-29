
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
library(umx)

rm(list = ls())

#workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/Modelos de Prueba/ICVL_MCond_3F/Version2"
#setwd(workingDirectory)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)
DBC = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBCompleta_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

#database <- database %>%
  # Filtrar viajes problematicos
#  filter(!(ViajeId %in% c("{0D5CB44F-00E5-E811-8FB7-74867AD5B714}")))

  
  # Normalización de las variables tiempo
for (i in 1:nrow(database)){ 
  database$T_Alt_1[i] = (database$TIEMPOAlt1[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_2[i] = (database$TIEMPOAlt2[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_3[i] = (database$TIEMPOAlt3[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_4[i] = (database$TIEMPOEC[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  
  # Normalización de la variable distancia
  database$D_Alt_1[i] = (database$DISTAlt1[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))
  database$D_Alt_2[i] = (database$DISTAlt2[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))
  database$D_Alt_3[i] = (database$DISTAlt3[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))
  database$D_Alt_4[i] = (database$DISTEC[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))

  database$Vel_Alt_1[i] = database$D_Alt_1[i]/database$T_Alt_1[i]
  
  if(database$HPICOHVALLE[i] == 1){
    database$Vel_fl_Alt1[i] = 28/60;
    database$Vel_fl_Alt2[i] = 28/60;
    database$Vel_fl_Alt3[i] = 28/60;
    database$Vel_fl_Alt4[i] = 28/60}
  else{database$Vel_fl_Alt1[i] = 24/60;
  database$Vel_fl_Alt2[i] = 24/60;
  database$Vel_fl_Alt3[i]= 24/60; 
  database$Vel_fl_Alt4[i]= 24/60}
  
  database$Vel_

  database$T_fl_Alt1[i] = database$DISTAlt1[i]/database$Vel_fl_Alt1[i]
  database$T_fl_Alt2[i] = database$DISTAlt2[i]/database$Vel_fl_Alt2[i]
  database$T_fl_Alt3[i] = database$DISTAlt3[i]/database$Vel_fl_Alt3[i]
  database$T_fl_Alt4[i] = database$DISTEC[i]/database$Vel_fl_Alt4[i]
  
  database$CG_Alt_1[i] = (database$TIEMPOAlt1[i]/database$T_fl_Alt1[i])-1
  database$CG_Alt_2[i] = (database$TIEMPOAlt2[i]/database$T_fl_Alt2[i])-1
  database$CG_Alt_3[i] = (database$TIEMPOAlt3[i]/database$T_fl_Alt3[i])-1
  database$CG_Alt_4[i] = (database$TIEMPOEC[i]/database$T_fl_Alt4[i])-1
}


database$MERIDIANO <- factor(database$MERIDIANO,
                          levels = c('1','2') ,
                          labels = c("AM","PM"))

database$HPICOHVALLE <- factor(database$HPICOHVALLE,
                            levels = c('1','2'),
                            labels = c("Valle","Pico"))

Flibre_grp <- database %>%
  group_by(HPICOHVALLE) %>%
  summarize(mean_Vel1=mean(DISTAlt1*60/TIEMPOAlt1),
            min_Vel1 = min(DISTAlt1*60/TIEMPOAlt1),
            quan_Vel1=quantile(DISTAlt1*60/TIEMPOAlt1, probs = 0.8),
            max_Vel1 = max(DISTAlt1*60/TIEMPOAlt1),
            mean_Vel2=mean(DISTAlt2*60/TIEMPOAlt2),
            min_Vel2 = min(DISTAlt2*60/TIEMPOAlt2),
            quan_Vel2=quantile(DISTAlt2*60/TIEMPOAlt2, probs = 0.8),
            max_Vel2 = max(DISTAlt2*60/TIEMPOAlt2),
            mean_Vel3= mean(DISTAlt3*60/TIEMPOAlt3),
            min_Vel3 = min(DISTAlt3*60/TIEMPOAlt3),
            quan_Vel3=quantile(DISTAlt3*60/TIEMPOAlt3, probs = 0.8),
            max_Vel3 = max(DISTAlt3*60/TIEMPOAlt3),
            mean_Vel4=mean(DISTEC*60/TIEMPOEC),
            min_Vel4 = min(DISTEC*60/TIEMPOEC),
            quan_Vel4=quantile(DISTEC*60/TIEMPOEC, probs = 0.8),
            max_Vel4 = max(DISTEC*60/TIEMPOEC))
Flibre_grp
#hist(database$DISTAlt1*60/database$TIEMPOAlt1)

table(DBC$EDAD)

Tiempos <- data.frame(database$CG_Alt_1 , database$CG_Alt_2))
 
ggplot(DBC, aes( x = TIEMPO_PROFESION)) + 
  geom_histogram() +
  xlab("Tiempo de Profesión") + 
  ylab("Años") + ggtitle("Distribución Tiempo de profesión") 

 
Tiempos
database$CG_Alt_1
ggplot(Tiempos) + geom_boxplot(aes(y=database$CG_Alt_1)) + coord_flip()


hist(database$CG_Alt_1)
Tiempo


ggplot(database, mapping = aes(x=CG_Alt_1)) + 
  geom_density(aes(x=c(CG_Alt_1)),bins=10, position = "stack",alpha = 0.5)
  

ggplot(database, mapping = aes(x=CG_Alt_1)) + 
  geom_density(aes(x=T_Alt_1, fill=factor(HPICOHVALLE)),bins=10, position = "stack",alpha = 0.5)



ggplot(database) + geom_boxplot(aes( x=c("CG_Alt_1", "CG_Alt_2"))) + coord_flip()

hist(database$CG_Alt_2)
hist(database$CG_Alt_3)
hist(database$CG_Alt_4)



plot(database$CG_Alt_4)
boxplot(database$CG_Alt_3~ database$HPICOHVALLE, horizontal = TRUE)
