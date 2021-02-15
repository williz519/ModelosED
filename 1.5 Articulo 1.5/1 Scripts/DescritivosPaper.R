
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


workingDirectory="/Users/williz/Desktop/ModelosED/1.5 Articulo 1.5/3 Resultados"
setwd(workingDirectory)

# Cargar datos
Datos   <- readRDS("/Users/williz/Desktop/ModelosED/1.5 Articulo 1.5/2 Database/DBCOMPLETA.rds")
Rutas   <- readRDS("/Users/williz/Desktop/ModelosED/1.5 Articulo 1.5/2 Database/Rutas.rds")

db   <- Datos %>%
  select(ViajeId:USOCINTURON) %>%
  # Rutas 
  inner_join(Rutas %>%
               select(ViajeId, Duracion, Distancia, V_promedio),
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

#######################################################################################

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
table(db$HORAS_TRABAJO)

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

#Congestión
prop.table(table(db$CONGESTION))*100

# Pavimento
prop.table(table(db$PAVIMENTO))*100

# Incidente
prop.table(table(db$INCIDENTE))*100

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





# Regresión lineal Costo ~ duracion + distancia

plot1 = ggplot(db, aes(x = COSTO, y = Duracion)) + 
  geom_point(alpha = 0.7, color = "blue")+
  xlab("Costo carrera") + ylab("Tiempo de viaje")+
  ggtitle("Costo vs tiempo de viaje")

plot2 = ggplot(db, aes(x = COSTO, y = Distancia)) + 
  geom_point(alpha = 0.7, color = "red")+
  xlab("Costo carrera") + ylab("Distancia de viaje")+
  ggtitle("Costo vs distancia de viaje")

grid.arrange(plot1, plot2, ncol=2)

modelo <- lm(Costo ~ Duracion + Distancia, data = Rutas )
summary(modelo)

## SEMAFOROS EN EL VIAJE

db$Semaforos <-  (ifelse((db$CHOICE == 1), (db$Semaf_A1/db$DISTAlt1),
                         ifelse((db$CHOICE == 2), (db$Semaf_A2/db$DISTAlt2),
                                ifelse((db$CHOICE == 3), (db$Semaf_A3/db$DISTAlt3),
                                       (db$Semaf_EC/db$DISTEC)))))

db$Semaforos

## Camaras de Fotomultas

db$Camaras <- (ifelse((db$CHOICE == 1), db$CamFD_A1,
                      ifelse((db$CHOICE == 2), db$CamFD_A2,
                             ifelse((db$CHOICE == 3), db$CamFD_A3,
                                    db$CamFD_EC))))

db$Camaras

### Velocidad

db$Velocidad <- (ifelse((db$V_promedio<19), 1,
                        ifelse((db$V_promedio < 26), 2,3)))

db$Velocidad <- factor(db$Velocidad,
                       levels = 1:3,
                       labels = c("Baja","Media","Alta"))


db$Velocidad


table(db$Experiencia)

# REGRESION LOGISTICA politomica ordinal

CONGESTION <- relevel(db$CONGESTION, ref = "Nivel F")
CLIMA <- relevel(db$CLIMA, ref = "Despejado")
Experiencia <- relevel(db$Experiencia, ref = "Menos 2 años")

Velocidad <- ordered(db$Velocidad, levels = c("Baja","Media","Alta"))

data <- data.frame(Vel=Velocidad, Cong = CONGESTION, Clima = CLIMA, Sem = db$Semaforos, 
                   HLab = db$HORAS_TRABAJO, HpicoHvalle=db$HPICOHVALLE, Exp=Experiencia, Merid = db$MERIDIANO)

## Ajustamos el modelo

modelo <- polr(Vel ~ Cong + Clima + Sem  + HpicoHvalle + Merid + Exp, data = data)

summary(modelo)

# Probabilidades estimadas
Prob <- unique(data.frame(CONGESTION, CLIMA, db$Semaforos, db$HPICOHVALLE, db$MERIDIANO, Experiencia, fitted.values(modelo)))

Tabla <- data.frame(db$Velocidad, Prob$Baja, Prob$Media, Prob$Alta)


modelo_glm <- glm(db$Velocidad ~ db$CONGESTION + db$CLIMA + db$Semaforos + 
                    db$HORAS_TRABAJO + db$HPICOHVALLE + db$MERIDIANO + db$Experiencia
                    , family = gaussian(), data = db)
summary(modelo_glm)

confint(modelo_glm)

# En caso de querer los intervalos basados en el error estándar.
confint.default(modelo_glm)

# El mismo cálculo se puede obtener directamente con:
anova(modelo_glm, test = "Chisq")

# Diferencia de residuos
dif_residuos <- modelo_glm$null.deviance - modelo_glm$deviance


# Grados libertad
df <- modelo_glm$df.null - modelo_glm$df.residual
# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

paste("Diferencia de residuos:", round(dif_residuos, 4))

paste("Grados de libertad:", df)

paste("p-value:", round(p_value, 4))
modelo_glm$fitted.values
modelo_glm$model$db$Velocidad

predicciones <- ifelse(test = modelo_glm$fitted.values > 0.5, yes = 1, no = 0)
predicciones
matriz_confusion <- table(modelo_glm$model$db$Velocidad, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion
