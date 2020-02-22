
library(lavaan)
library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx)
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
library(semPlot)

# Cargar Datos desde MAC
DBModLog <- read.csv("/Users/williz/Desktop/ModelosED/Database/ModeloLogitVL.csv", header = TRUE, sep = "\t")

names(DBModLog)

#MCond <- DBModLog %>%
 # select(ViajeId:Experiencia) %>%
  # DB Modelo Logistico
  #inner_join(DBModoCond %>%
   #            select(ViajeId:DispMob),
    #         by = "ViajeId")

MCond <- DBModLog

names(MCond)

#MCond[c("ViajeId")]<- NULL

# ModoCond <- ModoCond[ ,!colnames(ModoCond)=="ViajeId"]

names(MCond)
# ORGANIZACION DE VARIABLES DUMMY

# EDAD JOVEN - ADULTO - MAYOR_60

MCond$JOVEN30 <- (ifelse((MCond$EDAD == 1), 1,0)) 
MCond$ADULTO40 <- (ifelse((MCond$EDAD == 2),1,0))
MCond$ADULTO60 <- (ifelse((MCond$EDAD == 3),1,0))
MCond$ADULTOMAYOR <- (ifelse((MCond$EDAD == 4),1,0))


# NIVEL EDUCATIVO EDUBASICA - EDUSUP
MCond$EDUBASICA <- (ifelse((MCond$NIVEL_EDUCATIVO == 1), 1,0))
MCond$EDUSUP <- (ifelse((MCond$NIVEL_EDUCATIVO == 2 | MCond$NIVEL_EDUCATIVO == 3),1,0))


# HORA DEL RECORRIDO HPICO - HVALLE
MCond$HPICO <- (ifelse((MCond$HPICOHVALLE == 1), 1,0))
MCond$HVALLE <- (ifelse((MCond$HPICOHVALLE == 2), 1,0))


# CONDICIONES CLIMATICAS CSECO - CLLUVIA
MCond$CSECO <- (ifelse((MCond$CLIMA == 1),1,0))
MCond$CLLUVIA <- (ifelse((MCond$CLIMA == 2),1,0))

# NIVEL DE CONGESTION
MCond$CONG_AB <- (ifelse((MCond$CONGESTION == 1 | MCond$CONGESTION == 2), 1,0))
MCond$CONG_CD <- (ifelse((MCond$CONGESTION == 3 | MCond$CONGESTION == 4), 1,0))
MCond$CONG_EF <- (ifelse((MCond$CONGESTION == 5 | MCond$CONGESTION == 6), 1,0))


# INFORMACION TRAFICO DE LA CIUDAD
MCond$SININFOTRF <- (ifelse((MCond$INFOTRAFICO == 1),1,0))
MCond$CONINFOTRF <- (ifelse((MCond$INFOTRAFICO == 2),1,0))

#Uso del cinturon
tapply(MCond$CinSeg,MCond[,c("JOVEN30","ADULTO40","ADULTO60","ADULTOMAYOR")])

MCond$USOCINTURON <-(ifelse((MCond$CinSeg == 2),1,0))
MCond$NOUSOCINTURON <-(ifelse((MCond$CinSeg == 1),1,0))

#Uso dispositivos
MCond$USODISPMOB <- (ifelse((MCond$DispMob != 1 ),1,0))
MCond$NOUSODISPMOB <- (ifelse((MCond$DispMob ==1),1,0))

# Ver la base de datos

head(MCond)


# ?Quiero orden!
tibble::as_tibble(MCond) 

names(MCond) 
view(MCond)

# Listado y propiedades de variables

dplyr::glimpse(MCond)   

summary(MCond)

write.table(MCond, 
            file="/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL.csv", sep="\t", dec=".")


# ESPECIFICACION DEL MODELO DE VARIABLES LATENTES.


# Nota: Se restringe todas las covarianzasde las variables latentes en el modelo
# CFA para que sean ortogonales con la opcion orthogonal = TRUE

# Si se desea corregir las variaciones de todas las variables latentes en un modelo
# CFA a la unidad, se utiliza el argumento std.lv = TRUE, pero las cargas factoriales
# del primer indicador de cada variable latente ya no se fijan en 1.

#sink("Resul_MOD_ModoCond.txt")

# MODELO 1

model1 <- " #Variables Latentes
      ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel
      VIORD =~ PasoPeaton + UsoDirec + IgPare
      IMPAC =~ UsoPito + UsoCel
      
      # Regresiones
      ACTAGR ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
              CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
      VIORD ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
              CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
      IMPAC ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
              CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB

      # Interceptos"
      
fit1 <- cfa(model1, data = MCond, orthogonal = TRUE, std.lv = TRUE)
summary(fit1, fit.measures=TRUE, standardized = TRUE)
#parameterEstimates(fit1, standardized = TRUE, ci = FALSE)
semPaths(fit1, what = "par", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit1)
modindices(fit1, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit11 <- cfa(model1, data = DBVL, orthogonal = TRUE)
summary(fit11, fit.measures= TRUE, standardized = TRUE)
semPaths(fit11, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit11, sort = TRUE, maximum.number = 10) 

sink()


# Indices de ajuste más comunes: CFI (>= .95), TLI (>= .95), RMSEA (<= .05) y SRMR (<= .06).

# Los indices de modificación son valores que nos brindarán una orientación acerca de la 
# re-especificación de la estructura factorial evaluada inicialmente.

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit1, sort = TRUE, maximum.number = 10)  

# Los Indices de modificacion representan la disminucion en el valor de chi-cuadrado 
# que se produciria en caso se realizar? lo sugerido.


sink("ResultadosMODELO2CE.txt")

# MODELO 2

model2 <- " #Variables Latentes
      ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + UsoPito
      VIORD =~ PasoPeaton + UsoDirec + IgPare
      IMPAC =~ UsoPito + UsoCel

# Regresiones
ACTAGR ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
VIORD ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
IMPAC ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB

# Interceptos

#Varianzas-Covarianzas
ACTAGR ~~ VIORD
"

fit2 <- cfa(model2, data = MCond, orthogonal = TRUE, std.lv = TRUE)
summary(fit2, fit.measures=TRUE, standardized = TRUE)
semPaths(fit2,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit21 <- cfa(model2, data = DBVL, orthogonal = TRUE)
summary(fit21, fit.measures= TRUE, standardized = TRUE)
semPaths(fit21, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit21, sort = TRUE, maximum.number = 10) 


#MODELO 3

model3 <- " #Variables Latentes
          ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + UsoPito
          VIORD =~ PasoPeaton + UsoDirec + IgPare + OmLmVel
          IMPAC =~ UsoPito + UsoCel

# Regresiones
ACTAGR ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
VIORD ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
IMPAC ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB

# Interceptos

#Varianzas-Covarianzas
ACTAGR ~~ VIORD
"

fit3 <- cfa(model3, data = MCond, orthogonal = TRUE, std.lv = TRUE)
summary(fit3, fit.measures=TRUE, standardized = TRUE)
semPaths(fit3,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit3, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit31 <- cfa(model3, data = DBVL, orthogonal = TRUE)
summary(fit31, fit.measures= TRUE, standardized = TRUE)
semPaths(fit31, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit31, sort = TRUE, maximum.number = 10) 


#MODELO 4

model4 <- " #Variables Latentes
          ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + UsoPito + UsoDirec
          VIORD =~ PasoPeaton + UsoDirec + IgPare + OmLmVel + UsoPito
          IMPAC =~ UsoPito + UsoCel + UsoDirec

# Regresiones
ACTAGR ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
VIORD ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
IMPAC ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB

# Interceptos

#Varianzas-Covarianzas
ACTAGR ~~ VIORD
"

fit4 <- cfa(model4, data = MCond, orthogonal = TRUE, std.lv = TRUE)
summary(fit4, fit.measures=TRUE, standardized = TRUE)
semPaths(fit4,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit41 <- cfa(model4, data = DBVL, orthogonal = TRUE)
summary(fit41, fit.measures= TRUE, standardized = TRUE)
semPaths(fit41, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit41, sort = TRUE, maximum.number = 10) 

sink()


sink("Resul_MODELO5.txt")

# MODELO 5

model5 <-  " #Variables Latentes
          ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + UsoPito + UsoDirec
          VIORD =~ PasoPeaton + UsoDirec + IgPare + OmLmVel + UsoPito
          IMPAC =~ UsoPito + UsoCel + UsoDirec + OmLmVel

# Regresiones
ACTAGR ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
VIORD ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
IMPAC ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB

# Interceptos

#Varianzas-Covarianzas
ACTAGR ~~ VIORD
UsoPito ~~ PasoPeaton
"

fit5 <- cfa(model5, data = MCond, orthogonal = TRUE, std.lv = TRUE)
summary(fit5, fit.measures=TRUE, standardized = TRUE)
semPaths(fit5,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit5, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit51 <- cfa(model5, data = DBVL, orthogonal = TRUE)
summary(fit51, fit.measures= TRUE, standardized = TRUE)
semPaths(fit51, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit51, sort = TRUE, maximum.number = 10) 

sink()

sink("Resul_MODELO6.txt")

# MODELO 6

model6 <- " #Variables Latentes
          ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + UsoPito + UsoDirec
          VIORD =~ PasoPeaton + UsoDirec + IgPare + OmLmVel + UsoPito
          IMPAC =~ UsoPito + UsoCel + UsoDirec + OmLmVel

# Regresiones
ACTAGR ~  ADULTO40 + CONG_CD + CONG_EF + SININFOTRF + USOCINTURON 
VIORD ~ EDUBASICA + CONG_EF + USOCINTURON 
IMPAC ~ EDUBASICA + CONG_CD + CONG_EF + USOCINTURON + USODISPMOB

# Interceptos

#Varianzas-Covarianzas
ACTAGR ~~ VIORD
UsoPito ~~ PasoPeaton
"

fit6 <- lavaan::cfa(model6, data = MCond, orthogonal = TRUE, std.lv = TRUE)
summary(fit6, fit.measures=TRUE, standardized = TRUE)
semPaths(fit6,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit6, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit61 <- cfa(model6, data = DBVL, orthogonal = TRUE)
summary(fit61, fit.measures= TRUE, standardized = TRUE)
semPaths(fit61, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit61, sort = TRUE, maximum.number = 10) 

sink()


sink("ResultadoMODELO7CE.txt")

# MODELO 7

model7 <- " #Variables Latentes
      ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + UsoPito
VIORD =~ PasoPeaton + UsoDirec + IgPare + OmLmVel
IMPAC =~ UsoPito + UsoCel + UsoDirec + OmLmVel + PasoPeaton

# Regresiones
ACTAGR ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
VIORD ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
IMPAC ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB

# Interceptos

#Varianzas-Covarianzas
ACTAGR ~~ VIORD
UsoPito ~~ PasoPeaton
"

fit7 <- lavaan::cfa(model7, data = MCond, orthogonal = TRUE, std.lv = TRUE)
summary(fit7, fit.measures=TRUE, standardized = TRUE)
semPaths(fit7,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit7, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit71 <- cfa(model7, data = DBVL, orthogonal = TRUE)
summary(fit71, fit.measures= TRUE, standardized = TRUE)
semPaths(fit71, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit71, sort = TRUE, maximum.number = 10) 

sink()

sink("ComparacionModelos.txt")

# TABLA COMPARATIVA DE TODOS LOS MODELOS

fit_index01 <- broom::glance(fit1) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index02 <- broom::glance(fit2) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index03 <- broom::glance(fit3) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index04 <- broom::glance(fit4) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index05 <- broom::glance(fit5) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index06 <- broom::glance(fit6) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index07 <- broom::glance(fit7) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

# Uniendo 
bind_rows(fit_index01, fit_index02, fit_index03, fit_index04, fit_index05, fit_index06, 
          fit_index07, .id = "Modelo")

sink()




sink("ResulMODELO_8.txt")

# MODELO 8

model8 <-  " #Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC + ConsCl + UsDirec + CulFr
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec
HABPROSOC =~ ComVrb + ComAfec

# Regresiones
ACTAGR ~ EDUBASICA + HORASTRABAJO + CONG_CD + CONG_EF + SININFOTRF

STRESS ~ JOVEN + HORASTRABAJO + HPICO  + SININFOTRF

AMBLABOR ~  ADULTO + CONG_CD + ACTAGR + STRESS

CONDSEG ~ EDUBASICA  + STRESS

HABPROSOC ~ JOVEN + ADULTO + HORASTRABAJO  + CONG_CD + CONG_EF + SININFOTRF  

# Varianzas - Covarianzas
ACTAGR ~~ STRESS
FRbr ~~ AFrSem

# Interceptos"

fit8 <- cfa(model8, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit8, fit.measures=TRUE, standardized = TRUE)
semPaths(fit8,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit8, sort = TRUE, maximum.number = 10) 

fit81 <- cfa(model8, data = DBVL, orthogonal = TRUE)
summary(fit81, fit.measures= TRUE, standardized = TRUE)
semPaths(fit81, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit81, sort = TRUE, maximum.number = 10) 

sink()


sink("ResulMODELO_9.txt")

# MODELO 9

model9 <- " #Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC + ConsCl + UsDirec + CulFr
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec
HABPROSOC =~ ComVrb + ComAfec + ConsCl

# Regresiones
ACTAGR ~ EDUBASICA + HORASTRABAJO + CONG_CD + CONG_EF + SININFOTRF

STRESS ~ JOVEN + HORASTRABAJO + HPICO  + SININFOTRF

AMBLABOR ~  ADULTO + CONG_CD + ACTAGR + STRESS

CONDSEG ~ EDUBASICA  + STRESS

HABPROSOC ~ JOVEN + ADULTO + HORASTRABAJO  + CONG_CD + CONG_EF + SININFOTRF  

# Varianzas - Covarianzas
ACTAGR ~~ STRESS
FRbr ~~ AFrSem

# Interceptos"

fit9 <- cfa(model9, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit9, fit.measures=TRUE, standardized = TRUE)
semPaths(fit9,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit9, sort = TRUE, maximum.number = 10) 


fit91 <- cfa(model9, data = DBVL, orthogonal = TRUE)
summary(fit91, fit.measures= TRUE, standardized = TRUE)
semPaths(fit91, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit91, sort = TRUE, maximum.number = 10) 

sink()





sink("ResulMODELO_10.txt")

# MODELO 10

model10 <- " #Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC + ConsCl + UsDirec + CulFr
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec + Ans
HABPROSOC =~ ComVrb + ComAfec + ConsCl

# Regresiones
ACTAGR ~ EDUBASICA + HORASTRABAJO + CONG_CD + CONG_EF + SININFOTRF

STRESS ~ JOVEN + HORASTRABAJO + HPICO  + SININFOTRF

AMBLABOR ~  ADULTO + CONG_CD + ACTAGR + STRESS

CONDSEG ~ EDUBASICA  + STRESS

HABPROSOC ~ JOVEN + ADULTO + HORASTRABAJO  + CONG_CD + CONG_EF + SININFOTRF  

# Varianzas - Covarianzas
ACTAGR ~~ STRESS
FRbr ~~ AFrSem

# Interceptos"

fit10 <- cfa(model10, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit10, fit.measures=TRUE, standardized = TRUE)
semPaths(fit10,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit10, sort = TRUE, maximum.number = 10) 


fit10_1 <- cfa(model10, data = DBVL, orthogonal = TRUE)
summary(fit10_1, fit.measures= TRUE, standardized = TRUE)
semPaths(fit10_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit10_1, sort = TRUE, maximum.number = 10) 

sink()




sink("ResulMODELO_11.txt")

# MODELO 11

model11 <- " #Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC + ConsCl + UsDirec + CulFr
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec + Ans
HABPROSOC =~ ComVrb + ComAfec + ConsCl

# Regresiones
ACTAGR ~ EDUBASICA + HORASTRABAJO + CONG_CD + CONG_EF + SININFOTRF

STRESS ~ JOVEN + HORASTRABAJO + HPICO  + SININFOTRF

AMBLABOR ~  ADULTO + CONG_CD + ACTAGR + STRESS

CONDSEG ~ EDUBASICA  + STRESS

HABPROSOC ~ JOVEN + ADULTO + HORASTRABAJO  + CONG_CD + CONG_EF + SININFOTRF  

# Varianzas - Covarianzas
ACTAGR ~~ STRESS
FRbr ~~ AFrSem
CulFr ~~ ComVrb

# Interceptos"
fit11 <- lavaan::cfa(model11, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit11, fit.measures=TRUE, standardized = TRUE)
semPaths(fit11,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit11, sort = TRUE, maximum.number = 10) 

fit11_1 <- cfa(model11, data = DBVL, orthogonal = TRUE)
summary(fit11_1, fit.measures= TRUE, standardized = TRUE)
semPaths(fit11_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit11_1, sort = TRUE, maximum.number = 10) 

sink()



sink("ResulMODELO_12.txt")

# MODELO 12

model12 <- " #Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare
STRESS =~ Ans + ComAfec + StrC + ConsCl + UsDirec + CulFr
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec + Ans
HABPROSOC =~ ComVrb + ComAfec + ConsCl

# Regresiones
ACTAGR ~ EDUBASICA + HORASTRABAJO + CONG_CD + CONG_EF + SININFOTRF

STRESS ~ JOVEN + HORASTRABAJO + HPICO  + SININFOTRF

AMBLABOR ~  ADULTO + CONG_CD + ACTAGR + STRESS

CONDSEG ~ EDUBASICA  + STRESS

HABPROSOC ~ JOVEN + ADULTO + HORASTRABAJO  + CONG_CD + CONG_EF + SININFOTRF + AMBLABOR 

# Varianzas - Covarianzas
ACTAGR ~~ STRESS
FRbr ~~ AFrSem
CulFr ~~ ComVrb
Ans ~~ ComAfec

# Interceptos"

fit12 <- lavaan::cfa(model12, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit12, fit.measures=TRUE, standardized = TRUE)
semPaths(fit12,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit12, sort = TRUE, maximum.number = 10) 

fit12_1 <- cfa(model12, data = DBVL, orthogonal = TRUE)
summary(fit12_1, fit.measures= TRUE, standardized = TRUE)
semPaths(fit12_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit12_1, sort = TRUE, maximum.number = 10) 

sink()




# MODELO 13
model13 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + ComVrb
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec + StrC
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR ~ EDUBASICA  + TIEMPO_PROFESION + HORASTRABAJO
STRESS ~ JOVEN + ADULTO + TIEMPO_PROFESION + HORASTRABAJO
AMBLABOR ~ EDUBASICA + ADULTO + HORASTRABAJO
CONDSEG ~ EDUBASICA + JOVEN + ADULTO + TIEMPO_PROFESION 
HABPROSOC ~ EDUBASICA + JOVEN + ADULTO + HORASTRABAJO 

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS
CulFr ~~ ComVrb
CulFr ~~ Ans
FRbr ~~ AFrSem
AMBLABOR ~~ HABPROSOC

# Interceptos"

fit13 <- lavaan::cfa(model13, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit13, fit.measures=TRUE, standardized = TRUE)
semPaths(fit13,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit13, sort = TRUE, maximum.number = 10) 


# MODELO 14

model14 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + ComVrb
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec + StrC
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR ~ EDUBASICA + HORASTRABAJO
STRESS ~ JOVEN + ADULTO + HORASTRABAJO
AMBLABOR ~ EDUBASICA + ADULTO 
CONDSEG ~ EDUBASICA + JOVEN  + TIEMPO_PROFESION 
HABPROSOC ~  JOVEN + ADULTO + HORASTRABAJO 

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS
CulFr ~~ ComVrb
CulFr ~~ Ans
FRbr ~~ AFrSem
AMBLABOR ~~ HABPROSOC

# Interceptos"

fit14 <- lavaan::cfa(model14, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit14, fit.measures=TRUE, standardized = TRUE)
semPaths(fit14,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit14, sort = TRUE, maximum.number = 10) 


#MODELO 15

model15 <- "#Variables Latentes
ACTAGR =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + ComVrb
STRESS =~ Ans + ComAfec + StrC + ConsCl
AMBLABOR =~ PrPers + AmbT + ConsCl
CONDSEG =~  CinSeg + UsDirec + StrC
HABPROSOC =~ ComVrb + ComAfec


# Regresiones
ACTAGR ~  HORASTRABAJO
STRESS ~ JOVEN + HORASTRABAJO
AMBLABOR ~  ADULTO 
CONDSEG ~ EDUBASICA  + TIEMPO_PROFESION 
HABPROSOC ~  ADULTO + HORASTRABAJO 

AMBLABOR ~ ACTAGR
CONDSEG ~ STRESS

#Varianzas - Covarianzas
ACTAGR ~~ STRESS
CulFr ~~ ComVrb
CulFr ~~ Ans
FRbr ~~ AFrSem
AMBLABOR ~~ HABPROSOC

# Interceptos"

fit15 <- lavaan::cfa(model15, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit15, fit.measures=TRUE, standardized = TRUE)
semPaths(fit15,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit15, sort = TRUE, maximum.number = 10)


# TABLA COMPARATIVA DE TODOS LOS MODELOS

fit_index <- broom::glance(fit1) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index02 <- broom::glance(fit2) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index03 <- broom::glance(fit3) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index04 <- broom::glance(fit4) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index05 <- broom::glance(fit5) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index06 <- broom::glance(fit6) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index07 <- broom::glance(fit7) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index08 <- broom::glance(fit8) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index09 <- broom::glance(fit9) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index10 <- broom::glance(fit10) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index11 <- broom::glance(fit11) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index12 <- broom::glance(fit12) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index13 <- broom::glance(fit13) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)


# Uniendo 
bind_rows(fit_index, fit_index02, fit_index03, fit_index04, fit_index05, fit_index06, 
          fit_index07, fit_index08, fit_index09, fit_index10, fit_index11,
          fit_index12, fit_index13, fit_index14, fit_index15, .id = "Modelo")



#CREACION DE LAS VARIABLES LATENTES EN LA DB

ACTAGR <- 0.372*DBVL$FRbr+0.260*DBVL$EnfCond+0.422*DBVL$AFrSem+0.278*DBVL$CulFr+0.254*DBVL$OmLmVel
          +0.233*DBVL$IgPare-0.816*DBVL$ComVrb

STRESS <- 1.283*DBVL$Ans+1.238*DBVL$ComAfec+1.012*DBVL$StrC+0.596*DBVL$ConsCl

AMBLABOR  <- DBVL$PrPers + DBVL$AmbT + DBVL$ConsCl
CONDSEG   <- DBVL$CinSeg + DBVL$UsDirec + DBVL$StrC
HABPROSOC <- DBVL$ComVrb + DBVL$ComAfec


DBVL

ACTAGR

DB<- cbind(DBVL,ACTAGR,STRESS)

DB

getwd() 

#sink("Resultados.txt")