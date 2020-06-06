
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
library(MVN)
library(mvtnorm)

# Cargar Datos desde MAC
DBModLog <- read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv", header = TRUE, sep = "\t")


MCond <- DBModLog
names(MCond)

MCond[c("ViajeId")]<-NULL

# ESPECIFICACION DEL MODELO DE VARIABLES LATENTES.


# Nota: Se restringe todas las covarianzas de las variables latentes en el modelo
# CFA para que sean ortogonales con la opcion orthogonal = TRUE

# Si se desea corregir las variaciones de todas las variables latentes en un modelo
# CFA a la unidad, se utiliza el argumento std.lv = TRUE, pero las cargas factoriales
# del primer indicador de cada variable latente ya no se fijan en 1.

#sink("Resul_MOD_ModoCond.txt")

# MODELO 1

model1 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr 
      FA_2 =~ OmLmVel + IgPare  
      FA_3 =~ PasoPeaton + UsoDirec + UsoCel
      
      # Regresiones
      FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
              CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
      FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
              CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB
      FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + TIEMPO_PROFESION + HORAS_TRABAJO + HPICO + CSECO +
              CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB

      # Interceptos"
      
fit1 <- cfa(model1, data = MCond, orthogonal = TRUE, std.lv = TRUE)
summary(fit1, fit.measures=TRUE, standardized = TRUE)
#parameterEstimates(fit1, standardized = TRUE, ci = FALSE)
semPaths(fit1, what = "par", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit1)
modindices(fit1, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit11 <- cfa(model1, data = MCond, orthogonal = TRUE)
summary(fit11, fit.measures= TRUE, standardized = TRUE)
semPaths(fit11, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit11, sort = TRUE, maximum.number = 10) 

sink()



model11 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr 
      FA_2 =~ OmLmVel + IgPare  
      FA_3 =~ PasoPeaton + UsoDirec + UsoCel

# Regresiones
FA_1 ~ EDUBASICA  + HORAS_TRABAJO  + CSECO  + CONG_EF + SININFOTRF + USOCINTURON
FA_2 ~ HORAS_TRABAJO + HPICO + CONG_CD + CONG_EF + USOCINTURON + USODISPMOB
FA_3 ~ EDUBASICA  + CONG_CD + CONG_EF  + USOCINTURON + USODISPMOB

# Interceptos"


fit11 <- cfa(model11, data = MCond, orthogonal = TRUE, std.lv = TRUE)
summary(fit11, fit.measures=TRUE, standardized = TRUE)
#parameterEstimates(fit1, standardized = TRUE, ci = FALSE)
semPaths(fit11, what = "par", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit1)
modindices(fit11, sort = TRUE, maximum.number = 10) 


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
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr 
      FA_2 =~ OmLmVel + IgPare  
      FA_3 =~ PasoPeaton + UsoDirec+ UsoCel

# Regresiones
FA_1 ~ EDUBASICA  + HORAS_TRABAJO  + CSECO  + CONG_EF + SININFOTRF + USOCINTURON
FA_2 ~ HORAS_TRABAJO + HPICO + CONG_CD + CONG_EF + USOCINTURON + USODISPMOB
FA_3 ~ EDUBASICA  + CONG_CD + CONG_EF  + USOCINTURON + USODISPMOB
FA_2 ~ FA_1
# Interceptos"

fit2 <- cfa(model2, data = MCond, orthogonal = TRUE, std.lv = TRUE)
summary(fit2, fit.measures=TRUE, standardized = TRUE)
semPaths(fit2,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit21 <- cfa(model2, data = MCond, orthogonal = TRUE)
summary(fit21, fit.measures= TRUE, standardized = TRUE)
semPaths(fit21, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit21, sort = TRUE, maximum.number = 10) 

sink("Modelo3.txt")

#MODELO 3

model3 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr 
      FA_2 =~ OmLmVel + IgPare + UsoCel
      FA_3 =~ PasoPeaton + UsoDirec+ UsoCel

# Regresiones
FA_1 ~ EDUBASICA  + HORAS_TRABAJO  + CSECO  + CONG_EF + SININFOTRF + USOCINTURON
FA_2 ~ HORAS_TRABAJO + HPICO + CONG_CD + CONG_EF + USOCINTURON + USODISPMOB
FA_3 ~ EDUBASICA  + CONG_CD + CONG_EF  + USOCINTURON + USODISPMOB
FA_2 ~ FA_1
# Interceptos"

fit3 <- cfa(model3, data = MCond, orthogonal = TRUE, std.lv = TRUE)
summary(fit3, fit.measures=TRUE, standardized = TRUE)
semPaths(fit3,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit3, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit31 <- cfa(model3, data = MCond, orthogonal = TRUE)
summary(fit31, fit.measures= TRUE, standardized = TRUE)
semPaths(fit31, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit31, sort = TRUE, maximum.number = 10) 


model4 <- " #Variables Latentes
      FA_1 =~ FRbr + EnfCond + AFrSem + CulFr 
      FA_2 =~ OmLmVel + IgPare + UsoCel
      FA_3 =~ PasoPeaton + UsoDirec+ UsoCel

# Regresiones
FA_1 ~ EDUBASICA  + HORAS_TRABAJO  + CSECO  + CONG_EF + SININFOTRF + USOCINTURON
FA_2 ~ HPICO + CONG_CD  + USOCINTURON + USODISPMOB
FA_3 ~ EDUBASICA  + CONG_CD + CONG_EF  + USOCINTURON + USODISPMOB
FA_2 ~ FA_1
# Interceptos"

fit4 <- cfa(model4, data = MCond, orthogonal = TRUE, std.lv = TRUE)
summary(fit4, fit.measures=TRUE, standardized = TRUE)
semPaths(fit4,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4, sort = TRUE, maximum.number = 10) 

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit41 <- cfa(model4, data = MCond, orthogonal = TRUE)
summary(fit41, fit.measures= TRUE, standardized = TRUE)
semPaths(fit41, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit41, sort = TRUE, maximum.number = 10) 


sink()

sink("ComparacionModelos.txt")

# TABLA COMPARATIVA DE TODOS LOS MODELOS

fit_index01 <- broom::glance(fit1) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index01 <- broom::glance(fit11) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index02 <- broom::glance(fit2) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index03 <- broom::glance(fit3) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

# Uniendo 
bind_rows(fit_index01, fit_index02, fit_index03, .id = "Modelo")

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

fit_index08 <- broom::glance(fit8) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index09 <- broom::glance(fit9) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index10 <- broom::glance(fit10) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)


# Uniendo 
bind_rows(fit_index01, fit_index02, fit_index03, fit_index04, fit_index05, fit_index06, 
          fit_index07, fit_index08,fit_index09,fit_index10, .id = "Modelo")

sink()


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