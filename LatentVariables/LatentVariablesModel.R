
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
DBVL <- readRDS("/Users/williz/Dropbox/Doctorado/Resultados Tesis/BasesDatos/DBModeloLogitVL.rds")
names(DBVL)


#Cargar datos desde PC UdeA
#DB <- readRDS("/Users/sin definir/Dropbox/Doctorado/Resultados Tesis/BasesDatos/DBModeloLogitVL.rds")

names(DBVL)

DBVL$EDAD

DBVL$MENOR_60 <- (ifelse((DBVL$EDAD <= 3), 1,0)) 

DBVL$MENOR_60

DBVL$MAYOR_60 <- (ifelse((DBVL$EDAD == 4),1,0))

DBVL$MAYOR_60

DBVL$EDUBASICA <- (ifelse((DBVL$NIVELEDUCATIVO == 1 | DBVL$NIVELEDUCATIVO == 4), 1,0))

DBVL$EDUSUP <- (ifelse((DBVL$NIVELEDUCATIVO == 2 | DBVL$NIVELEDUCATIVO == 3),1,0))

DBVL

# Ver la base de datos

head(DBVL)

# ?Quiero orden!
tibble::as_tibble(DBVL) 

names(DBVL) 

# Listado y propiedades de variables

dplyr::glimpse(DBVL)   

summary(DBVL)


write.table(DBVL, 
        file="/Users/williz/Dropbox/Doctorado/Resultados Tesis/BasesDatos/DBModeloLogitVL1.csv", sep="\t", dec=".")

write.csv2(DBVL, 
           file="/Users/williz/Dropbox/Doctorado/Resultados Tesis/BasesDatos/DBModeloLogitVL1.csv", sep="\t", dec=".")



# Especificacion del Modelo

model1 <- " #Variables Latentes
      ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
                 + IgnoraSenhalPare + OmiteLimiteVelocidad
      STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente
      AMBLABORAL =~ PresPersonal + AmbTrabajo 
      SEGURIDAD =~  CinturonDeSeguridad + UsaDireccionales
      HABPROSOC =~ ComunicVerbal 
      
      # Regresiones
      ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
           MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

      # Interceptos"
      

fit1 <- lavaan::cfa(model1, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit1, fit.measures=TRUE, standardized = TRUE)
parameterEstimates(fit1, standardized = TRUE, ci = FALSE)
semPaths(fit1,"std", title = FALSE, curvePivot = TRUE)

# Indices de ajuste más comunes: CFI (>= .95), TLI (>= .95), RMSEA (<= .05) y SRMR (<= .06).

# Los indices de modificación son valores que nos brindarán una orientación acerca de la 
# re-especificación de la estructura factorial evaluada inicialmente.

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit1, sort = TRUE, maximum.number = 10)  


# Los ?ndices de modificaci?n representan la disminuci?n en el valor de chi-cuadrado 
# que se producir?a en caso se realizar? lo sugerido.

model2 <- " #Variables Latentes
      ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
                + IgnoraSenhalPare + OmiteLimiteVelocidad
      STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente
      AMBLABORAL =~ PresPersonal + AmbTrabajo 
      SEGURIDAD =~  CinturonDeSeguridad + UsaDireccionales
      HABPROSOC =~ ComunicVerbal 

      # Regresiones
      ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
      MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

      #Varianza Covarianzas
      ACTAGRE ~~ STRESS
      
      # Interceptos"

fit2 <- lavaan::cfa(model2, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit2, fit.measures=TRUE, standardized = TRUE)
semPaths(fit2,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2, sort = TRUE, maximum.number = 10) 



model3 <- " #Variables Latentes
      ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
                + IgnoraSenhalPare + OmiteLimiteVelocidad + UsaDireccionales
      STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente 
      AMBLABORAL =~ PresPersonal + AmbTrabajo 
      SEGURIDAD =~  CinturonDeSeguridad 
      HABPROSOC =~ ComunicVerbal 

      # Regresiones
      ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
      MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

     #Varianza Covarianzas
      ACTAGRE ~~ STRESS
      

      # Interceptos"

fit3 <- lavaan::cfa(model3, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit3, fit.measures=TRUE, standardized = TRUE)
semPaths(fit3,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit3, sort = TRUE, maximum.number = 10) 


fit_index <- broom::glance(fit1) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index02 <- broom::glance(fit2) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index03 <- broom::glance(fit3) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

# Uniendo 
bind_rows(fit_index, fit_index02, fit_index03, .id = "Modelo")



#MODELO 4
model4 <- " #Variables Latentes
      ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
                + IgnoraSenhalPare + OmiteLimiteVelocidad
      STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente
      AMBLABORAL =~ PresPersonal + AmbTrabajo 
      SEGURIDAD =~  CinturonDeSeguridad + UsaDireccionales
      HABPROSOC =~ ComunicVerbal 

      # Regresiones
      ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
      MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

      AMBLABORAL ~ STRESS
      SEGURIDAD ~ STRESS

      #Varianza Covarianzas
      ACTAGRE ~~ STRESS

      # Interceptos"

fit4 <- lavaan::cfa(model4, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit4, fit.measures=TRUE, standardized = TRUE)
semPaths(fit4,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit4, sort = TRUE, maximum.number = 10) 

# MODELO 5
model5 <- " #Variables Latentes
      ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
                + IgnoraSenhalPare + OmiteLimiteVelocidad
      STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente
      AMBLABORAL =~ PresPersonal + AmbTrabajo 
      SEGURIDAD =~  CinturonDeSeguridad + UsaDireccionales
      HABPROSOC =~ ComunicVerbal + ComunicAfectiva

      # Regresiones
      ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
      MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

      AMBLABORAL ~ STRESS
      SEGURIDAD ~ STRESS

      #Varianza Covarianzas
      ACTAGRE ~~ STRESS
      CulebreaConFrecuencia ~~ Ansiedad

      # Interceptos"

fit5 <- lavaan::cfa(model5, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit5, fit.measures=TRUE, standardized = TRUE)
semPaths(fit5,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit5, sort = TRUE, maximum.number = 10) 




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

# Uniendo 
bind_rows(fit_index, fit_index02, fit_index03, fit_index04, fit_index05, .id = "Modelo")

# ?ndices de ajuste m?s comunes: CFI (>= .95), TLI (>= .95), RMSEA (<= .05) y SRMR (<= .06).

# MODELO 6
model6 <- " #Variables Latentes
ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
+ IgnoraSenhalPare + OmiteLimiteVelocidad
STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente
AMBLABORAL =~ PresPersonal + AmbTrabajo 
SEGURIDAD =~  CinturonDeSeguridad + UsaDireccionales
HABPROSOC =~ ComunicVerbal + ComunicAfectiva

# Regresiones
ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

AMBLABORAL ~ STRESS
SEGURIDAD ~ STRESS

#Varianza Covarianzas
ACTAGRE ~~ STRESS
CulebreaConFrecuencia ~~ Ansiedad
FrenoRapidoBrusco ~~ AceleraFrenaBruscamenteSemaforo
# Interceptos"

fit6 <- lavaan::cfa(model6, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit6, fit.measures=TRUE, standardized = TRUE)
semPaths(fit6,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit6, sort = TRUE, maximum.number = 10) 


# MODELO 7

model7 <- " #Variables Latentes
ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
+ IgnoraSenhalPare + OmiteLimiteVelocidad
STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente
AMBLABORAL =~ PresPersonal + AmbTrabajo 
SEGURIDAD =~  CinturonDeSeguridad + UsaDireccionales
HABPROSOC =~ ComunicVerbal + ComunicAfectiva

# Regresiones
ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

AMBLABORAL ~ STRESS
SEGURIDAD ~ STRESS

#Varianza Covarianzas
ACTAGRE ~~ STRESS
CulebreaConFrecuencia ~~ Ansiedad
FrenoRapidoBrusco ~~ AceleraFrenaBruscamenteSemaforo
Ansiedad ~~    StressAlCond

# Interceptos"

fit7 <- lavaan::cfa(model7, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit7, fit.measures=TRUE, standardized = TRUE)
semPaths(fit7,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit7, sort = TRUE, maximum.number = 10) 


# MODELO 8
model8 <- " #Variables Latentes
ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
+ IgnoraSenhalPare + OmiteLimiteVelocidad
STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente
AMBLABORAL =~ PresPersonal + AmbTrabajo 
SEGURIDAD =~  CinturonDeSeguridad + UsaDireccionales
HABPROSOC =~ ComunicVerbal + ComunicAfectiva

# Regresiones
ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

AMBLABORAL ~ STRESS
SEGURIDAD ~ STRESS

#Varianza Covarianzas
ACTAGRE ~~ STRESS
CulebreaConFrecuencia ~~ Ansiedad
FrenoRapidoBrusco ~~ AceleraFrenaBruscamenteSemaforo
Ansiedad ~~ StressAlCond
StressAlCond ~~ UsaDireccionales


# Interceptos"

fit8 <- lavaan::cfa(model8, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit8, fit.measures=TRUE, standardized = TRUE)
semPaths(fit8,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit8, sort = TRUE, maximum.number = 10) 





# MODELO 9

model9 <- " #Variables Latentes
ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
+ IgnoraSenhalPare + OmiteLimiteVelocidad + ComunicVerbal
STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente
AMBLABORAL =~ PresPersonal + AmbTrabajo 
SEGURIDAD =~  CinturonDeSeguridad + UsaDireccionales
HABPROSOC =~ ComunicVerbal + ComunicAfectiva

# Regresiones
ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

AMBLABORAL ~ STRESS
SEGURIDAD ~ STRESS

#Varianza Covarianzas
ACTAGRE ~~ STRESS
CulebreaConFrecuencia ~~ Ansiedad
FrenoRapidoBrusco ~~ AceleraFrenaBruscamenteSemaforo
Ansiedad ~~    StressAlCond
Ansiedad ~~  ComunicAfectiva
StressAlCond ~~ UsaDireccionales

# Interceptos"

fit9 <- lavaan::cfa(model9, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit9, fit.measures=TRUE, standardized = TRUE)
semPaths(fit9,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit9, sort = TRUE, maximum.number = 10) 

fit_index06 <- broom::glance(fit6) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index07 <- broom::glance(fit7) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index08 <- broom::glance(fit8) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

# Uniendo 
bind_rows(fit_index06, fit_index07, fit_index08, .id = "Modelo")



# MODELO 10

model10 <- " #Variables Latentes
ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
+ IgnoraSenhalPare + OmiteLimiteVelocidad + ComunicVerbal
STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente + OmiteLimiteVelocidad
AMBLABORAL =~ PresPersonal + AmbTrabajo 
SEGURIDAD =~  CinturonDeSeguridad + UsaDireccionales
HABPROSOC =~ ComunicVerbal + ComunicAfectiva

# Regresiones
ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

ACTAGRE ~ STRESS
SEGURIDAD ~ STRESS

#Varianza Covarianzas
STRESS ~~ AMBLABORAL
CulebreaConFrecuencia ~~ ComunicVerbal
CulebreaConFrecuencia ~~ Ansiedad 
IgnoraSenhalPare ~~ OmiteLimiteVelocidad

# Interceptos"

fit10 <- lavaan::cfa(model10, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit10, fit.measures=TRUE, standardized = TRUE)
semPaths(fit10,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit10, sort = TRUE, maximum.number = 10) 





fit_index09 <- broom::glance(fit9) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index10 <- broom::glance(fit10) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

# Uniendo 
bind_rows(fit_index06, fit_index07, fit_index08, fit_index09, fit_index10, .id = "Modelo")

# ?ndices de ajuste m?s comunes: CFI (>= .95), TLI (>= .95), RMSEA (<= .05) y SRMR (<= .06).


# MODELO 11

model11 <- " #Variables Latentes
ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
+ IgnoraSenhalPare + OmiteLimiteVelocidad + ComunicVerbal
STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente + OmiteLimiteVelocidad
AMBLABORAL =~ PresPersonal + AmbTrabajo 
SEGURIDAD =~  CinturonDeSeguridad + UsaDireccionales
HABPROSOC =~ ComunicVerbal + ComunicAfectiva

# Regresiones
ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

ACTAGRE ~ STRESS
SEGURIDAD ~ STRESS

#Varianza Covarianzas
STRESS ~~ AMBLABORAL
CulebreaConFrecuencia ~~ ComunicVerbal
CulebreaConFrecuencia ~~ Ansiedad 
IgnoraSenhalPare ~~ OmiteLimiteVelocidad
Ansiedad ~~ StressAlCond

# Interceptos"

fit11 <- lavaan::cfa(model11, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit11, fit.measures=TRUE, standardized = TRUE)
semPaths(fit11,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit11, sort = TRUE, maximum.number = 10) 


# MODELO 12
model12 <- " #Variables Latentes
ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
+ IgnoraSenhalPare + OmiteLimiteVelocidad + ComunicVerbal
STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente + OmiteLimiteVelocidad
AMBLABORAL =~ PresPersonal + AmbTrabajo 
SEGURIDAD =~  CinturonDeSeguridad + UsaDireccionales
HABPROSOC =~ ComunicVerbal + ComunicAfectiva + Ansiedad

# Regresiones
ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

ACTAGRE ~ STRESS
SEGURIDAD ~ STRESS

#Varianza Covarianzas
STRESS ~~ AMBLABORAL
CulebreaConFrecuencia ~~ ComunicVerbal
CulebreaConFrecuencia ~~ Ansiedad 
IgnoraSenhalPare ~~ OmiteLimiteVelocidad
Ansiedad ~~ StressAlCond

# Interceptos"

fit12 <- lavaan::cfa(model12, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit12, fit.measures=TRUE, standardized = TRUE)
semPaths(fit12,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit12, sort = TRUE, maximum.number = 10) 


# MODELO 13
model13 <- " #Variables Latentes
ACTAGRE =~ FrenoRapidoBrusco + AceleraFrenaBruscamenteSemaforo +CulebreaConFrecuencia
+ IgnoraSenhalPare + OmiteLimiteVelocidad + ComunicVerbal
STRESS =~ Ansiedad + StressAlCond + ComunicAfectiva + ConsidCliente + OmiteLimiteVelocidad
AMBLABORAL =~ PresPersonal + AmbTrabajo 
SEGURIDAD =~  CinturonDeSeguridad + UsaDireccionales + StressAlCond
HABPROSOC =~ ComunicVerbal + ComunicAfectiva + Ansiedad

# Regresiones
ACTAGRE + STRESS + AMBLABORAL + SEGURIDAD + HABPROSOC ~ EDUBASICA + 
MENOR_60 + TIEMPO_PROFESION + HORASTRABAJO

ACTAGRE ~ STRESS
SEGURIDAD ~ STRESS

#Varianza Covarianzas
STRESS ~~ AMBLABORAL
CulebreaConFrecuencia ~~ ComunicVerbal
CulebreaConFrecuencia ~~ Ansiedad 
IgnoraSenhalPare ~~ OmiteLimiteVelocidad
Ansiedad ~~ StressAlCond

# Interceptos"

fit13 <- lavaan::cfa(model13, data = DBVL, orthogonal = TRUE, std.lv = TRUE)
summary(fit13, fit.measures=TRUE, standardized = TRUE)
semPaths(fit13,"std", title = FALSE, curvePivot = TRUE)

# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit13, sort = TRUE, maximum.number = 10) 



fit_index11 <- broom::glance(fit11) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index12 <- broom::glance(fit12) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index13 <- broom::glance(fit13) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)


# Uniendo 
bind_rows(fit_index, fit_index02, fit_index03, fit_index04, fit_index05, fit_index06, 
          fit_index07, fit_index08, fit_index09, fit_index10, fit_index11,
          fit_index12, fit_index13, .id = "Modelo")

getwd() 

sink("Resultados.txt")