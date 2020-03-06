
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
DBModLog <- read.csv("/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL.csv", header = TRUE, sep = "\t")


MCond <- DBModLog
names(MCond)

MCond$EXP_1 <- ifelse((MCond$Experiencia == 1),1,0)
MCond$EXP_2 <- ifelse((MCond$Experiencia == 2),1,0)
MCond$EXP_3 <- ifelse((MCond$Experiencia == 3),1,0)
MCond$EXP_4 <- ifelse((MCond$Experiencia == 4),1,0)
MCond$EXP_5 <- ifelse((MCond$Experiencia == 5),1,0)


MCond[c("ViajeId")]<-NULL

# ESPECIFICACION DEL MODELO DE VARIABLES LATENTES.


# Nota: Se restringe todas las covarianzas de las variables latentes en el modelo
# CFA para que sean ortogonales con la opcion orthogonal = TRUE

# Si se desea corregir las variaciones de todas las variables latentes en un modelo
# CFA a la unidad, se utiliza el argumento std.lv = TRUE, pero las cargas factoriales
# del primer indicador de cada variable latente ya no se fijan en 1.

# Indices de ajuste más comunes: CFI (>= .95), TLI (>= .95), RMSEA (<= .05) y SRMR (<= .06).

# Los indices de modificación son valores que nos brindarán una orientación acerca de la 
# re-especificación de la estructura factorial evaluada inicialmente.


sink("Modelo_Dos_Factores_alpha_0.1.txt")

# MODELO 1 Dos Factores alpha 0.1 t value= 1.64

model1_0 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoCel
FA_2 =~ PasoPeaton + UsoDirec  

# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + EXP_4 + 
HTRB_2 + HTRB_3 + HTRB_4 +HPICO + CSECO + CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + EXP_4 + 
HTRB_2 + HTRB_3 + HTRB_4 +HPICO + CSECO + CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_0 <- cfa(model1_0, data = MCond, orthogonal = TRUE)
summary(fit1_0, fit.measures= TRUE, standardized = TRUE)
semPaths(fit1_0, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_0, sort = TRUE, maximum.number = 10) 



model1_1 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoCel
FA_2 =~ PasoPeaton + UsoDirec  

# Regresiones
FA_1 ~ CONG_EF +  USOCINTURON 

FA_2 ~ EDUBASICA + EXP_2 + CONG_CD + USOCINTURON + USODISPMOB

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_1 <- cfa(model1_1, data = MCond, orthogonal = TRUE)
summary(fit1_1, fit.measures= TRUE, standardized = TRUE)
semPaths(fit1_1, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_1, sort = TRUE, maximum.number = 10) 



model1_2 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoCel
FA_2 =~ PasoPeaton + UsoDirec  

# Regresiones
FA_1 ~ CONG_EF +  USOCINTURON 
FA_2 ~ EDUBASICA + EXP_2 + CONG_CD + USOCINTURON + USODISPMOB

#Covarianzas
UsoCel ~~ PasoPeaton

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_2 <- cfa(model1_2, data = MCond, orthogonal = TRUE)
summary(fit1_2, fit.measures= TRUE, standardized = TRUE)
semPaths(fit1_2, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_2, sort = TRUE, maximum.number = 10) 



model1_3 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoCel
FA_2 =~ PasoPeaton + UsoDirec  

# Regresiones
FA_1 ~ CONG_EF +  USOCINTURON 
FA_2 ~ EDUBASICA + EXP_2 + CONG_CD + USOCINTURON + USODISPMOB

#Covarianzas
UsoCel ~~ PasoPeaton
OmLmVel ~~ IgPare

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_3 <- cfa(model1_3, data = MCond, orthogonal = TRUE)
summary(fit1_3, fit.measures= TRUE, standardized = TRUE)
semPaths(fit1_3, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_3, sort = TRUE, maximum.number = 10) 



model1_4 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoCel
FA_2 =~ PasoPeaton + UsoDirec

# Regresiones
FA_1 ~ CONG_EF +  USOCINTURON 
FA_2 ~ EDUBASICA + EXP_2 + CONG_CD + USOCINTURON + USODISPMOB

#Covarianzas
UsoCel ~~ PasoPeaton
OmLmVel ~~ IgPare
CulFr ~~   UsoDirec

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_4 <- cfa(model1_4, data = MCond, orthogonal = TRUE)
summary(fit1_4, fit.measures= TRUE, standardized = TRUE)
semPaths(fit1_4, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_4, sort = TRUE, maximum.number = 10) 



model1_5 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoCel
FA_2 =~ PasoPeaton + UsoDirec

# Regresiones
FA_1 ~ CONG_EF +  USOCINTURON 
FA_2 ~ EDUBASICA + EXP_2 + CONG_CD + USOCINTURON + USODISPMOB
FA_2 ~ FA_1

#Covarianzas
UsoCel ~~ PasoPeaton
OmLmVel ~~ IgPare
CulFr ~~   UsoDirec

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_5 <- cfa(model1_5, data = MCond, orthogonal = TRUE)
summary(fit1_5, fit.measures= TRUE, standardized = TRUE)
semPaths(fit1_5, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_5, sort = TRUE, maximum.number = 10) 


model1_6 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr + OmLmVel + IgPare + UsoCel
FA_2 =~ PasoPeaton + UsoDirec

# Regresiones
FA_1 ~ USOCINTURON 
FA_2 ~ EDUBASICA + EXP_2 + USOCINTURON + USODISPMOB
FA_2 ~ FA_1

#Covarianzas
UsoCel ~~ PasoPeaton
OmLmVel ~~ IgPare
CulFr ~~   UsoDirec

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit1_6 <- cfa(model1_6, data = MCond, orthogonal = TRUE)
summary(fit1_6, fit.measures= TRUE, standardized = TRUE)
semPaths(fit1_6, what = "std", style = "mx", title = FALSE, curvePivot = TRUE)
#parameterestimates(fit11)
modindices(fit1_6, sort = TRUE, maximum.number = 10)



# TABLA COMPARATIVA DE TODOS LOS MODELOS

fit_index00 <- broom::glance(fit1_0) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index01 <- broom::glance(fit1_1) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index02 <- broom::glance(fit1_2) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index03 <- broom::glance(fit1_3) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index04 <- broom::glance(fit1_4) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index05 <- broom::glance(fit1_5) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index06 <- broom::glance(fit1_6) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

# Uniendo 
bind_rows(fit_index00, fit_index01, fit_index02, fit_index03, fit_index04, fit_index05,fit_index06, .id = "Modelo")

sink()


#######################################################################################################

#######################################################################################################


sink("Modelo_Tres_Factores_alpha_0.1.txt")

# MODELO 2: Tres Factores alpha 0.05 t value= 1.96


model2_0 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr 
FA_2 =~ OmLmVel + IgPare  
FA_3 =~ PasoPeaton + UsoDirec + UsoCel

# Regresiones
FA_1 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + EXP_4 + 
HTRB_2 + HTRB_3 + HTRB_4 +HPICO + CSECO + CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB

FA_2 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + EXP_4 + 
HTRB_2 + HTRB_3 + HTRB_4 +HPICO + CSECO + CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB

FA_3 ~ EDUBASICA + JOVEN30 + ADULTO40 + ADULTO60 + EXP_1 + EXP_2 + EXP_3 + EXP_4 + 
HTRB_2 + HTRB_3 + HTRB_4 +HPICO + CSECO + CONG_CD + CONG_EF + SININFOTRF + USOCINTURON + USODISPMOB

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_0 <- cfa(model2_0, data = MCond, orthogonal = TRUE)
summary(fit2_0, fit.measures=TRUE, standardized = TRUE)
semPaths(fit2_0,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_0, sort = TRUE, maximum.number = 10) 


model2_1 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr 
FA_2 =~ OmLmVel + IgPare  
FA_3 =~ PasoPeaton + UsoDirec + UsoCel

# Regresiones
FA_1 ~ CONG_EF + SININFOTRF + USOCINTURON

FA_2 ~ EXP_1 + HPICO + CONG_CD + CONG_EF + USOCINTURON 

FA_3 ~ EDUBASICA + EXP_2 + CONG_CD + USOCINTURON + USODISPMOB

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_1 <- cfa(model2_1, data = MCond, orthogonal = TRUE)
summary(fit2_1, fit.measures=TRUE, standardized = TRUE)
semPaths(fit2_1,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_1, sort = TRUE, maximum.number = 10) 


model2_2 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr 
FA_2 =~ OmLmVel + IgPare  
FA_3 =~ PasoPeaton + UsoDirec + UsoCel

# Regresiones
FA_1 ~ CONG_EF + SININFOTRF + USOCINTURON

FA_2 ~ EXP_1 + HPICO + CONG_CD + CONG_EF + USOCINTURON 

FA_3 ~ EDUBASICA + EXP_2 + CONG_CD + USOCINTURON + USODISPMOB

FA_2 ~ FA_1

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_2 <- cfa(model2_2, data = MCond, orthogonal = TRUE)
summary(fit2_2, fit.measures=TRUE, standardized = TRUE)
semPaths(fit2_2,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_2, sort = TRUE, maximum.number = 10) 


model2_3 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr 
FA_2 =~ OmLmVel + IgPare + UsoCel 
FA_3 =~ PasoPeaton + UsoDirec + UsoCel

# Regresiones
FA_1 ~ CONG_EF + SININFOTRF + USOCINTURON

FA_2 ~ EXP_1 + HPICO + CONG_CD + CONG_EF + USOCINTURON 

FA_3 ~ EDUBASICA + EXP_2 + CONG_CD + USOCINTURON + USODISPMOB

FA_2 ~ FA_1

# Interceptos"

# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_3 <- cfa(model2_3, data = MCond, orthogonal = TRUE)
summary(fit2_3, fit.measures=TRUE, standardized = TRUE)
semPaths(fit2_3,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_3, sort = TRUE, maximum.number = 10) 


model2_4 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr 
FA_2 =~ OmLmVel + IgPare + UsoCel 
FA_3 =~ PasoPeaton + UsoDirec + UsoCel

# Regresiones
FA_1 ~ CONG_EF + SININFOTRF + USOCINTURON

FA_2 ~ EXP_1 + HPICO + CONG_CD + CONG_EF + USOCINTURON 

FA_3 ~ EDUBASICA + EXP_2 + CONG_CD + USOCINTURON + USODISPMOB

FA_2 ~ FA_1

#Covarianzas
UsoCel ~~ PasoPeaton

# Interceptos"


# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_4 <- cfa(model2_4, data = MCond, orthogonal = TRUE)
summary(fit2_4, fit.measures=TRUE, standardized = TRUE)
semPaths(fit2_4,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_4, sort = TRUE, maximum.number = 10) 



model2_5 <- " #Variables Latentes
FA_1 =~ FRbr + EnfCond + AFrSem + CulFr 
FA_2 =~ OmLmVel + IgPare + UsoCel 
FA_3 =~ PasoPeaton + UsoDirec + UsoCel

# Regresiones
FA_1 ~ CONG_EF + SININFOTRF + USOCINTURON

FA_2 ~ EXP_1 + HPICO + CONG_CD  + USOCINTURON 

FA_3 ~ EDUBASICA + EXP_2 + CONG_CD + USOCINTURON + USODISPMOB

FA_2 ~ FA_1

#Covarianzas
UsoCel ~~ PasoPeaton

# Interceptos"


# Modelo sin correccion de variaciones, el primer argumento de las variables latentes es 1

fit2_5 <- cfa(model2_5, data = MCond, orthogonal = TRUE)
summary(fit2_5, fit.measures=TRUE, standardized = TRUE)
semPaths(fit2_5,"std", title = FALSE, curvePivot = TRUE)
# Solicitar los 10 primeros IM con valores m?s altos
modindices(fit2_5, sort = TRUE, maximum.number = 10) 


# TABLA COMPARATIVA DE TODOS LOS MODELOS

fit_index00 <- broom::glance(fit2_0) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index01 <- broom::glance(fit2_1) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index02 <- broom::glance(fit2_2) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index03 <- broom::glance(fit2_3) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index04 <- broom::glance(fit2_4) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

fit_index05 <- broom::glance(fit2_5) %>% 
  select(chisq, npar, cfi, tli, rmsea, rmsea.conf.high, srmr, aic, bic, estimator)

# Uniendo 
bind_rows(fit_index00, fit_index01, fit_index02, fit_index03, fit_index04, fit_index05, .id = "Modelo")

sink()

