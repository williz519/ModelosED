
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
library(caret)
library(lmtest)
library(reshape)
library(generalhoslem)
library(DescTools)
library(VGAM)
library(ordinal)

# Limpiar Entorno de trabajo
rm(list = ls())

## Semilla
set.seed(1)


# Cargar Base de datos
dt   <- readRDS("/Users/williz/Desktop/ModelosED/1.5 Articulo 1.5/2 Database/DataBase.rds")

## SEMAFOROS EN EL VIAJE

dt$Semaforos <-  (ifelse((dt$CHOICE == 1), (dt$Semaf_A1/dt$DISTAlt1),
                         ifelse((dt$CHOICE == 2), (dt$Semaf_A2/dt$DISTAlt2),
                                ifelse((dt$CHOICE == 3), (dt$Semaf_A3/dt$DISTAlt3),
                                       (dt$Semaf_EC/dt$DISTEC)))))

dt$Semaforos

## Camaras de Fotomultas

dt$Camaras <- (ifelse((dt$CHOICE == 1), (dt$CamFD_A1/dt$DISTAlt1),
                      ifelse((dt$CHOICE == 2), (dt$CamFD_A2/dt$DISTAlt2),
                             ifelse((dt$CHOICE == 3), (dt$CamFD_A3/dt$DISTAlt3),
                                    (dt$CamFD_EC/dt$DISTEC)))))

dt$Camaras

### Velocidad
## Baja <= 16.09, media <= 27.4

dt$Velocidad <- (ifelse((dt$V_promedio <= 17), 1,
                        ifelse((dt$V_promedio < 27), 2,3)))

dt$Velocidad <- factor(dt$Velocidad,
                       levels = 1:3,
                       labels = c("Baja","Media","Alta"))
dt$Velocidad <- ordered(dt$Velocidad, levels = c("Baja","Media","Alta"))
prop.table(table(dt$Velocidad))*100

dt$CONGESTION <-ordered(dt$CONGESTION)

# Variables Dummy Modelo Logistico

dt$CongA <- (ifelse((dt$CONGESTION == "Nivel A"),1,0))
dt$CongB <- (ifelse((dt$CONGESTION == "Nivel B"),1,0))
dt$CongC <- (ifelse((dt$CONGESTION == "Nivel C"),1,0))
dt$CongD <- (ifelse((dt$CONGESTION == "Nivel D"),1,0))
dt$CongE <- (ifelse((dt$CONGESTION == "Nivel E"),1,0))
dt$CongF <- (ifelse((dt$CONGESTION == "Nivel F"),1,0))

dt$CLIMAlluvioso <- (ifelse((dt$CLIMA == "lluvioso"),1,0))
dt$CLIMADespejado <- (ifelse((dt$CLIMA == "Despejado"),1,0))

dt$HPICO <- (ifelse((dt$HPICOHVALLE == "Pico"),1,0))
dt$HVALLE <- (ifelse((dt$HPICOHVALLE == "valle"),1,0))

dt$AM <- (ifelse((dt$MERIDIANO == "AM"),1,0))
dt$PM <- (ifelse((dt$MERIDIANO == "PM"),1,0))

dt$Inc_Si <- (ifelse((dt$INCIDENTE == "Si"),1,0))
dt$Inc_No <- (ifelse((dt$INCIDENTE == "NO"),1,0))

dt$FRBrNunca <- (ifelse((dt$FRbr == "Nunca"),1,0))
dt$FRBrCasiNunca <- (ifelse((dt$FRbr == "Casi Nunca"),1,0))
dt$FRBrOcasionalmente <- (ifelse((dt$FRbr == "Ocasionalmente"),1,0))
dt$FRBrCasisiempre <- (ifelse((dt$FRbr == "Casi siempre"),1,0))
dt$FRBrSiempre <- (ifelse((dt$FRbr == "Siempre"),1,0))



#db$CONGESTION <- relevel(db$CONGESTION, ref = "Nivel F")

#db$CLIMA <- relevel(db$CLIMA, ref = "Despejado")

dt$Experiencia <- relevel(dt$Experiencia, ref = "Menos 2 años")

## TEST CHI CUADRADO VARIABLE CULITATIVAS
tabla <-table(dt$Velocidad, dt$Semaforos)
chisq.test(x=tabla)

ks.test(x = dt$Semaforos,"pnorm", mean(dt$Semaforos), sd(dt$Semaforos))
histogram(dt$Semaforos)
ks.test(x = dt$TIEMPO_PROFESION,"pnorm", mean(dt$TIEMPO_PROFESION), sd(dt$TIEMPO_PROFESION))
ks.test(x = dt$Camaras,"pnorm", mean(dt$Camaras), sd(dt$Camaras))
ks.test(x = dt$Duracion,"pnorm", mean(dt$Duracion), sd(dt$Duracion))


## Particion de la DB

registros <- createDataPartition(dt$Velocidad, p = 0.8, list = FALSE)
db <- dt[registros,]
db_test <- dt[-registros,]

#write.xlsx(db, 
#            file="/Users/williz/Desktop/ModelosED/1.5 Articulo 1.5/2 Database/db.xlsx", 
#           col.names = TRUE, row.names = TRUE, append = FALSE )

# 

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

modelo <- lm(COSTO ~ Duracion + Distancia, data = db )
summary(modelo)

#predict(modelo, db_test)


####  REGRESION LOGISTICA politomica ordinal

## Seleccion del modelo
modelo0 <- polr(db$Velocidad ~ 1, 
                method = "logistic", Hess = TRUE, data = db)

## Ajustamos el modelo
modelo1 <- polr(db$Velocidad ~ db$CONGESTION, 
                method = "logistic", Hess = TRUE, data = db)

modelo2 <- polr(db$Velocidad ~ db$CONGESTION + db$CLIMA, 
                method = "logistic", Hess = TRUE, data = db)

modelo3 <- polr(db$Velocidad ~ db$CONGESTION + db$CLIMA + db$Semaforos, 
                method = "logistic", Hess = TRUE, data = db)

modelo4 <- polr(db$Velocidad ~ db$CONGESTION + db$Semaforos + 
                  db$HPICOHVALLE, 
                   method = "logistic", Hess = TRUE, data = db)

modelo5 <- polr(db$Velocidad ~ db$CONGESTION + db$CLIMA + db$Semaforos + 
                  db$HPICOHVALLE + db$MERIDIANO, 
                method = "probit", Hess = TRUE, data = db)

modelo6 <- polr(db$Velocidad ~ db$CONGESTION + db$Semaforos + 
                  db$HPICOHVALLE + db$MERIDIANO, 
                method = "logistic", Hess = TRUE, data = db)

anova(modelo0, modelo1, test = "Chisq")
anova(modelo1, modelo2, test = "Chisq")
anova(modelo2, modelo3, test = "Chisq")
anova(modelo4, modelo3, test = "Chisq")
anova(modelo3, modelo5, test = "Chisq")
anova(modelo5, modelo6, test = "Chisq")
anova(modelo4, modelo6, test = "Chisq")


### MODELO ESCOGIDO
modelo_log <- modelo6



#Resumen del modelo
summary(modelo_log)

# default method gives profiled CIs
(ci <- confint(modelo_log)) 
## odds ratios
OR <- exp(coef(modelo_log))

## OR and CI
exp(cbind(OR = coef(modelo_log), ci))

# Para sacar el valor P de cada variable
(ctable<-coef(summary(modelo_log)))
p <- pnorm(abs(ctable[,"t value"]), lower.tail = FALSE)
(ctable <- cbind(ctable, "p value"=p, OR))

#######  RESULTADOS

# PREDICCION
modelo_predictions <- predict(modelo_log, db)
# ERROR
mean(factor(modelo_predictions, ordered = TRUE) == db$Velocidad)
# Matriz de confusion
table(predicted = modelo_predictions, actual = db$Velocidad)

# Predicciones con probabilidad: se adicionan a la base
probabilidades_modelo <- predict(modelo_log, type = "probs")
db$prediccion <- predict(modelo_log, db)
db$prediccion


## Deviance, log-likelihood and AIC
VGAM::deviance(modelo_log)
VGAM::logLik(modelo_log)
VGAM::AIC(modelo_log)

sumOrd   <- summary(modelo_log)
(coefOrd <- coef(sumOrd))

VGAM::confint(modelo_log, method="profile")
exp(MASS:::confint.polr(modelo_log))


## AJUSTE GLOBAL DEL MODELO ############

# Test de chi-cuadrado de razón de verosimilitud:
# Un modelo ajustado por polr es un glm especial, por lo que todas las suposiciones que se 
# aplican a un glm tradicional se mantienen aquí. Si cuida los parámetros correctamente, 
# puede averiguar la distribución. Específicamente, para probar si el modelo es bueno o no, 
# es posible que desee hacer una prueba de bondad de ajuste, que prueba el siguiente nulo 
# (observe que esto es sutil, en su mayoría desea rechazar el nulo, pero aquí no desea 
# rechazarlo para que encaje bien):
# 𝐻𝑜: el modelo actual es suficientemente bueno
# Usaría la prueba de chi-cuadrado para esto. El valor p se obtiene como:

pvalue = 1-pchisq(deviance(modelo_log),df.residual(modelo_log))
pvalue

## Valor de la Chi Cuadrado
qchisq(1-pvalue, 17)

#test de Wald
waldtest(modelo_log)

# p-value
p_value <- 1-pchisq(deviance(modelo_log),df.residual(modelo_log))
paste("p-value:", round(p_value, 5))
coeftest(modelo_log)


# Test Lipsitz
# La prueba de Lipsitz es una prueba de bondad de ajuste para modelos de regresión logística 
# de respuesta ordinal. Implica agrupar los datos observados en grupos de igual tamaño 
# según una puntuación de respuesta ordinal. Esta puntuación se calcula sumando las probabilidades 
# predichas de cada sujeto para cada nivel de resultado multiplicado por pesos enteros 
# igualmente espaciados. El usuario puede especificar el número de grupos asignando un valor 
# entero a g, que es 10 por defecto. g<= N/(5*K), N observaciones, K items de respuesta

lipsitz.test(modelo_log, g = 10)


# Test Hosmer-Lemeshow
# Las pruebas de Hosmer-Lemeshow Las pruebas de Hosmer-Lemeshow son pruebas de bondad de ajuste
# para modelos de regresión logística binarios, multinomiales y ordinales. logitgof es capaz de 
# realizar los tres. Esencialmente, comparan las frecuencias observadas con las esperadas del 
# resultado y calculan una estadística de prueba que se distribuye de acuerdo con la distribución 
# chi-cuadrado. Los grados de libertad dependen del número de cuantiles utilizados y del 
# número de categorías de resultados. Un valor p no significativo indica que no hay evidencia 
# de que las frecuencias observadas y esperadas difieran (es decir, evidencia de un buen ajuste).

logitgof(db$Velocidad, fitted(modelo_log), g = 10, ord = TRUE)


## Test Pulkstenis-Robinson
#  Las pruebas de Pulkstenis-Robinson son pruebas de bondad de ajuste para modelos de regresión 
# logística ordinal. Son capaces de acomodar modelos con predictores continuos y categóricos. 
# Los datos se dividen de acuerdo con los patrones de covariables observados utilizando únicamente 
# las covariables categóricas. Se descartan todos los patrones de covariables no observados. 
# Solo se utilizan predictores categóricos para evitar la división entre un número inaceptablemente 
# alto de patrones de covariables. A cada sujeto se le asigna una puntuación de respuesta ordinal 
# sumando las probabilidades predichas de cada sujeto para cada nivel de resultado multiplicado 
# por pesos enteros igualmente espaciados. Los patrones de covariables se dividen en dos en 
# la puntuación media dentro de cada uno.

pulkrob.chisq(modelo_log, c("db$CONGESTION"))

#pulkrob.chisq(modelo_log, c("db$CLIMA"))

pulkrob.chisq(modelo_log, c("db$HPICOHVALLE"))

pulkrob.chisq(modelo_log, c("db$MERIDIANO"))

pulkrob.chisq(modelo_log, c("db$INCIDENTE"))

pulkrob.deviance(modelo_log, c("db$CONGESTION"))


# Tasa de clasificaciones correctas
mean(factor(modelo_predictions, ordered = TRUE) == db$Velocidad)


### CALIDAD DE AJUSTE DEL MODELO ################

# El cálculo de éstos dependen del valor de las devianzas del modelo final y del modelo inicial 
# con sólo la constante (modelo0)

PseudoR2(modelo_log, which=c("all"))

dv0 <- deviance(modelo0)
dvf <- deviance(modelo_log)
N <- nrow(db)
k <- 8

LLv <- logLik(modelo_log)
LLo <- logLik(modelo0)

## R2MACFADDEN - Si R2MF es menor que 0.2, se afirma que el modelo no tiene un buen ajuste ##
## Suele considerarse una buena calidad de ajuste cuando 0.2 >= R2MF <= 0.4 y excelente para valores superiores

R2MF <- 1-(dvf/dv0)
R2MF

AdjR2MF <- 1-((0.5*dvf+k+1)/(0.5*dv0+1))
AdjR2MF

## R2 Cox-Snell
R2CS <- 1-exp((dvf-dv0)/N)
R2CS

## R2 de Nagelkerke 0<= R2N < 1
R2N <- (1-exp((dvf-dv0)/N))/(1-exp(-dv0/N))
R2N

### R2 Ben Akiva Lerman k/dv0<R2BL<(dv0 + k)/dv0
R2BL <- 1-((LLv-k)/LLo)
li<-k/LLo
ls<-(LLo + k)/LLo
(ctable <- cbind(li,R2BL, ls ))


### Horowitz k/(2*dv0)<=R2H < (2*dv0+k)/(2*dv0)
R2H <- 1-((LLv-k/2)/LLo)
(ctable <- cbind(k/(2*LLo), R2H, (2*LLo+k)/(2*LLo) ))

### Maddala

R2Md <- 1-exp(-(2*(LLv-LLo)/N))
(ctable <- cbind(0, R2Md, 1-exp(2*LLo/N)))

### Cragg Uhler Nagelkerke
R2CUN <- (1-exp(-(2*(LLv-LLo)/N)))/(1-exp(2*LLo/N))
R2CUN


(ctable <- cbind(R2MF, AdjR2MF, R2CS, R2N, R2BL, R2M, R2H))



#### VALIDACION DEL MODELO ##########

## Aciertos sobre la base de testeo
ce <- modelo_log$coefficients         # coefficients b1, b2
ic <- modelo_log$zeta                 # intercepts b0.1, b0.2, b0.3

## Modelo 5
logit1 <- ic[1] - (ce[1]*db_test$CongB + ce[2]*db_test$CongC + ce[3]*db_test$CongD +
                     ce[4]*db_test$CongE + ce[5]*db_test$CongF + ce[6]*db_test$CLIMAlluvioso +
                     ce[7]*db_test$Semaforos + ce[8]*db_test$HPICO + ce[9]*db_test$PM)

logit2 <- ic[2] - (ce[1]*db_test$CongB + ce[2]*db_test$CongC + ce[3]*db_test$CongD +
                     ce[4]*db_test$CongE + ce[5]*db_test$CongF + ce[6]*db_test$CLIMAlluvioso +
                     ce[7]*db_test$Semaforos + ce[8]*db_test$HPICO + ce[9]*db_test$PM)

## Modelo 6
# modelo6 <- polr(db$Velocidad ~ db$CONGESTION + db$Semaforos + 
# db$HPICOHVALLE + db$MERIDIANO, method = "logistic", Hess = TRUE, data = db)

logit1 <- ic[1] - (ce[1]*db_test$CongB + ce[2]*db_test$CongC + ce[3]*db_test$CongD +
                     ce[4]*db_test$CongE + ce[5]*db_test$CongF + 
                     ce[6]*db_test$Semaforos + ce[7]*db_test$HPICO + ce[8]*db_test$PM)

logit2 <- ic[2] - (ce[1]*db_test$CongB + ce[2]*db_test$CongC + ce[3]*db_test$CongD +
                     ce[4]*db_test$CongE + ce[5]*db_test$CongF + 
                     ce[6]*db_test$Semaforos + ce[7]*db_test$HPICO + ce[8]*db_test$PM)

### Modelo con el teste de wald
logit1 <- ic[1] - (ce[1]*db_test$CongB + ce[3]*db_test$CongD +
                     ce[6]*db_test$Semaforos + ce[7]*db_test$HPICO + ce[8]*db_test$PM)

logit2 <- ic[2] - (ce[1]*db_test$CongB + ce[3]*db_test$CongD +
                     ce[6]*db_test$Semaforos + ce[7]*db_test$HPICO + ce[8]*db_test$PM)




pLeq1  <- 1 / (1 + exp(-logit1))   # p(Y <= 1)
pLeq2  <- 1 / (1 + exp(-logit2))   # p(Y <= 2)
pMat   <- cbind(P1 =pLeq1, P2 =pLeq2-pLeq1, P3 =1-pLeq2)
pMat

prediccion = array()

for (i in 1:nrow(db_test)){
  if (max(pMat[i,],pMat[i,], pMat[i,]) == pMat[i,1]) {prediccion[i] = 1}
  else
  {if (max(pMat[i,],pMat[i,], pMat[i,]) == pMat[i,2]) {prediccion[i] = 2}
    else
    {prediccion[i] = 3}
  }
}

prediccion <- factor(prediccion,
                     levels = 1:3,
                     labels = c("Baja","Media","Alta"))

prediccion <- ordered(prediccion, levels = c("Baja","Media","Alta"))

table(predicted = prediccion, actual = db_test$Velocidad)

mean(factor(prediccion, ordered = TRUE) == db_test$Velocidad)
