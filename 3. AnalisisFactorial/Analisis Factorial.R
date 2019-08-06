
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

#Leer la dataset

indicators <- readRDS(file="/Users/williz/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/Analisis en R/Modelo Logit DB/DBModoConduccion.rds")


# Estadisticas descriptivas

summary(indicators)

cor(na.omit(indicators), use = "pairwise.complete.obs")

Rcor <- cor(na.omit(indicators))

# Gráfico de las Correlaciones
corrplot(Rcor, order = "hclust", tl.col = "black", tl.cex = 1)

# Determinante de la Matriz de correlaciones
det(Rcor)

#Matriz Anti-imagen (Objeto A)
invRcor <-solve(Rcor)
A <- matrix(1,nrow(invRcor),ncol(invRcor))
for (i in 1:nrow(invRcor)){
  for (j in (i):ncol(invRcor)){
    A[i,j] <- invRcor[i,j]/sqrt(invRcor[i,i]*invRcor[j,j])
    A[j,i] <- A[i,j]
  }
}
colnames(A) <- colnames(indicators)
rownames(A) <- colnames(indicators)
print(A)

# Esta matriz muestra el negativo del coeficiente de las correlaciones parciales (aquellas que se
# estiman entre par de variables sin considerar el efecto de las demás). Se interpreta de forma
# inversa a la matriz de correlación: cuando los valores fuera de la diagonal son bajos (cercanos a 0),
# se está en presencia de alta colinealidad entre pares de variables.

# El test de esfericidad de Bartlett busca contrastar la hipótesis nula de que la matriz de
# correlaciones es igual a una matriz de identidad. Lo que nos interesa para efectos de buscar
# multicolinealidad, por lo tanto, es rechazar la hipótesis nula, y aceptar la hipótesis alternativa de
# que la matriz es distinta a una matriz de identidad, y por ende hay un nivel suficiente de
# multicolinealidad entre las variables. Este procedimiento es particularmente útil cuando el tamaño
# muestral es pequeño. 

#Test de esfericidad de Bartlett
print(cortest.bartlett(Rcor, n=nrow(indicators)))


# Como última prueba de multicolinealidad antes de comenzar probar modelos, analizaremos el
# KMO. El índice KMO compara la magnitud de los coeficientes de correlación observados con la
# magnitud de los coeficientes de correlación parcial. Este estadístico varía entre 0 y 1, y se pueden
# calificar de la siguiente forma:
#  0,90 > KMO Muy bueno
#  0,90 > KMO > 0,80 Bueno
#  0,80 > KMO > 0,70 Aceptable
#  0,70 > KMO > 0,60 Mediocre o regular
#  0,60 > KMO > 0,50 Malo
#  0,50 > KMO Inaceptable o muy malo

#KMO
print(KMO(Rcor))

kmo.num <-sum(Rcor^2)-sum(diag(Rcor^2))
kmo.denom <- kmo.num + (sum(A^2)-sum(diag(A^2)))
kmo <- kmo.num/kmo.denom
print(kmo)



#Analisis de Componentes Principales
pca1 <- princomp(na.omit(indicators), scores = TRUE, cor = TRUE)
summary(pca1)

#Cargar los componentes principales
loadings(pca1)
#pca1$loadings

#Screen los componentes principales
plot(pca1)
screeplot(pca1, type = "line", main = "Scree Plot")

biplot(pca1)

#score los componentes
pca1$scores[1:10,]

#Rotation
#varimax(pca1$rotation)
#promax(pca1$rotation)

#Analisis Factorial
fa1 <-factanal(na.omit(indicators), factor=3)
print(fa1,cutoff=0.4, sort=FALSE)

fa2 <-factanal(na.omit(indicators), factor=3, rotation = "varimax", na.rm = TRUE)
print(fa2,cutoff=0.1, sort=FALSE)

sink()

factores <- factanal(na.omit(indicators), factor=3, rotation = "varimax", na.rm = TRUE, scores = "regression")$scores

#Normalizamos los valores que podran tomarse como el peso para el cálculo de un indice que
# explica por cada i-esima fila la variabilidad de todo el conjunto de datos

indicators <- cbind(na.omit(indicators), factores)
indicators$Factor1 <- round(((indicators$Factor1 - min(indicators$Factor1))/(max(indicators$Factor1)-min(indicators$Factor1))),3)
indicators$Factor2 <- round(((indicators$Factor2 - min(indicators$Factor2))/(max(indicators$Factor2)-min(indicators$Factor2))),3)
indicators$Factor3 <- round(((indicators$Factor3 - min(indicators$Factor3))/(max(indicators$Factor3)-min(indicators$Factor3))),3)

indicators

par(mfrow=c(1,3))
hist(indicators$Factor1, freq = TRUE, main = "Distribución del Factor 1",
     xlab = "Factor 1", ylab = "Frecuencia", col = "4")
hist(indicators$Factor2, freq = TRUE, main = "Distribución del Factor 2",
     xlab = "Factor 2", ylab = "Frecuencia", col = "blue")
hist(indicators$Factor3, freq = TRUE, main = "Distribución del Factor 3",
     xlab = "Factor 3", ylab = "Frecuencia", col = "green")










  

