#Installing the Psych package and loading it
#install.packages("psych")

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

#Leer los dataset Personalidad y Modo Conduccion

# Analisis Factorial con datos escalados a una escala likert 1 - 5

DB <- read.csv("/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVLCE.csv", header = TRUE, sep = "\t")

names(DB)

DBConjunta <- select(DB, -("GENERO":"UsaPito"))

names(DBConjunta)

summary(DBConjunta)

sink("AnalisisFactorialConjuntoCE.txt")

#Correlacion
cor(DBConjunta, use = "pairwise.complete.obs")

Rcor <- cor(DBConjunta)

# Gráfico de las Correlaciones
corrplot(Rcor, method = "shade", type="upper", order = "hclust", tl.col = "black", tl.cex = 1)

corrplot.mixed(Rcor,lower.col = "black",number.cex=.6)

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
colnames(A) <- colnames(DBPers)
rownames(A) <- colnames(DBPers)
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
print(cortest.bartlett(Rcor, n=nrow(DBConjunta)))


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
pca1 <- princomp(DBConjunta, scores = TRUE, cor = TRUE)
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

fa <-factanal(DBConjunta, factor=5, rotation = "varimax", na.rm = TRUE)
print(fa,cutoff=0.35, sort=FALSE)

fa2 <-factanal(DBConjunta, factor=4, rotation = "varimax", na.rm = TRUE)
print(fa2,cutoff=0.3, sort=FALSE)



factores <- factanal(DBConjunta, factor=5, rotation = "varimax", na.rm = TRUE, scores = "regression")$scores
print(factores,cutoff=0.35, sort=FALSE)

#Normalizamos los valores que podran tomarse como el peso para el cálculo de un indice que
# explica por cada i-esima fila la variabilidad de todo el conjunto de datos

DBFAC <- cbind(DBConjunta, factores)

DBFAC$Factor1 <- round(((DBFAC$Factor1 - min(DBFAC$Factor1))/(max(DBFAC$Factor1)-min(DBFAC$Factor1))),5)
DBFAC$Factor2 <- round(((DBFAC$Factor2 - min(DBFAC$Factor2))/(max(DBFAC$Factor2)-min(DBFAC$Factor2))),5)
DBFAC$Factor3 <- round(((DBFAC$Factor3 - min(DBFAC$Factor3))/(max(DBFAC$Factor3)-min(DBFAC$Factor3))),5)
DBFAC$Factor4 <- round(((DBFAC$Factor4 - min(DBFAC$Factor4))/(max(DBFAC$Factor4)-min(DBFAC$Factor4))),5)
DBFAC$Factor5 <- round(((DBFAC$Factor5 - min(DBFAC$Factor5))/(max(DBFAC$Factor5)-min(DBFAC$Factor5))),5)


DBFAC

DBFAC <- rename(DBFAC, replace = c(Factor1 = "CondAgresiva",
                                   Factor2 = "Stress",
                                   Factor3 = "AmbienteLaboral",
                                   Factor4 = "CondSegura",
                                   Factor5 = "HabProsoc"))



par(mfrow=c(2,3))

hist(DBFAC$CondAgresiva, freq = TRUE, main = "Distribución del Factor 1",
     xlab = "Cond Agresiva", ylab = "Frecuencia", col = "red")
hist(DBFAC$Stress, freq = TRUE, main = "Distribución del Factor 2",
     xlab = "Stress al Cond", ylab = "Frecuencia", col = "red")
hist(DBFAC$AmbienteLaboral, freq = TRUE, main = "Distribución del Factor 3",
     xlab = "Ambiente Laboral", ylab = "Frecuencia", col = "green")
hist(DBFAC$CondSegura, freq = TRUE, main = "Distribución del Factor 4",
     xlab = "Cond Segura", ylab = "Frecuencia", col = "3")
hist(DBFAC$HabProsoc, freq = TRUE, main = "Distribución del Factor 5",
     xlab = "Hab Prosociales", ylab = "Frecuencia", col = "blue")

sink()

saveRDS(DBFAC, 
        file="/Users/williz/Desktop/ModelosED/Database/AFConjunto.rds")







