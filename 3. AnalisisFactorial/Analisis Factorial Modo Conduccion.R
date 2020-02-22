
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

DBModoCond <- readRDS(file="/Users/williz/Desktop/ModelosED/Database/DBModoConduccion.rds")
DBModLog <- read.csv("/Users/williz/Desktop/ModelosED/Database/ModeloLogitVL.csv", header = TRUE, sep = "\t")

names(DBModoCond)
names(DBModLog)

ModoCond <- DBModLog %>%
  select(ViajeId,Experiencia) %>%
  # DB Modelo Logistico
  inner_join(DBModoCond %>%
               select(ViajeId:DispMob),
             by = "ViajeId")

names(ModoCond)

tibble::as_tibble(ModoCond) 

ModoCond[c("ViajeId","Genero","INFOTRAFICO","CinSeg","DispMob","Experiencia")] <- NULL
names(ModoCond)


reliability(cov(ModoCond))

ModoCond$Suma <- ModoCond$FRbr+ModoCond$UsoDirec+ModoCond$EnfCond+ModoCond$AFrSem+ModoCond$CulFr+ModoCond$OmLmVel+
  ModoCond$IgPare+ModoCond$UsoCel

cor(ModoCond)
ModoCond$PasoPeaton[ModoCond$PasoPeaton == 1] <- 4
ModoCond$PasoPeaton[ModoCond$PasoPeaton == 3] <- 1
ModoCond$PasoPeaton[ModoCond$PasoPeaton == 4] <- 3

ModoCond$OmLmVel[ModoCond$OmLmVel == 1]<- 4
ModoCond$OmLmVel[ModoCond$OmLmVel == 3]<- 1
ModoCond$OmLmVel[ModoCond$OmLmVel == 4]<- 3

ModoCond[c("Suma")]<- NULL

cor(ModoCond)

sink("AFModoConduccion.txt")

#Alpha de Cronbach
reliability(cov(ModoCond))

summary(ModoCond)
# Estadisticas descriptivas

summary(ModoCond)


cor(ModoCond, use = "pairwise.complete.obs")

Rcor <- cor(ModoCond)

# Gráfico de las Correlaciones
corrplot(Rcor, order = "AOE", method = c("shade"), tl.col = "black", addCoef.col = "black",
         tl.srt= 45, tl.cex = 1, type = "upper", diag = F,  addshade = "all")

corrplot.mixed(Rcor,lower.col = "black",number.cex=.7, title("Matriz de Correlación"))


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
colnames(A) <- colnames(indicators1)
rownames(A) <- colnames(indicators1)
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
print(cortest.bartlett(Rcor, n=nrow(ModoCond)))


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
pca1 <- princomp(ModoCond, scores = TRUE, cor = TRUE)
summary(pca1)

#Cargar los componentes principales
loadings(pca1)
#pca1$loadings

#Screen los componentes principales
plot(pca1)
screeplot(pca1, type = "line", main = "Scree Plot")

biplot(pca1)

psych::scree(pca1)

#score los componentes
pca1$scores[1:10,]

#Rotation
#varimax(pca1$rotation)
#promax(pca1$rotation)

#Analisis Factorial
fa <-factanal(ModoCond, factors = 3, rotation = "varimax", na.rm = TRUE,lower = 0.05)
print(fa,cutoff=0.3, sort=FALSE)

fa1 <-factanal(ModoCond, factor = 4, rotation = "varimax", na.rm = TRUE, lower = 0.05)
print(fa1,cutoff=0.3, sort=FALSE)

#sink()

factores <- factanal(ModoCond, factor=3, rotation = "varimax", na.rm = TRUE, scores = "regression")$scores

#Normalizamos los valores que podran tomarse como el peso para el cálculo de un indice que
# explica por cada i-esima fila la variabilidad de todo el conjunto de datos

indicators2 <- cbind(ModoCond, factores)
indicators2$Factor1 <- round(((indicators2$Factor1 - min(indicators2$Factor1))/(max(indicators2$Factor1)-min(indicators2$Factor1))),3)
indicators2$Factor2 <- round(((indicators2$Factor2 - min(indicators2$Factor2))/(max(indicators2$Factor2)-min(indicators2$Factor2))),3)
indicators2$Factor3 <- round(((indicators2$Factor3 - min(indicators2$Factor3))/(max(indicators2$Factor3)-min(indicators2$Factor3))),3)

indicators2

indicators2 <- rename(indicators2, replace =c(Factor1 = "ActitudAgresiva",
                                              Factor2 = "ViolacionNormas",
                                              Factor3 = "Impaciencia"))



par(mfrow=c(1,3))
hist(indicators2$ActitudAgresiva, freq = TRUE, main = "Distribución del Factor 1",
     xlab = "Actitud Agresiva", ylab = "Frecuencia", col = "red")
hist(indicators2$ViolacionNormas, freq = TRUE, main = "Distribución del Factor 2",
     xlab = "Violación Normas", ylab = "Frecuencia", col = "blue")
hist(indicators2$Impaciencia, freq = TRUE, main = "Distribución del Factor 3",
     xlab = "Impaciencia", ylab = "Frecuencia", col = "green")

sink()

saveRDS(indicators2, file="/Users/williz/Desktop/ModelosED/Database/AFModoConduccion.rds")










  

