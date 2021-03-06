
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

DBPerso <- readRDS(file="/Users/williz/Desktop/ModelosED/Database/DBPersonalidad.rds")

names(DBPerso)
head(DBPerso)
View(DBPerso)
str(DBPerso)

# Estadisticas descriptivas

summary(DBPerso)

DBPerso[c("ViajeId","HorTrDia","Edad","TiempoProf","NivEdu","Experiencia")]<- NULL
names(DBPerso)

DBPerso1 <- DBPerso[ ,!colnames(DBPerso)=="ViajeId"]
names(DBPerso1)
#Eliminar Variables que no entran en el analisis factorial

names(DBPerso)

#Alpha Cronbach
reliability(cov(DBPerso))

DBPerso$Suma <- DBPerso$ComVrb+DBPerso$Ans+DBPerso$ComAfec+DBPerso$PrPer+DBPerso$AmbTr+
  DBPerso$StrC+DBPerso$ConCl

summary(DBPerso)
DBPerso[c("Suma")]<-NULL

cor(DBPerso)



#Correlacion
cor(na.omit(DBPerso), use = "pairwise.complete.obs")

Rcor <- cor(na.omit(DBPerso))

# Gráfico de las Correlaciones
corrplot(Rcor, method = "shade", type="upper", order = "hclust", tl.col = "black", tl.cex = 1)

corrplot.mixed(Rcor,lower.col = "black",number.cex=.7)



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
print(cortest.bartlett(Rcor, n=nrow(DBPerso)))


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
pca1 <- princomp(na.omit(DBPerso), scores = TRUE, cor = TRUE)
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

fa <-factanal(na.omit(DBPerso), factor=3, rotation = "varimax", na.rm = TRUE)
print(fa,cutoff=0.3, sort=FALSE)

#sink()

factores <- factanal(na.omit(DBPerso), factor=3, rotation = "varimax", na.rm = TRUE, scores = "regression")$scores

#Normalizamos los valores que podran tomarse como el peso para el cálculo de un indice que
# explica por cada i-esima fila la variabilidad de todo el conjunto de datos

DBPers1 <- cbind(na.omit(DBPersonalidad), factores)

DBPers1$Factor1 <- round(((DBPers1$Factor1 - min(DBPers1$Factor1))/(max(DBPers1$Factor1)-min(DBPers1$Factor1))),3)
DBPers1$Factor2 <- round(((DBPers1$Factor2 - min(DBPers1$Factor2))/(max(DBPers1$Factor2)-min(DBPers1$Factor2))),3)
DBPers1$Factor3 <- round(((DBPers1$Factor3 - min(DBPers1$Factor3))/(max(DBPers1$Factor3)-min(DBPers1$Factor3))),3)

DBPers1

DBPers1 <- rename(DBPers1, replace =c(Factor1 = "AmbLab",
                                    Factor2 = "Stress",
                                    Factor3 = "HabPros"))

DBPers1


par(mfrow=c(1,3))
hist(DBPers1$AmbLab, freq = TRUE, main = "Distribución del Factor 1",
     xlab = "Ambiente Laboral", ylab = "Frecuencia", col = "4")
hist(DBPers1$Stress, freq = TRUE, main = "Distribución del Factor 2",
     xlab = "Stress al Conducir", ylab = "Frecuencia", col = "red")
hist(DBPers1$HabPros, freq = TRUE, main = "Distribución del Factor 3",
     xlab = "Habilidades Prosociales", ylab = "Frecuencia", col = "green")


saveRDS(DBPers1, 
        file="/Users/williz/Desktop/ModelosED/Database/AFPersonalidad.rds")


