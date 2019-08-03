
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

DB_file <- "/Users/williz/Desktop/rutasviajes/DB_Viajes.xlsx"
DB <- DB_file %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_xlsx, path = DB_file)

# Zonificar las ubicaciones
#Ubicacion <- DB$Ubicacion %>%
#  separate(Nombre, c("Nombre", "Zona"),sep = " - Zona ") %>%
#  arrange(Nombre) %>%
#  mutate(Cod = 1:n()) %>%
#  st_as_sf(coords = c("Longitud","Latitud")) %>%
#  st_set_crs(4326)

#Viaje <- DB$Viajes %>%
#  select(ViajeId:CostoCarrera) %>%
#  # Origen
#  inner_join(Ubicacion %>%
#               rename_all(paste, "Origen", sep = "_"),
#             by = c("OrigenId" = "Id_Origen")) %>%
#  # Destino
#  inner_join(Ubicacion %>%
#               rename_all(paste, "Destino", sep = "_"),
#             by = c("DestinoId" = "Id_Destino")) %>%
#  # Estado tráfico y condiciones
#  inner_join(DB$EstadoTraficoCondiciones %>%
#               select(ViajeId:TipoIncidente),
#             by = "ViajeId") %>%
#  # Modo conducción
#  inner_join(DB$ModoConduccion %>%
#               select(ViajeId:UsoCelular),
#             by = "ViajeId") %>%
#  # Caracterización taxista
#  inner_join(DB$CaracterizacionTaxista %>%
#               select(ViajeId:Edad),
#             by = "ViajeId") %>%
#  # Personalidad conductor
#  inner_join(DB$PersonalidadConductor %>%
#               select(ViajeId:AmbienteOrdenadoDesordenado),
#             by = "ViajeId")


DBModo <- DB$Viajes %>%
  select(ViajeId) %>%
  # Modo conducción
  inner_join(DB$ModoConduccion %>%
               select(ViajeId:RespetuosaIrrespetuosa,UsoCelular),
             by = "ViajeId")
View(DBModo)
attach(DBModo)

#Frecuencias de las variables DBModo

prop.table(table(DBModo$CinturonDeSeguridad))
prop.table(table(DBModo$PasoPeatones))
prop.table(table(DBModo$UsaPito))
prop.table(table(DBModo$FrenoRapidoBrusco))
prop.table(table(DBModo$UsaDireccionales))
prop.table(table(DBModo$AceleraFrenaBruscamenteSemaforo))
prop.table(table(DBModo$EnfadoConOtroConductor))
prop.table(table(DBModo$CulebreaConFrecuencia))
prop.table(table(DBModo$OmiteLimiteVelocidad))
prop.table(table(DBModo$IgnoraSenhalPare))
prop.table(table(DBModo$UsoCelular))

  

#Variables en las cuales hay no observados

DBModo$UsoCelular <- replace(DBModo$UsoCelular, DBModo$UsoCelular == 4, 1)

#Definimos las variables
X <- data.frame(DBModo$CinturonDeSeguridad,DBModo$PasoPeatones, DBModo$UsaPito, DBModo$FrenoRapidoBrusco,
                DBModo$UsaDireccionales, DBModo$EnfadoConOtroConductor, DBModo$AceleraFrenaBruscamenteSemaforo,
                DBModo$CulebreaConFrecuencia, DBModo$OmiteLimiteVelocidad, DBModo$IgnoraSenhalPare,
                DBModo$UsoCelular)

X <- rename(X, replace =c(DBModo.CinturonDeSeguridad = "CinturonDeSeguridad",
                          DBModo.PasoPeatones = "PasoPeatones",
                          DBModo.UsaPito = "UsaPito", 
                          DBModo.FrenoRapidoBrusco = "FrenoRapidoBrusco",
                          DBModo.UsaDireccionales = "UsaDireccionales", 
                          DBModo.EnfadoConOtroConductor = "EnfadoConOtroConductor", 
                          DBModo.AceleraFrenaBruscamenteSemaforo = "AceleraFrenaBruscamenteSemaforo",
                          DBModo.CulebreaConFrecuencia = "CulebreaConFrecuencia", 
                          DBModo.OmiteLimiteVelocidad = "OmiteLimiteVelocidad", 
                          DBModo.IgnoraSenhalPare = "IgnoraSenhalPare",
                          DBModo.UsoCelular = "UsoCelular"))

indicators = X[c("CinturonDeSeguridad",
                 "PasoPeatones",
                 "UsaPito", 
                 "FrenoRapidoBrusco",
                 "UsaDireccionales", 
                 "EnfadoConOtroConductor", 
                 "AceleraFrenaBruscamenteSemaforo",
                 "CulebreaConFrecuencia", 
                 "OmiteLimiteVelocidad", 
                 "IgnoraSenhalPare",
                 "UsoCelular")]

indicators[indicators > 3] <- NA

#sink('ModoConduc.txt')

# Estadisticos descriptivos
summary(indicators)

p<- na.omit(indicators)

cor(indicators)
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
print(fa2,cutoff=0.35, sort=FALSE)

sink()

factores <- factanal(na.omit(indicators), factor=4, rotation = "varimax", na.rm = TRUE, scores = "regression")$scores

#Normalizamos los valores que podran tomarse como el peso para el cálculo de un indice que
# explica por cada i-esima fila la variabilidad de todo el conjunto de datos

indicators <- cbind(na.omit(indicators), factores)
indicators$Factor1 <- round(((indicators$Factor1 - min(indicators$Factor1))/(max(indicators$Factor1)-min(indicators$Factor1))),3)
indicators$Factor2 <- round(((indicators$Factor2 - min(indicators$Factor2))/(max(indicators$Factor2)-min(indicators$Factor2))),3)
indicators$Factor3 <- round(((indicators$Factor3 - min(indicators$Factor3))/(max(indicators$Factor3)-min(indicators$Factor3))),3)
indicators$Factor4 <- round(((indicators$Factor4 - min(indicators$Factor4))/(max(indicators$Factor4)-min(indicators$Factor4))),3)

indicators

par(mfrow=c(1,4))
hist(indicators$Factor1, freq = TRUE, main = "Distribución del Factor 1",
     xlab = "Factor 1", ylab = "Frecuencia", col = "4")
hist(indicators$Factor2, freq = TRUE, main = "Distribución del Factor 2",
     xlab = "Factor 2", ylab = "Frecuencia", col = "red")
hist(indicators$Factor3, freq = TRUE, main = "Distribución del Factor 3",
     xlab = "Factor 3", ylab = "Frecuencia", col = "green")
hist(indicators$Factor4, freq = TRUE, main = "Distribución del Factor 4",
     xlab = "Factor 4", ylab = "Frecuencia", col = "yellow")
