#Installing the Psych package and loading it
#install.packages("psych")
#install_github('likert', 'jbryer')
require(devtools)
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
library(umx)
library(likert)

#Leer la dataset 

#Leer los dataset Personalidad y Modo Conduccion

# Analisis Factorial con datos escalados a una escala likert 1 - 5

#DBModLog <- read.csv("/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVLCE.csv", header = TRUE, sep = "\t")

# Base de datos de escala likert diferentes
DBModLog <- read.csv("/Users/williz/Desktop/ModelosED/Database/ModeloLogitVL.csv", header = TRUE, sep = "\t")


names(DBModLog)

DB1 <- select(DBModLog, -("ViajeId":"CHOICE"))
names(DB1)

DBPer <- select(DB1, c("ComVrb","Ans","ComAfec","PrPer","AmbTr","StrC","ConCl","EnfCond"))
names(DBPer)

DBMod <- select(DB1, c("Experiencia","PasoPeaton","UsoPito","CinSeg","FRbr",
                       "UsoDirec","AFrSem","CulFr","OmLmVel","IgPare","UsoCel","DispMob") )
names(DBMod)

l1<- likert(DBMod)

DBConjunta <- tibble(DBPer,DBMod)


#Alpha de Cronbach

#Personalidad
cov(DBPer)
reliability(cov(DBPer))

DBPer$Suma <- DBPer$ComVrb+DBPer$Ans+DBPer$ComAfec+DBPer$PrPer+DBPer$AmbTr+
  DBPer$StrC+DBPer$ConCl+DBPer$EnfCond

cor(DBPer)

#Modo
cov(DBMod)
reliability(cov(DBMod))

DBMod$Suma <- DBMod$Experiencia+DBMod$CinSeg+DBMod$FRbr+DBMod$UsoDirec+DBMod$UsoPito+DBMod$AFrSem+
  DBMod$CulFr+DBMod$OmLmVel+DBMod$IgPare+DBMod$UsoCel+DBMod$PasoPeaton+DBMod$DispMob

cor(DBMod)
DBMod$OmLmVel[DBMod$OmLmVel == 1]<-4
DBMod$OmLmVel[DBMod$OmLmVel == 3]<-1
DBMod$OmLmVel[DBMod$OmLmVel == 4]<-3

DBMod[c("Suma")] <- NULL

alfa <- alpha(DBMod)


summary(DBConjunta)

#sink("AnalisisFactorialConjuntoCE.txt")

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
colnames(A) <- colnames(DBConjunta)
rownames(A) <- colnames(DBConjunta)
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

# Alpha de Cronbach
# Alfa >0.9 Excelente
# Alfa > 0.8 Bueno
# Alfa > 0.6 Cuestionable
# Alfa > 0.5 Pobre
# Alfa < 0.5 Inaceptable

DBConj <- data.matrix(DBConjunta)


cor(DB4)
cor(DB5)

str(DB5$CinSeg)
DB5$CinSeg[DB5$CinSeg == 1] <- 3
DB5$CinSeg[DB5$CinSeg == 2] <- 1
DB5$CinSeg[DB5$CinSeg == 3] <- 1

reliability(cov(DB5))


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
print(fa,cutoff=0.30, sort=FALSE)

fa2 <-factanal(DBConjunta, factor=4, rotation = "varimax", na.rm = TRUE)
print(fa2,cutoff=0.3, sort=FALSE)



factores <- factanal(DBConjunta, factor=4, rotation = "varimax", na.rm = TRUE, scores = "regression")$scores
print(factores,cutoff=0.35, sort=FALSE)

#Normalizamos los valores que podran tomarse como el peso para el cálculo de un indice que
# explica por cada i-esima fila la variabilidad de todo el conjunto de datos
names(DB1)

DBFAC <- cbind(DB1, factores)

DBFAC$Factor1 <- round(((DBFAC$Factor1 - min(DBFAC$Factor1))/(max(DBFAC$Factor1)-min(DBFAC$Factor1))),5)
DBFAC$Factor2 <- round(((DBFAC$Factor2 - min(DBFAC$Factor2))/(max(DBFAC$Factor2)-min(DBFAC$Factor2))),5)
DBFAC$Factor3 <- round(((DBFAC$Factor3 - min(DBFAC$Factor3))/(max(DBFAC$Factor3)-min(DBFAC$Factor3))),5)
DBFAC$Factor4 <- round(((DBFAC$Factor4 - min(DBFAC$Factor4))/(max(DBFAC$Factor4)-min(DBFAC$Factor4))),5)


DBFAC

DBFAC <- rename(DBFAC, replace = c(Factor1 = "CondAgres",
                                   Factor2 = "AmbienteLaboral",
                                   Factor3 = "HabProsoc",
                                   Factor4 = "Stress"))

names(DBFAC)



#GRAFICOS
par(mfrow=c(2,2))

hist(DBFAC$CondAgres, freq = TRUE, main = "Distr del Factor 1",
     xlab = "Cond Agresiva", ylab = "Frecuencia", col = "red")
hist(DBFAC$AmbienteLaboral, freq = TRUE, main = "Distr del Factor 2",
     xlab = "Ambiente Laboral", ylab = "Frecuencia", col = "green")
hist(DBFAC$HabProsoc, freq = TRUE, main = "Distr del Factor 3",
     xlab = "Hab Prosociales", ylab = "Frecuencia", col = "blue")
hist(DBFAC$Stress, freq = TRUE, main = "Distr del Factor 4",
     xlab = "Stress al Cond", ylab = "Frecuencia", col = "red")


#sink()


write.table(DBFAC, 
            file="/Users/williz/Desktop/ModelosED/Database/DataBaseVLCE.csv", sep="\t", dec=".")



DBMuestra <- DBFAC %>%
  # Filtrar 54 viajes para validacion del modelo Logit
  filter(!(ViajeId %in% c("{F6DF6FDC-3F7A-E811-B124-74867AD5B714}",
                          "{0B431AE3-7DBE-E811-914C-74867AD5B714}",
                          "{708DBE38-3DC0-E811-914C-74867AD5B714}",
                          "{04857B76-DDC0-E811-914C-74867AD5B714}",
                          "{0E557454-B5C1-E811-914C-74867AD5B714}",
                          "{EF25C87F-90C2-E811-914C-74867AD5B714}",
                          "{5528961E-D7C5-E811-914C-74867AD5B714}",
                          "{CD4C129D-8BC6-E811-914C-74867AD5B714}",
                          "{7FD68364-DEC7-E811-914C-74867AD5B714}",
                          "{56FF6CE7-E9C8-E811-914C-74867AD5B714}",
                          "{8549CD86-9DC9-E811-914C-74867AD5B714}",
                          "{7F5B9070-42CB-E811-914C-74867AD5B714}",
                          "{60D70C69-4FCB-E811-914C-74867AD5B714}",
                          "{29758F97-06CC-E811-914C-74867AD5B714}",
                          "{A91D3981-1ACC-E811-914C-74867AD5B714}",
                          "{F77E8A2C-97CC-E811-914C-74867AD5B714}",
                          "{15FA02A6-ADCC-E811-914C-74867AD5B714}",
                          "{4A9D79A1-82CD-E811-914C-74867AD5B714}",
                          "{93260574-3CCE-E811-914C-74867AD5B714}",
                          "{72BC66FC-06D2-E811-8FB7-74867AD5B714}",
                          "{09CAC4F7-D8D2-E811-8FB7-74867AD5B714}",
                          "{D72D0A96-9CD3-E811-8FB7-74867AD5B714}",
                          "{A0CE2445-6AD4-E811-8FB7-74867AD5B714}",
                          "{2A8FA3E1-FFD5-E811-8FB7-74867AD5B714}",
                          "{D918DCC0-1AD7-E811-8FB7-74867AD5B714}",
                          "{4FDFB32F-B0D7-E811-8FB7-74867AD5B714}",
                          "{B687E395-CFDB-E811-8FB7-74867AD5B714}",
                          "{A83D77D6-44DC-E811-8FB7-74867AD5B714}",
                          "{A9DAA2D0-CDDE-E811-8FB7-74867AD5B714}",
                          "{0C1D8C2A-12E2-E811-8FB7-74867AD5B714}",
                          "{DFA93A62-93E2-E811-8FB7-74867AD5B714}",
                          "{D9845004-43E4-E811-8FB7-74867AD5B714}",
                          "{0D4FE76E-4AE4-E811-8FB7-74867AD5B714}",
                          "{3CFD12ED-BAE6-E811-8FB7-74867AD5B714}",
                          "{08C503A7-58E8-E811-8FB7-74867AD5B714}",
                          "{FDC01CA6-F4E8-E811-8FB7-74867AD5B714}",
                          "{BB9B2829-A0E9-E811-8FB7-74867AD5B714}",
                          "{707EA9A3-FEEB-E811-8FB7-74867AD5B714}",
                          "{6C4E58BF-A7E3-E811-8FB7-74867AD5B714}",
                          "{C0E06776-9DE2-E811-8FB7-74867AD5B714}",
                          "{ABA9B7BC-06E2-E811-8FB7-74867AD5B714}",
                          "{653E753C-29DE-E811-8FB7-74867AD5B714}",
                          "{AA6A7427-7FDC-E811-8FB7-74867AD5B714}",
                          "{BE04651F-E1D7-E811-8FB7-74867AD5B714}",
                          "{B286DAF7-02D6-E811-8FB7-74867AD5B714}",
                          "{54F72912-A0D4-E811-8FB7-74867AD5B714}",
                          "{0C97D2B2-DCD3-E811-8FB7-74867AD5B714}",
                          "{6113CA0C-0CD2-E811-8FB7-74867AD5B714}",
                          "{0C1FF5E1-42CE-E811-914C-74867AD5B714}",
                          "{BD29DDF5-27DA-E811-8FB7-74867AD5B714}",
                          "{6DB46E7C-DFDE-E811-8FB7-74867AD5B714}",
                          "{1DA55572-BD69-E811-95E4-74867AD5B714}",
                          "{968C85AE-09B2-E811-BDF0-74867AD5B714}",
                          "{AE8170B6-E6C7-E811-914C-74867AD5B714}")))


DBMuestra <- DBMuestra[ ,!colnames(DBMuestra)=="ViajeId"]
names(DBMuestra)
DBMuestra <-select(DBMuestra, -("SatDispMob"),-("PasoPeatones"),
                   -("UsaPito"))

view(DBMuestra)

#Cambio de Escala a Congestion
#DBMuestra$CONGESTION[DBMuestra$CONGESTION == 1 | DBMuestra$CONGESTION == 2 ] <- 1
#DBMuestra$CONGESTION[DBMuestra$CONGESTION == 3 | DBMuestra$CONGESTION == 4 ] <- 2
#DBMuestra$CONGESTION[DBMuestra$CONGESTION == 5 | DBMuestra$CONGESTION == 6 ] <- 3
#DBMuestra$CONGESTION[DBMuestra$CONGESTION == 7 ] <- 4





write.table(DBMuestra, 
            file="/Users/williz/Desktop/ModelosED/Database/DBModLogitMuestraVLCE.csv", sep="\t", dec=".")





