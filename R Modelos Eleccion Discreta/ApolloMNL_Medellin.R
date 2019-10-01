
# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

### Limpiar memoria
rm(list = ls())

### Cargar libreria de  Apollo
library(apollo)

### Código de inicialización
apollo_initialise()

### Establecer controles principales
apollo_control = list(
  modelName  ="Apollo_MNL_1",
  modelDescr ="Modelo MNL simple en datos RP de elección de ruta",
  indivID    ="ViajeId"
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR ALGUNA TRANSFORMACIÓN                ####
# ################################################################# #

database = read.csv("apollo_modeChoiceData.csv",header=TRUE)

### Use only RP data
database = subset(database,database$RP==1)

### Create new variable with average income
database$mean_income = mean(database$income)
