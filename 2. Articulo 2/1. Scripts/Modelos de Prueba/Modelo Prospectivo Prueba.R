
# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

#install.packages("apollo")
### Clear memory
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/Modelos de Prueba/Prospectivo"
setwd(workingDirectory)

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "Prospectivo Simple",
  modelDescr = "Modelos Prospectivo en Eleccion de Ruta",
  indivID    = "ViajeId"
)


# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

# Normalización de las variables tiempo

database$T_Alt_1 = (database$TIEMPOAlt1- ((database$TIEMPOAlt1+database$TIEMPOAlt2+database$TIEMPOAlt3+database$TIEMPOEC)/4))/(sd(c(database$TIEMPOAlt1,database$TIEMPOAlt2,database$TIEMPOAlt3,database$TIEMPOEC)))
database$T_Alt_2 = (database$TIEMPOAlt2- ((database$TIEMPOAlt1+database$TIEMPOAlt2+database$TIEMPOAlt3+database$TIEMPOEC)/4))/(sd(c(database$TIEMPOAlt1,database$TIEMPOAlt2,database$TIEMPOAlt3,database$TIEMPOEC)))
database$T_Alt_3 = (database$TIEMPOAlt3- ((database$TIEMPOAlt1+database$TIEMPOAlt2+database$TIEMPOAlt3+database$TIEMPOEC)/4))/(sd(c(database$TIEMPOAlt1,database$TIEMPOAlt2,database$TIEMPOAlt3,database$TIEMPOEC)))
database$T_Alt_4 = (database$TIEMPOEC- ((database$TIEMPOAlt1+database$TIEMPOAlt2+database$TIEMPOAlt3+database$TIEMPOEC)/4))/(sd(c(database$TIEMPOAlt1,database$TIEMPOAlt2,database$TIEMPOAlt3,database$TIEMPOEC)))


# Normalización de la variable distancia
database$D_Alt_1 = (database$DISTAlt1- ((database$DISTAlt1+database$DISTAlt2+database$DISTAlt3+database$DISTEC)/4))/(sd(c(database$DISTAlt1+database$DISTAlt2+database$DISTAlt3+database$DISTEC)))
database$D_Alt_2 = (database$DISTAlt2- ((database$DISTAlt1+database$DISTAlt2+database$DISTAlt3+database$DISTEC)/4))/(sd(c(database$DISTAlt1+database$DISTAlt2+database$DISTAlt3+database$DISTEC)))
database$D_Alt_3 = (database$DISTAlt3- ((database$DISTAlt1+database$DISTAlt2+database$DISTAlt3+database$DISTEC)/4))/(sd(c(database$DISTAlt1+database$DISTAlt2+database$DISTAlt3+database$DISTEC)))
database$D_Alt_4 = (database$DISTEC- ((database$DISTAlt1+database$DISTAlt2+database$DISTAlt3+database$DISTEC)/4))/(sd(c(database$DISTAlt1+database$DISTAlt2+database$DISTAlt3+database$DISTEC)))



#Tiempos de Referencia
database$T_rp = (database$T_Alt_1+database$T_Alt_2+database$T_Alt_3)/3
database$D_rp = (database$D_Alt_1+database$D_Alt_2+database$D_Alt_3)/3

alpha=0.88
beta=0.88
lamda= 2.25

for(z in 1:nrow(database)){
  database$DT1[z] <- database$T_Alt_4[z]-database$T_Alt_1[z]
  if(database$DT1[z]>=0)
  {database$VN1[z] <- -lamda*(database$DT1[z])**beta}
  else {database$VN1[z]<- 0}
  if(database$DT1[z]<0)
  {database$VP1[z] <- (-database$DT1[z])**alpha}
  else
  {database$VP1[z]<- 0}
  
  database$DT2[z] <- database$T_Alt_4[z]- database$T_Alt_2[z]
  if(database$DT2[z]>=0)
  {database$VN2[z] <- -lamda*(database$DT2[z])**beta}
  else {database$VN2[z]<-0}
  if(database$DT2[z]<0)
  {database$VP2[z] <- (-database$DT2[z])**alpha}
  else
  {database$VP2[z]<- 0}
  
  database$DT3[z] <- database$T_Alt_4[z]-database$T_Alt_3[z]
  if(database$DT3[z]>=0)
  {database$VN3[z] <- -lamda*(database$DT3[z])**beta}
  else {database$VN3[z]<-0}
  if(database$DT3[z]<0)
  {database$VP3[z] <- (-database$DT3[z])**beta}
  else
  {database$VP3[z]<- 0}
  
  database$DT4[z] <- database$T_Alt_4[z]-database$T_rp[z]
  if(database$DT4[z]>=0)
  {database$VN4[z] <- -lamda*(database$DT4[z])**beta}
  else {database$VN4[z]<-0}
  if(database$DT4[z]<0)
  {database$VP4[z] <- (-database$DT4[z])**beta}
  else
  {database$VP4[z]<- 0}
}



# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0, asc_ruta2   = 0, asc_ruta3   = 0, asc_ruta4  = 0,
              b_tg  = 0, b_tl  = 0, 
              b_dt  = 0)


### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta3")




# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()



# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  
  ### Crear una lista de probabilidades P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['ruta1']]  = asc_ruta1  + b_tg * VP1 + b_tl * VN1 + b_dt * D_Alt_1 
  
  V[['ruta2']]  = asc_ruta2  + b_tg * VP2 + b_tl * VN2 + b_dt * D_Alt_2
  
  V[['ruta3']]  = asc_ruta3  + b_tg * VP3 + b_tl * VN3 + b_dt * D_Alt_3
  
  V[['rutaEC']] = asc_ruta4 + b_tg * VP4 + b_tl * VN4 + b_dt * D_Alt_4 
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(ruta1=1, ruta2=2, ruta3=3, rutaEC=4), 
    avail         = list(ruta1=1, ruta2=1, ruta3=1, rutaEC=1), 
    choiceVar     = CHOICE,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model, modelOutput_settings=list(printPVal=TRUE) )


# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model, saveOutput_settings=list(printPVal=TRUE) )
