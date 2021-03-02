# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

#install.packages("apollo")
### Clear memory
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/1. Articulo 2/1. Scripts/Prospectiva"
setwd(workingDirectory)

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "EUT Simple",
  modelDescr = "Modelos EUT en Eleccion de Ruta",
  indivID    = "ViajeId"
)


# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/1. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)
database$CHOICE2 <-  (ifelse((database$CHOICE == 4),1,2))

names(database)

#summary(database)

# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_real  = 0, asc_sim   = 0,
              b_t  = 0, b_d  = 0) 

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_real")



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
  
  p_1 = 0.6178
  p_2 = 0.2473
  p_3 = 0.1349
  
  ### Crear una lista de probabilidades P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['real']]  = asc_real + b_t*TIEMPOEC + b_d*DISTEC 
  
  V[['sim']]  = asc_sim + b_t*(p_1*TIEMPOAlt1+p_2*TIEMPOAlt2+p_3*TIEMPOAlt3) +
    b_d*(p_1*DISTAlt1+p_2*DISTAlt2+p_3*DISTAlt3)
  
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(real=1, sim=2), 
    avail         = list(real=1, sim=1), 
    choiceVar     = CHOICE2,
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

