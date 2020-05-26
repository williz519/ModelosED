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
  modelName  = "Prospective Simple",
  modelDescr = "Modelos Prospectivo en Eleccion de Ruta",
  indivID    = "ViajeId"
)


# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/1. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)
database$CHOICE2 <-  (ifelse((database$CHOICE == 4),1,2))

names(data)

#summary(database)

# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_real   = 1, asc_sim   = 0,
              b_tg  = 1, b_tl  = 1) 
              

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_sim")



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
  
  alpha=0.88
  beta=0.88
  lamda= 2.25
  
  T_rp = c()
  DT1 = c()
  VP1 = c()
  VN1 = c()
  DT2 = c()
  VP2 = c()
  VN2 = c()
  DT3 = c()
  VP3 = c()
  VN3 = c()
  V1 = c()
  
  for(z in 1:282){
    T_rp[z] = mean(c(TIEMPOAlt1[z],TIEMPOAlt2[z],TIEMPOAlt3[z]))
    
    DT1[z] <- TIEMPOAlt1[z]-T_rp[z]
    
    if(DT1[z]>=0)
    {V1[z] <- -lamda*(DT1[z])**beta}
    else
    {V1[z] <- (-DT1[z])**alpha}
    
    if(DT1[z]>=0)
    {VN1[z] <- -lamda*(DT1[z])**beta}
    else {VN1[z]<- 0}
    if(DT1[z]<0)
    {VP1[z] <- (-DT1[z])**alpha}
    else
    {VP1[z]<- 0}
    
    DT2[z] <- TIEMPOAlt2[z]- T_rp[z]
    if(DT2[z]>=0)
    {VN2[z] <- -lamda*(DT2[z])**beta}
    else {VN2[z]<-0}
    if(DT2[z]<0)
    {VP2[z] <- (-DT2[z])**alpha}
    else
    {VP2[z]<- 0}
    
    DT3[z] <- TIEMPOAlt3[z]-T_rp[z]
    if(DT3[z]>=0)
    {VN3[z] <- -lamda*(DT3[z])**beta}
    else {VN3[z]<-0}
    if(DT3[z]<0)
    {VP3[z] <- (-DT3[z])**beta}
    else
    {VP3[z]<- 0}
  }
  
  
  
  ### Crear una lista de probabilidades P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['real']]  = asc_real + b_tg *(T_rp-TIEMPOEC) + b_tl*(TIEMPOEC-T_rp) 
  
  V[['sim']]  = asc_sim + b_tg*(p_1*VP1 + p_2*VP2 + p_3*VP3) + b_tl*(p_1*VN1 + p_2*VN2 + p_3*VN3)  
  
 
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

