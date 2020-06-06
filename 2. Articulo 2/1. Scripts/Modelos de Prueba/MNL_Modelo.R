
# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

## Elimino la ruta 3 debido a que no es significativa en el modelo

### Limpiar memoria
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/Modelos de Prueba/MNL Basico"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "MNL_Modelo",
  modelDescr = "Modelo MNL de solo dos elecciones ",
  indivID    = "ViajeId",
  nCores     = 1
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

#table(database$CHOICE)

#database <-  database %>%
  # Filtrar viajes Eliminados en la primera revisión
#  filter(!(CHOICE %in% c("3")))

#Reemplazar la ruta 3 en la ruta 2
database$CHOICE[database$CHOICE == 3 ]<-2
database$CHOICE[database$CHOICE == 4]<- 3

#table(database$CHOICE)

#names(database)

### Create new variable with time

for (i in 1:nrow(database)) {
  database$TIEMPOAlt23[i] = (database$TIEMPOAlt2[i]+ database$TIEMPOAlt3[i])/2
  database$DISTAlt23[i] = min(database$DISTAlt2[i],database$DISTAlt3[i])
}
#database$TIEMPOAlt23
database$DISTAlt23

database$DISTAlt2
database$DISTAlt3


# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_Op1   = 0, asc_Op2   = 0, asc_Op3 =0,
              b_tt  = 0,  
              b_dt  = 0,
              b_CongAB  = 0, b_CongCD  = 0, b_CongEF  = 0,
              b_Sem = 0,
              b_ACC_0 = 0, b_ACC_1 = 0, b_ACC_2 = 0,
              b_NO_CAMFD = 0, b_SI_CAMFD = 0, 
              b_PANEL0 = 0, b_PANEL1 = 0,  
              b_ZER0 = 0, b_ZER1 = 0
              )



### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_Op1","asc_Op3", "b_CongEF", "b_ACC_0", "b_ZER0","b_NO_CAMFD", "b_PANEL0")

# ################################################################# #
#### ENTRADAS DE GRUPO Y VALIDACIÓN                                ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  

  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['Op1']]  = asc_Op1  + b_tt * TIEMPOAlt1+ b_dt * DISTAlt1 + 
    b_CongAB*CONG_AB_A1 + b_CongCD*CONG_CD_A1 + b_CongEF*CONG_EF_A1 +
    b_Sem*SEM_A1 + 
    b_ACC_0*ACC_A1_0 + b_ACC_1*ACC_A1_1 + b_ACC_2*ACC_A1_2 + 
    b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +
    b_PANEL0 * (Panel_A1<=0.3) + b_PANEL1* (Panel_A1>0.3) + 
    b_ZER0 * (ZER_A1_km<=0.1) +
    b_ZER1 * (ZER_A1_km>0.1) 
  
  V[['Op2']]  = asc_Op2  + b_tt * TIEMPOAlt23+ b_dt * DISTAlt23 + 
    b_CongAB*(CONG_AB_A2+CONG_AB_A3-CONG_AB_A2*CONG_AB_A3) + 
    b_CongCD*(CONG_CD_A2 + CONG_CD_A3 - CONG_CD_A2*CONG_CD_A3) +
    b_CongEF*(CONG_EF_A2 + CONG_EF_A3 - CONG_EF_A2*CONG_EF_A3) +
    b_Sem*((SEM_A2 + SEM_A3)/2) +
    b_ACC_0* (ACC_A2_0 + ACC_A3_0 - ACC_A2_0*ACC_A2_0) +
    b_ACC_1* (ACC_A2_1 + ACC_A3_1 - ACC_A2_1*ACC_A3_1) +
    b_ACC_2* (ACC_A2_2 + ACC_A3_2 - ACC_A2_2*ACC_A3_2) + 
    b_NO_CAMFD * (NO_CAMFD_A2 + NO_CAMFD_A3 - NO_CAMFD_A2*NO_CAMFD_A3) + 
    b_SI_CAMFD * (SI_CAMFD_A2 + SI_CAMFD_A3 - SI_CAMFD_A2*SI_CAMFD_A3) +
    b_PANEL0 * (((Panel_A2 + Panel_A3)/2)<=0.3) + b_PANEL1* (((Panel_A2 + Panel_A3)/2)>0.3) +
    b_ZER0 * (((ZER_A2_km + ZER_A3_km)/2)<=0.1)  + 
    b_ZER1 * (((ZER_A2_km + ZER_A3_km)/2)>0.1) 
    
  
  V[['Op3']] = asc_Op3 + b_tt * TIEMPOEC   + b_dt * DISTEC + b_CongAB*CONG_AB_EC + 
    b_CongCD*CONG_CD_EC + b_CongEF*CONG_EF_EC +
    b_Sem*SEM_EC + 
    b_ACC_0*ACC_EC_0 + b_ACC_1*ACC_EC_1 + b_ACC_2*ACC_EC_2 + 
    b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +
    b_PANEL0 * (Panel_EC<=0.3) + b_PANEL1* (Panel_EC>0.3) +
    b_ZER0 * (ZER_EC_km<=0.1) + b_ZER1*(ZER_EC_km >0.1)  
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(Op1=1, Op2=2, Op3=3), 
    avail         = list(Op1=1, Op2=1, Op3=1), 
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
#### ESTIMACION DEL MODELO                                       ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
                        estimate_settings = list(maxIterations = 700))

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

predictions_base = apollo_prediction(model, 
                                     apollo_probabilities, 
                                     apollo_inputs)


