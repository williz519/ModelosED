
# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

## Elimino la ruta 3 debido a que no es significativa en el modelo

### Limpiar memoria
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/Modelos MNL/MNL_Modelo_2/MNL_Modelo_2_2"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "MNL_Modelo_2_2",
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
  database$TIEMPOAlt23[i] = max(database$TIEMPOAlt2[i], database$TIEMPOAlt3[i])
  database$DISTAlt23[i] = max(database$DISTAlt2[i], database$DISTAlt3[i])
}
#database$TIEMPOAlt23
#database$DISTAlt23




# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_Op1   = 0, asc_Op2   = 0, asc_Op3 =0,
              b_tt  = 0,  
              b_dt  = 0,
              b_CongAB  = 0, b_CongCD  = 0, b_CongEF  = 0,
              b_Sem_1 = 0, b_Sem_2 = 0, b_Sem_3 = 0,
              b_ACC_0 = 0, b_ACC_1 = 0, b_ACC_2 = 0,
              b_NO_CAMFD = 0, b_SI_CAMFD = 0, 
              b_NO_PANEL = 0, b_SI_PANEL = 0, 
              b_NO_ZER = 0, b_SI_ZER = 0, 
              b_NO_MTRP = 0, b_SI_MTRP = 0)



### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_Op3","asc_Op1", "b_CongEF", "b_Sem_1", "b_ACC_0", "b_NO_CAMFD", "b_NO_PANEL", 
                 "b_NO_ZER", "b_NO_MTRP")

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
  
  ### Create alternative specific constants and coefficients using interactions with socio-demographics
  #CONG_AB = c1*CONG_AB_A1 + c2*CONG_AB_A2 + c3*CONG_AB_A3
  #CONG_CD = c1*CONG_CD_A1 + c2*CONG_CD_A2 + c3*CONG_CD_A3
  #CONG_EF = c1*CONG_EF_A1 + c2*CONG_EF_A2 + c3*CONG_EF_A3
  
  #SEM_1 = a1*SEMF_A1_1 + a2*SEMF_A2_1 + a3*SEMF_A3_1
  #SEM_2 = a1*SEMF_A1_2 + a2*SEMF_A2_2 + a3*SEMF_A3_2
  #SEM_3 = a1*SEMF_A1_3 + a2*SEMF_A2_3 + a3*SEMF_A3_3
  
  #ACC_0 = ac1*ACC_A1_0 + ac2*ACC_A2_0 + ac3*ACC_A3_0
  #ACC_1 = ac1*ACC_A1_1 + ac2*ACC_A2_1 + ac3*ACC_A3_1
  #ACC_2 = ac1*ACC_A1_2 + ac2*ACC_A2_2 + ac3*ACC_A3_2
  
  #SI_CAMFD = cfd1*SI_CAMFD_A1 + cfd2*SI_CAMFD_A2 + cfd3*SI_CAMFD_A3
  #NO_CAMFD = cfd1*NO_CAMFD_A1 + cfd2*NO_CAMFD_A2 + cfd3*NO_CAMFD_A3
  
  #SI_PANEL = p1*SI_PANEL_A1 + p2*SI_PANEL_A2 + p3*SI_PANEL_A3
  #NO_PANEL = p1*NO_PANEL_A1 + p2*NO_PANEL_A2 + p3*NO_PANEL_A3
  
  #SI_ZER = z1*SI_ZER_A1 + z2*SI_ZER_A2 + z3*SI_ZER_A3
  #NO_ZER = z1*NO_ZER_A1 + z2*NO_ZER_A2 + z3*NO_ZER_A3
  
  #SI_MTRP = m1*SI_MTRP_A1 + m2*SI_MTRP_A2 + m3*SI_MTRP_A3
  #NO_MTRP = m1*NO_MTRP_A1 + m2*NO_MTRP_A2 + m3*NO_MTRP_A3
  
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['Op1']]  = asc_Op1  + b_tt * TIEMPOAlt1+ b_dt * DISTAlt1 + 
    b_CongAB*CONG_AB_A1 + b_CongCD*CONG_CD_A1 + b_CongEF*CONG_EF_A1 +
    b_Sem_1*SEMF_A1_1 + b_Sem_2*SEMF_A1_2 + b_Sem_3*SEMF_A1_3 + 
    b_ACC_0*ACC_A1_0 + b_ACC_1*ACC_A1_1 + b_ACC_2*ACC_A1_2 + 
    b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +
    b_NO_PANEL * NO_PANEL_A1 + b_SI_PANEL * SI_PANEL_A1 + 
    b_NO_ZER * NO_ZER_A1 + b_SI_ZER * SI_ZER_A1 + 
    b_NO_MTRP * NO_MTRP_A1 + b_SI_MTRP * SI_MTRP_A1 
  
  V[['Op2']]  = asc_Op2  + b_tt * TIEMPOAlt23+ b_dt * DISTAlt23 + 
    b_CongAB*(CONG_AB_A2+CONG_AB_A3-CONG_AB_A2*CONG_AB_A3) + 
    b_CongCD*(CONG_CD_A2 + CONG_CD_A3 - CONG_CD_A2*CONG_CD_A3) +
    b_CongEF*(CONG_EF_A2 + CONG_EF_A3 - CONG_EF_A2*CONG_EF_A3) +
    b_Sem_1*(SEMF_A2_1 + SEMF_A3_1 - SEMF_A2_1*SEMF_A3_1) + 
    b_Sem_2*(SEMF_A2_2 + SEMF_A3_2 - SEMF_A2_2*SEMF_A3_2) +
    b_Sem_3*(SEMF_A2_3 + SEMF_A3_3 - SEMF_A2_3*SEMF_A3_3) +
    b_ACC_0* (ACC_A2_0 + ACC_A3_0 - ACC_A2_0*ACC_A2_0) +
    b_ACC_1* (ACC_A2_1 + ACC_A3_1 - ACC_A2_1*ACC_A3_1) +
    b_ACC_2* (ACC_A2_2 + ACC_A3_2 - ACC_A2_2*ACC_A3_2) + 
    b_NO_CAMFD * (NO_CAMFD_A2 + NO_CAMFD_A3 - NO_CAMFD_A2*NO_CAMFD_A3) + 
    b_SI_CAMFD * (SI_CAMFD_A2 + SI_CAMFD_A3 - SI_CAMFD_A2*SI_CAMFD_A3) +
    b_NO_PANEL * (NO_PANEL_A2 + NO_PANEL_A3 - NO_PANEL_A2*NO_PANEL_A3) +
    b_SI_PANEL * (SI_PANEL_A2 + SI_PANEL_A3 - SI_PANEL_A2*SI_PANEL_A3) +
    b_NO_ZER * (NO_ZER_A2 + NO_ZER_A3 - NO_ZER_A2*NO_ZER_A3) +
    b_SI_ZER * (SI_ZER_A2 + SI_ZER_A3 - SI_ZER_A2*SI_ZER_A3) +
    b_NO_MTRP * (NO_MTRP_A2 + NO_MTRP_A3 - NO_MTRP_A2*NO_MTRP_A3) + 
    b_SI_MTRP * (SI_MTRP_A2 + SI_MTRP_A3 - SI_MTRP_A2*SI_MTRP_A3) 
  
  V[['Op3']] = asc_Op3 + b_tt * TIEMPOEC   + b_dt * DISTEC + b_CongAB*CONG_AB_EC + 
    b_CongCD*CONG_CD_EC + b_CongEF*CONG_EF_EC +
    b_Sem_1*SEMF_EC_1 + b_Sem_2*SEMF_EC_2 + b_Sem_3*SEMF_EC_3 + 
    b_ACC_0*ACC_EC_0 + b_ACC_1*ACC_EC_1 + b_ACC_2*ACC_EC_2 + 
    b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +
    b_NO_PANEL * NO_PANEL_EC + b_SI_PANEL * SI_PANEL_EC + 
    b_NO_ZER * NO_ZER_EC + b_SI_ZER * SI_ZER_EC + b_NO_MTRP * NO_MTRP_EC +
    b_SI_MTRP * SI_MTRP_EC 
  
  
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


