# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

#install.packages("apollo")
### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "Regret Simple",
  modelDescr = "Modelos Regret en Eleccion de Ruta",
  indivID    = "ViajeId"
)


# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/1. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

names(database)


#summary(database)

# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0, asc_ruta2   = 0, asc_ruta3   = 0, asc_rutaEC  = 0,
              b_tt  = 0,  
              b_dt  = 0,
              b_CongAB_ruta1  = 0, b_CongCD_ruta1  = 0, b_CongEF_ruta1  = 0,
              b_CongAB_ruta2  = 0, b_CongCD_ruta2  = 0, b_CongEF_ruta2  = 0,
              b_CongAB_ruta3  = 0, b_CongCD_ruta3  = 0, b_CongEF_ruta3  = 0,
              b_CongAB_rutaEC  = 0, b_CongCD_rutaEC  = 0, b_CongEF_rutaEC  = 0,
              b_Sem_0_ruta1 = 0, b_Sem_0_ruta2 = 0, b_Sem_0_ruta3 = 0, b_Sem_0_rutaEC = 0,
              b_Sem_1_ruta1 = 0, b_Sem_1_ruta2 = 0, b_Sem_1_ruta3 = 0, b_Sem_1_rutaEC = 0, 
              b_Sem_2_ruta1 = 0, b_Sem_2_ruta2 = 0, b_Sem_2_ruta3 = 0, b_Sem_2_rutaEC = 0, 
              b_Sem_3_ruta1 = 0, b_Sem_3_ruta2 = 0, b_Sem_3_ruta3 = 0, b_Sem_3_rutaEC = 0,
              b_Sem_4_ruta1 = 0, b_Sem_4_ruta2 = 0, b_Sem_4_ruta3 = 0, b_Sem_4_rutaEC = 0,
              b_ACC_0_ruta1 = 0, b_ACC_0_ruta2 = 0, b_ACC_0_ruta3 = 0, b_ACC_0_rutaEC = 0,
              b_ACC_1_ruta1 = 0, b_ACC_1_ruta2 = 0, b_ACC_1_ruta3 = 0, b_ACC_1_rutaEC = 0, 
              b_ACC_2_ruta1 = 0, b_ACC_2_ruta2 = 0, b_ACC_2_ruta3 = 0, b_ACC_2_rutaEC = 0)

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta1", "b_CongEF_ruta1", "b_CongEF_ruta2", "b_CongEF_ruta3", "b_CongEF_rutaEC",
                 "b_Sem_4_ruta1", "b_Sem_4_ruta2", "b_Sem_4_ruta3", "b_Sem_4_rutaEC", "b_ACC_2_ruta1",
                 "b_ACC_2_ruta2", "b_ACC_2_ruta3", "b_ACC_2_rutaEC")

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
  
  ### Preparar componentes de arrepentimiento para variables categóricas.
  RSem_ruta1 = b_Sem_0_ruta1*SEMF_A1_0 + b_Sem_1_ruta1*SEMF_A1_1 + b_Sem_2_ruta1*SEMF_A1_2 + 
    b_Sem_3_ruta1*SEMF_A1_3 + b_Sem_4_ruta1*SEMF_A1_4
  
  RSem_ruta2 = b_Sem_0_ruta2*SEMF_A2_0 + b_Sem_1_ruta2*SEMF_A2_1 + b_Sem_2_ruta2*SEMF_A2_2 + 
    b_Sem_3_ruta2*SEMF_A2_3 + b_Sem_4_ruta2*SEMF_A2_4
  
  RSem_ruta3 = b_Sem_0_ruta3*SEMF_A3_0 + b_Sem_1_ruta3*SEMF_A3_1 + b_Sem_2_ruta3*SEMF_A3_2 + 
    b_Sem_3_ruta3*SEMF_A3_3 + b_Sem_4_ruta3*SEMF_A3_4
  
  RSem_rutaEC = b_Sem_0_rutaEC*SEMF_EC_0 + b_Sem_1_rutaEC*SEMF_EC_1 + b_Sem_2_rutaEC*SEMF_EC_2 + 
    b_Sem_3_rutaEC*SEMF_EC_3 + b_Sem_4_rutaEC*SEMF_EC_4
  
  RCong_ruta1 = b_CongAB_ruta1*CONG_AB_A1 + b_CongCD_ruta1*CONG_CD_A1 + b_CongEF_ruta1*CONG_EF_A1
  
  RCong_ruta2 = b_CongAB_ruta2*CONG_AB_A2 + b_CongCD_ruta2*CONG_CD_A2 + b_CongEF_ruta2*CONG_EF_A2
  
  RCong_ruta3 = b_CongAB_ruta3*CONG_AB_A3 + b_CongCD_ruta3*CONG_CD_A3 + b_CongEF_ruta3*CONG_EF_A3
  
  RCong_rutaEC = b_CongAB_rutaEC*CONG_AB_EC + b_CongCD_rutaEC*CONG_CD_EC + b_CongEF_rutaEC*CONG_EF_EC
  
  RAcc_ruta1 = b_ACC_0_ruta1*ACC_A1_0 + b_ACC_1_ruta1*ACC_A1_1 + b_ACC_2_ruta1*ACC_A1_2
  
  RAcc_ruta2 = b_ACC_0_ruta2*ACC_A2_0 + b_ACC_1_ruta2*ACC_A2_1 + b_ACC_2_ruta2*ACC_A2_2
  
  RAcc_ruta3 = b_ACC_0_ruta3*ACC_A3_0 + b_ACC_1_ruta3*ACC_A3_1 + b_ACC_2_ruta3*ACC_A3_2
  
  RAcc_rutaEC = b_ACC_0_rutaEC*ACC_EC_0 + b_ACC_1_rutaEC*ACC_EC_1 + b_ACC_2_rutaEC*ACC_EC_2
  
  
  ### List of regret functions: these must use the same names as in mnl_settings, order is irrelevant
  R = list()
  R[['ruta1']]  = asc_ruta1  + 
    log(1+exp(b_tt*(TIEMPOAlt2 - TIEMPOAlt1))) + 
    log(1+exp(b_tt*(TIEMPOAlt3  - TIEMPOAlt1))) + 
    log(1+exp(b_tt*(TIEMPOEC   - TIEMPOAlt1))) + 
    log(1+exp(b_dt*(DISTAlt2 - DISTAlt1))) + 
    log(1+exp(b_dt*(DISTAlt3 - DISTAlt1))) +
    log(1+exp(b_dt*(DISTEC - DISTAlt1))) +
    log(1+exp(RCong_ruta2  - RCong_ruta1))  +
    log(1+exp(RCong_ruta3  - RCong_ruta1))  +
    log(1+exp(RCong_rutaEC  - RCong_ruta1))  +
    log(1+exp(RSem_ruta2  - RSem_ruta1))  + 
    log(1+exp(RSem_ruta3  - RSem_ruta1))  + 
    log(1+exp(RSem_rutaEC  - RSem_ruta1))  +
    log(1+exp(RAcc_ruta2  - RAcc_ruta1))  +
    log(1+exp(RAcc_ruta3  - RAcc_ruta1))  +
    log(1+exp(RAcc_rutaEC  - RAcc_ruta1))
    
  R[['ruta2']]  = asc_ruta2  + 
    log(1+exp(b_tt*(TIEMPOAlt1 - TIEMPOAlt2))) + 
    log(1+exp(b_tt*(TIEMPOAlt3 - TIEMPOAlt2))) + 
    log(1+exp(b_tt*(TIEMPOEC - TIEMPOAlt2))) + 
    log(1+exp(b_dt*(DISTAlt1 - DISTAlt2))) + 
    log(1+exp(b_dt*(DISTAlt3 - DISTAlt2))) +
    log(1+exp(b_dt*(DISTEC - DISTAlt2))) +
    log(1+exp(RCong_ruta1  - RCong_ruta2))  +
    log(1+exp(RCong_ruta3  - RCong_ruta2))  +
    log(1+exp(RCong_rutaEC  - RCong_ruta2))  +
    log(1+exp(RSem_ruta1  - RSem_ruta2))  + 
    log(1+exp(RSem_ruta3  - RSem_ruta2))  + 
    log(1+exp(RSem_rutaEC  - RSem_ruta2))  +
    log(1+exp(RAcc_ruta1  - RAcc_ruta2))  +
    log(1+exp(RAcc_ruta3  - RAcc_ruta2))  +
    log(1+exp(RAcc_rutaEC  - RAcc_ruta2))
  
  R[['ruta3']]  = asc_ruta3  + 
    log(1+exp(b_tt*(TIEMPOAlt1 - TIEMPOAlt3))) + 
    log(1+exp(b_tt*(TIEMPOAlt2 - TIEMPOAlt3))) + 
    log(1+exp(b_tt*(TIEMPOEC - TIEMPOAlt3))) + 
    log(1+exp(b_dt*(DISTAlt1 - DISTAlt3))) + 
    log(1+exp(b_dt*(DISTAlt2 - DISTAlt3))) +
    log(1+exp(b_dt*(DISTEC - DISTAlt3))) +
    log(1+exp(RCong_ruta2  - RCong_ruta3))  +
    log(1+exp(RCong_ruta1  - RCong_ruta3))  +
    log(1+exp(RCong_rutaEC  - RCong_ruta3))  +
    log(1+exp(RSem_ruta1  - RSem_ruta3))  + 
    log(1+exp(RSem_ruta2  - RSem_ruta3))  + 
    log(1+exp(RSem_rutaEC  - RSem_ruta3))  +
    log(1+exp(RAcc_ruta1  - RAcc_ruta3))  +
    log(1+exp(RAcc_ruta2  - RAcc_ruta3))  +
    log(1+exp(RAcc_rutaEC  - RAcc_ruta3))
  
  R[['rutaEC']]  = asc_rutaEC  + 
    log(1+exp(b_tt*(TIEMPOAlt1  - TIEMPOEC))) + 
    log(1+exp(b_tt*(TIEMPOAlt2  - TIEMPOEC))) + 
    log(1+exp(b_tt*(TIEMPOAlt3   - TIEMPOEC))) + 
    log(1+exp(b_dt*(DISTAlt1    - DISTEC))) + 
    log(1+exp(b_dt*(DISTAlt2    - DISTEC))) +
    log(1+exp(b_dt*(DISTAlt3     - DISTEC))) +
    log(1+exp(RCong_ruta1  - RCong_rutaEC))  +
    log(1+exp(RCong_ruta1  - RCong_rutaEC))  +
    log(1+exp(RCong_ruta3  - RCong_rutaEC))  +
    log(1+exp(RSem_ruta1  - RSem_rutaEC))  + 
    log(1+exp(RSem_ruta2  - RSem_rutaEC))  + 
    log(1+exp(RSem_ruta3  - RSem_rutaEC))  +
    log(1+exp(RAcc_ruta1  - RAcc_rutaEC))  +
    log(1+exp(RAcc_ruta2  - RAcc_rutaEC)) +
    log(1+exp(RAcc_ruta3  - RAcc_rutaEC))
  

  
  ### Define settings for RRM model, which is MNL with negative regret as utility
  mnl_settings <- list(
    alternatives = c(ruta1=1, ruta2=2, ruta3=3, rutaEC=4),
    avail        = list(ruta1=1, ruta2=1, ruta3=1, rutaEC=1),
    choiceVar    = CHOICE,
    V            = lapply(R, "*", -1)
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

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)

