# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

#install.packages("apollo")
### Clear memory
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/Modelos de Prueba/Regret"
setwd(workingDirectory)



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

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)


for (i in 1:nrow(database)){
  # Normalización de las variables tiempo
  database$T_Alt_1[i] = (database$TIEMPOAlt1[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5))
  database$T_Alt_2[i] = (database$TIEMPOAlt2[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5))
  database$T_Alt_3[i] = (database$TIEMPOAlt3[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5))
  database$T_Alt_4[i] = (database$TIEMPOEC[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.5))
  
  # Normalización de la variable distancia
  database$D_Alt_1[i] = (database$DISTAlt1[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_2[i] = (database$DISTAlt2[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_3[i] = (database$DISTAlt3[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_4[i] = (database$DISTEC[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
}


#summary(database)

# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0, asc_ruta2   = 0, asc_ruta3   = 0, 
              b_tt  = 0,  
              b_dt  = 0,
              b_Sem = 0, 
              b_CongAB  = 0, b_CongCD  = 0, b_CongEF  = 0,
              b_ACC_0 = 0, b_ACC_1 = 0, b_ACC_2 = 0,
              b_NO_CAMFD = 0,  b_SI_CAMFD = 0, 
              b_NO_PANEL = 0,  b_SI_PANEL = 0,
              b_NO_ZER = 0,  b_SI_ZER = 0,
              b_No_MTRP = 0,  b_Si_MTRP = 0)
              

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta3", "b_CongAB", "b_ACC_0", "b_NO_CAMFD", "b_No_MTRP", "b_NO_PANEL", "b_NO_ZER")


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
  RSem_ruta1 = SEM_A1_km
  RSem_ruta2 = SEM_A2_km
  RSem_ruta3 = SEM_A3_km
  RSem_ruta4 = SEM_EC_km
  
  RCong_ruta1 = b_CongAB*CONG_AB_A1 + b_CongCD*CONG_CD_A1 + b_CongEF*CONG_EF_A1
  RCong_ruta2 = b_CongAB*CONG_AB_A2 + b_CongCD*CONG_CD_A2 + b_CongEF*CONG_EF_A2
  RCong_ruta3 = b_CongAB*CONG_AB_A3 + b_CongCD*CONG_CD_A3 + b_CongEF*CONG_EF_A3
  RCong_ruta4 = b_CongAB*CONG_AB_EC + b_CongCD*CONG_CD_EC + b_CongEF*CONG_EF_EC
  
  RAcc_ruta1 = b_ACC_0*ACC_A1_0 + b_ACC_1*ACC_A1_1 + b_ACC_2*ACC_A1_2
  RAcc_ruta2 = b_ACC_0*ACC_A2_0 + b_ACC_1*ACC_A2_1 + b_ACC_2*ACC_A2_2
  RAcc_ruta3 = b_ACC_0*ACC_A3_0 + b_ACC_1*ACC_A3_1 + b_ACC_2*ACC_A3_2
  RAcc_ruta4 = b_ACC_0*ACC_EC_0 + b_ACC_1*ACC_EC_1 + b_ACC_2*ACC_EC_2
  
  RCamFd_ruta1 = b_NO_CAMFD*NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1
  RCamFd_ruta2 = b_NO_CAMFD*NO_CAMFD_A2 + b_SI_CAMFD * SI_CAMFD_A2
  RCamFd_ruta3 = b_NO_CAMFD*NO_CAMFD_A3 + b_SI_CAMFD * SI_CAMFD_A3
  RCamFd_ruta4 = b_NO_CAMFD*NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC
  
  RPanel_ruta1 = b_NO_PANEL*NO_PANEL_A1 + b_SI_PANEL*SI_PANEL_A1
  RPanel_ruta2 = b_NO_PANEL*NO_PANEL_A2 + b_SI_PANEL*SI_PANEL_A2
  RPanel_ruta3 = b_NO_PANEL*NO_PANEL_A3 + b_SI_PANEL*SI_PANEL_A3
  RPanel_ruta4 = b_NO_PANEL*NO_PANEL_EC + b_SI_PANEL*SI_PANEL_EC
  
  RZer_r1 = b_NO_ZER* NO_ZER_A1 + b_SI_ZER *SI_ZER_A1
  RZer_r2 = b_NO_ZER* NO_ZER_A2 + b_SI_ZER *SI_ZER_A2
  RZer_r3 = b_NO_ZER* NO_ZER_A3 + b_SI_ZER *SI_ZER_A3
  RZer_r4 = b_NO_ZER* NO_ZER_EC + b_SI_ZER *SI_ZER_EC
  
  RMtrp_r1 = b_No_MTRP *NO_MTRP_A1 + b_Si_MTRP * SI_MTRP_A1
  RMtrp_r2 = b_No_MTRP *NO_MTRP_A2 + b_Si_MTRP * SI_MTRP_A2
  RMtrp_r3 = b_No_MTRP *NO_MTRP_A3 + b_Si_MTRP * SI_MTRP_A3
  RMtrp_r4 = b_No_MTRP *NO_MTRP_EC + b_Si_MTRP * SI_MTRP_EC
  
  
  ### List of regret functions: these must use the same names as in mnl_settings, order is irrelevant
  R = list()
  R[['ruta1']]  = asc_ruta1  + 
    #log(1+exp(b_tt*(T_Alt_2 - T_Alt_1))) + 
    #log(1+exp(b_tt*(T_Alt_3  - T_Alt_1))) + 
    #log(1+exp(b_tt*(T_Alt_4   - T_Alt_1))) + 
    #log(1+exp(b_dt*(D_Alt_2 - D_Alt_1))) + 
    #log(1+exp(b_dt*(D_Alt_3 - D_Alt_1))) +
    #log(1+exp(b_dt*(D_Alt_4 - D_Alt_1))) +
    log(1+exp(b_Sem*(RSem_ruta2  - RSem_ruta1)))  + 
    log(1+exp(b_Sem*(RSem_ruta3  - RSem_ruta1)))  + 
    log(1+exp(b_Sem*(RSem_ruta4  - RSem_ruta1)))  +
    log(1+exp(RCong_ruta2  - RCong_ruta1))  +
    log(1+exp(RCong_ruta3  - RCong_ruta1))  +
    log(1+exp(RCong_ruta4  - RCong_ruta1))  +
    log(1+exp(RAcc_ruta2  - RAcc_ruta1))  +
    log(1+exp(RAcc_ruta3  - RAcc_ruta1))  +
    log(1+exp(RAcc_ruta4  - RAcc_ruta1)) +
    log(1+exp(RCamFd_ruta2 - RCamFd_ruta1)) +
    log(1+exp(RCamFd_ruta3 - RCamFd_ruta1)) +
    log(1+exp(RCamFd_ruta4 - RCamFd_ruta1)) +
    log(1+exp(RPanel_ruta2 - RPanel_ruta1)) +
    log(1+exp(RPanel_ruta3 - RPanel_ruta1)) +
    log(1+exp(RPanel_ruta4 - RPanel_ruta1)) +
    log(1+exp(RZer_r2 - RZer_r1)) +
    log(1+exp(RZer_r3 - RZer_r1)) +
    log(1+exp(RZer_r4 - RZer_r1)) +
    log(1+exp(RMtrp_r2 - RMtrp_r1)) +
    log(1+exp(RMtrp_r3 - RMtrp_r1)) +
    log(1+exp(RMtrp_r4 - RMtrp_r1))
    
  R[['ruta2']]  = asc_ruta2  + 
    #log(1+exp(b_tt*(T_Alt_1 - T_Alt_2))) + 
    #log(1+exp(b_tt*(T_Alt_3 - T_Alt_2))) + 
    #log(1+exp(b_tt*(T_Alt_4 - T_Alt_2))) + 
    #log(1+exp(b_dt*(D_Alt_1 - D_Alt_2))) + 
    #log(1+exp(b_dt*(D_Alt_3 - D_Alt_2))) +
    #log(1+exp(b_dt*(D_Alt_4 - D_Alt_2))) +
    log(1+exp(b_Sem*(RSem_ruta1  - RSem_ruta2)))  + 
    log(1+exp(b_Sem*(RSem_ruta3  - RSem_ruta2)))  + 
    log(1+exp(b_Sem*(RSem_ruta4  - RSem_ruta2)))  +
    log(1+exp(RCong_ruta1  - RCong_ruta2))  +
    log(1+exp(RCong_ruta3  - RCong_ruta2))  +
    log(1+exp(RCong_ruta4  - RCong_ruta2))  +
    log(1+exp(RAcc_ruta1  - RAcc_ruta2))  +
    log(1+exp(RAcc_ruta3  - RAcc_ruta2))  +
    log(1+exp(RAcc_ruta4  - RAcc_ruta2)) +
    log(1+exp(RCamFd_ruta1 - RCamFd_ruta2)) +
    log(1+exp(RCamFd_ruta3 - RCamFd_ruta2)) +
    log(1+exp(RCamFd_ruta4 - RCamFd_ruta2)) +
    log(1+exp(RPanel_ruta1 - RPanel_ruta2)) +
    log(1+exp(RPanel_ruta3 - RPanel_ruta2)) +
    log(1+exp(RPanel_ruta4 - RPanel_ruta2)) +
    log(1+exp(RZer_r1 - RZer_r2)) +
    log(1+exp(RZer_r3 - RZer_r2)) +
    log(1+exp(RZer_r4 - RZer_r2)) +
    log(1+exp(RMtrp_r1 - RMtrp_r2)) +
    log(1+exp(RMtrp_r3 - RMtrp_r2)) +
    log(1+exp(RMtrp_r4 - RMtrp_r2))
  
    
  R[['ruta3']]  = asc_ruta3  + 
    #log(1+exp(b_tt*(T_Alt_1 - T_Alt_3))) + 
    #log(1+exp(b_tt*(T_Alt_2 - T_Alt_3))) + 
    #log(1+exp(b_tt*(T_Alt_4 - T_Alt_3))) + 
    #log(1+exp(b_dt*(D_Alt_1 - D_Alt_3))) + 
    #log(1+exp(b_dt*(D_Alt_2 - D_Alt_3))) +
    #log(1+exp(b_dt*(D_Alt_4 - D_Alt_3))) +
    log(1+exp(b_Sem*(RSem_ruta1  - RSem_ruta3)))  + 
    log(1+exp(b_Sem*(RSem_ruta2  - RSem_ruta3)))  + 
    log(1+exp(b_Sem*(RSem_ruta4  - RSem_ruta3)))  +
    log(1+exp(RCong_ruta2  - RCong_ruta3))  +
    log(1+exp(RCong_ruta1  - RCong_ruta3))  +
    log(1+exp(RCong_ruta4  - RCong_ruta3))  +
    log(1+exp(RAcc_ruta1  - RAcc_ruta3))  +
    log(1+exp(RAcc_ruta2  - RAcc_ruta3))  +
    log(1+exp(RAcc_ruta4  - RAcc_ruta3)) +
    log(1+exp(RCamFd_ruta1 - RCamFd_ruta3)) +
    log(1+exp(RCamFd_ruta2 - RCamFd_ruta3)) +
    log(1+exp(RCamFd_ruta4 - RCamFd_ruta3)) +
    log(1+exp(RPanel_ruta1 - RPanel_ruta3)) +
    log(1+exp(RPanel_ruta2 - RPanel_ruta3)) +
    log(1+exp(RPanel_ruta4 - RPanel_ruta3)) +
    log(1+exp(RZer_r1 - RZer_r3)) +
    log(1+exp(RZer_r2 - RZer_r3)) +
    log(1+exp(RZer_r4 - RZer_r3)) +
    log(1+exp(RMtrp_r1 - RMtrp_r3)) +
    log(1+exp(RMtrp_r2 - RMtrp_r3)) +
    log(1+exp(RMtrp_r4 - RMtrp_r3))
    
  
  R[['ruta4']]  =
    log(1+exp(b_tt*(T_Alt_1 - T_Alt_4))) + 
    log(1+exp(b_tt*(T_Alt_2 - T_Alt_4))) + 
    log(1+exp(b_tt*(T_Alt_3 - T_Alt_4))) + 
    log(1+exp(b_dt*(D_Alt_1 - D_Alt_4))) + 
    log(1+exp(b_dt*(D_Alt_2 - D_Alt_4))) +
    log(1+exp(b_dt*(D_Alt_3 - D_Alt_4))) +
    log(1+exp(b_Sem*(RSem_ruta1  - RSem_ruta4)))  + 
    log(1+exp(b_Sem*(RSem_ruta2  - RSem_ruta4)))  + 
    log(1+exp(b_Sem*(RSem_ruta3  - RSem_ruta4)))  +
    log(1+exp(RCong_ruta1  - RCong_ruta4))  +
    log(1+exp(RCong_ruta1  - RCong_ruta4))  +
    log(1+exp(RCong_ruta3  - RCong_ruta4))  +
    log(1+exp(RAcc_ruta1  - RAcc_ruta4))  +
    log(1+exp(RAcc_ruta2  - RAcc_ruta4)) +
    log(1+exp(RAcc_ruta3  - RAcc_ruta4)) +
    log(1+exp(RCamFd_ruta1 - RCamFd_ruta4)) +
    log(1+exp(RCamFd_ruta2 - RCamFd_ruta4)) +
    log(1+exp(RCamFd_ruta3 - RCamFd_ruta4)) +
    log(1+exp(RPanel_ruta1 - RPanel_ruta4)) +
    log(1+exp(RPanel_ruta2 - RPanel_ruta4)) +
    log(1+exp(RPanel_ruta3 - RPanel_ruta4)) +
    log(1+exp(RZer_r1 - RZer_r4)) +
    log(1+exp(RZer_r2 - RZer_r4)) +
    log(1+exp(RZer_r3 - RZer_r4)) +
    log(1+exp(RMtrp_r1 - RMtrp_r4)) +
    log(1+exp(RMtrp_r2 - RMtrp_r4)) +
    log(1+exp(RMtrp_r3 - RMtrp_r4))
    

  ### Define settings for RRM model, which is MNL with negative regret as utility
  mnl_settings <- list(
    alternatives = c(ruta1=1, ruta2=2, ruta3=3, ruta4=4),
    avail        = list(ruta1=1, ruta2=1, ruta3=1, ruta4=1),
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

apollo_modelOutput(model, modelOutput_settings=list(printPVal=TRUE) )


# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model, saveOutput_settings=list(printPVal=TRUE) )

