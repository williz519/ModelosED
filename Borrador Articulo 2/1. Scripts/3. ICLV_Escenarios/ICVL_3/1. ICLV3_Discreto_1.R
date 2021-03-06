
# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

## Modelo sin la ruta 3
### Limpiar memoria
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/3. ICLV_Escenarios/ICVL_3/Resultados/ModoConduccion/"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "ICLV3_ModoCond Fusion 2-3",
  modelDescr = "ICLV modelo sobre datos de elección de ruta",
  indivID    = "ViajeId",
  mixing     = TRUE,
  nCores     = 3
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)
#names(database)

#table(database$CHOICE)

#Eliminar Eleccion 3
#database <-  database %>%
  # Eliminar la elección 3
#  filter(!(CHOICE %in% c("3")))

#Reemplazar la ruta 3 en la ruta 2
database$CHOICE[database$CHOICE == 3 ]<-2
database$CHOICE[database$CHOICE == 4]<- 3

#table(database$CHOICE)

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
              b_NO_MTRP = 0, b_SI_MTRP = 0,
              lambda1        = 1,
              lambda2        = 1, 
              lambda3       = 1,
              gamma_USOCINTURON = 0,
              gamma_USODISPMOB =0,
              gamma_EXP_4 = 0, 
              gamma_HPICO = 0,
              gamma_LV1 = 0,
              gamma_EDUBASICA = 0,
              gamma_ADULTO40 = 0,
              gamma_LV2 = 0,
              zeta_FRbr     = 1, zeta_EnfCond  = 1, zeta_AFrSem   = 1, zeta_CulFr    = 1, zeta_OmLmVel  = 1, 
              zeta_IgPare    = 1,  zeta_UsoCel    = 1,  zeta_PasoPeaton = 1, 
              zeta_UsoDirec = 1, zeta_UsoPito = 1,
              tau_FRbr_1      =-2, tau_FRbr_2      =-1, tau_FRbr_3      = 1, tau_FRbr_4      = 2,
              tau_EnfCond_1   =-2, tau_EnfCond_2   =-1, tau_EnfCond_3   = 1, tau_EnfCond_4   = 2, 
              tau_AFrSem_1    =-2, tau_AFrSem_2    =-1, tau_AFrSem_3    = 1, tau_AFrSem_4    = 2, 
              tau_CulFr_1     =-2, 
              tau_CulFr_2     =-1, 
              tau_CulFr_3     = 1, 
              tau_CulFr_4     = 2,
              tau_OmLmVel_1     =-2, 
              tau_OmLmVel_2     =-1, 
              tau_OmLmVel_3     = 1, 
              tau_OmLmVel_4     = 2,
              tau_IgPare_1     =-2, 
              tau_IgPare_2     =-1, 
              tau_IgPare_3     = 1, 
              tau_IgPare_4     = 2,
              tau_UsoCel_1     =-2, 
              tau_UsoCel_2     =-1, 
              tau_UsoCel_3     = 1, 
              tau_UsoCel_4     = 2,
              tau_PasoPeaton_1     =-2, 
              tau_PasoPeaton_2    =-1, 
              tau_PasoPeaton_3    = 1, 
              tau_PasoPeaton_4    = 2,
              tau_UsoDirec_1     =-2, 
              tau_UsoDirec_2    =-1, 
              tau_UsoDirec_3    = 1, 
              tau_UsoDirec_4    = 2,
              tau_UsoPito_1 = -2, 
              tau_UsoPito_2 = -1, 
              tau_UsoPito_3 = 1, 
              tau_UsoPito_4 = 2
)

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_Op2", "b_CongEF", "b_Sem_3", "b_ACC_2", "b_NO_CAMFD", "b_NO_PANEL", "b_NO_ZER",
                 "b_NO_MTRP")

### Lea los valores iniciales para al menos algunos parámetros del archivo de salida del modelo existente
#apollo_beta = apollo_readBeta(apollo_beta, apollo_fixed, "ICLV2_ModoCond", overwriteFixed=FALSE)

# ################################################################# #
#### DEFINE COMPONENTES ALEATORIOS                              ####
# ################################################################# #

### Establecer parámetros para generar sorteos
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=100,          
  interUnifDraws=c(),      
  interNormDraws=c("eta1","eta2","eta3")
)

### Crear parametros aleatorios
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV_1"]] = gamma_USOCINTURON * USOCINTURON + gamma_USODISPMOB * USODISPMOB + eta1
  
  randcoeff[["LV_2"]] = gamma_ADULTO40*ADULTO40 + gamma_EXP_4*EXP_4 + gamma_HPICO*HPICO + 
    gamma_USOCINTURON*USOCINTURON  + gamma_LV1*randcoeff[["LV_1"]] + eta2
  
  randcoeff[["LV_3"]] = gamma_EDUBASICA * EDUBASICA + 
    gamma_USOCINTURON * USOCINTURON + gamma_USODISPMOB * USODISPMOB + gamma_LV2*randcoeff[["LV_2"]] + eta3
  
  return(randcoeff)
}


# ################################################################# #
#### ENTRADAS DE GRUPO Y VALIDACIÓN                              ####
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
  
  ### Likelihood of indicators
  ol_settings1 = list(outcomeOrdered=FRbr, 
                      V=zeta_FRbr*LV_1, 
                      tau=c(tau_FRbr_1, tau_FRbr_2, tau_FRbr_3, tau_FRbr_4))
  ol_settings2 = list(outcomeOrdered=EnfCond, 
                      V=zeta_EnfCond*LV_1, 
                      tau=c(tau_EnfCond_1, tau_EnfCond_2, tau_EnfCond_3, tau_EnfCond_4))
  ol_settings3 = list(outcomeOrdered=AFrSem, 
                      V=zeta_AFrSem*LV_1, 
                      tau=c(tau_AFrSem_1, tau_AFrSem_2, tau_AFrSem_3, tau_AFrSem_4))
  ol_settings4 = list(outcomeOrdered=CulFr, 
                      V=zeta_CulFr*LV_1, 
                      tau=c(tau_CulFr_1, tau_CulFr_2, tau_CulFr_3, tau_CulFr_4))
  ol_settings5 = list(outcomeOrdered=UsoPito, 
                      V=zeta_UsoPito*LV_1, 
                      tau=c(tau_UsoPito_1, tau_UsoPito_2, tau_UsoPito_3, tau_UsoPito_4))
  
  
  ol_settings6 = list(outcomeOrdered=OmLmVel, 
                      V=zeta_OmLmVel*LV_2, 
                      tau=c(tau_OmLmVel_1, tau_OmLmVel_2, tau_OmLmVel_3, tau_OmLmVel_4))
  ol_settings7 = list(outcomeOrdered=IgPare, 
                      V=zeta_IgPare*LV_2, 
                      tau=c(tau_IgPare_1, tau_IgPare_2, tau_IgPare_3, tau_IgPare_4))
  ol_settings8 = list(outcomeOrdered=UsoCel, 
                      V=zeta_UsoCel*LV_2, 
                      tau=c(tau_UsoCel_1, tau_UsoCel_2,tau_UsoCel_3,tau_UsoCel_4))
  ol_settings9 = list(outcomeOrdered=FRbr, 
                      V=zeta_FRbr*LV_2, 
                      tau=c(tau_FRbr_1, tau_FRbr_2, tau_FRbr_3, tau_FRbr_4))
  
  
  ol_settings10 = list(outcomeOrdered=PasoPeaton, 
                      V=zeta_PasoPeaton*LV_3, 
                      tau=c(tau_PasoPeaton_1, tau_PasoPeaton_2,tau_PasoPeaton_3,tau_PasoPeaton_4))
  ol_settings11 = list(outcomeOrdered=UsoDirec, 
                      V=zeta_UsoDirec*LV_3, 
                      tau=c(tau_UsoDirec_1, tau_UsoDirec_2,tau_UsoDirec_3,tau_UsoDirec_4))
  ol_settings12 = list(outcomeOrdered=UsoCel, 
                      V=zeta_UsoCel*LV_3, 
                      tau=c(tau_UsoCel_1, tau_UsoCel_2,tau_UsoCel_3,tau_UsoCel_4))
  ol_settings13 = list(outcomeOrdered=CulFr, 
                      V=zeta_CulFr*LV_3, 
                      tau=c(tau_CulFr_1, tau_CulFr_2, tau_CulFr_3, tau_CulFr_4))
  
  
  
  P[["indic_FRbr"]]       = apollo_ol(ol_settings1, functionality)
  P[["indic_EnfCond"]]    = apollo_ol(ol_settings2, functionality)
  P[["indic_AFrSem"]]     = apollo_ol(ol_settings3, functionality)
  P[["indic_CulFr"]]      = apollo_ol(ol_settings4, functionality)
  P[["indic_UsoPito"]]    = apollo_ol(ol_settings5, functionality)
  P[["indic_OmLmVel"]]    = apollo_ol(ol_settings6, functionality)
  P[["indic_IgPare"]]     = apollo_ol(ol_settings7, functionality)
  P[["indic_UsoCel"]]     = apollo_ol(ol_settings8, functionality)
  P[["indic_FRbr"]]       = apollo_ol(ol_settings9, functionality)
  P[["indic_PasoPeaton"]] = apollo_ol(ol_settings10, functionality)
  P[["indic_UsoDirec"]]   = apollo_ol(ol_settings11, functionality)
  P[["indic_UsoCel"]]     = apollo_ol(ol_settings12, functionality)
  P[["indic_CulFr"]]      = apollo_ol(ol_settings13, functionality)
  
  
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['Op1']]  = (asc_Op1  + b_tt * TIEMPOAlt1+ b_dt * DISTAlt1 + 
    b_CongAB*CONG_AB_A1 + b_CongCD*CONG_CD_A1 + b_CongEF*CONG_EF_A1 +
    b_Sem_1*SEMF_A1_1 + b_Sem_2*SEMF_A1_2 + b_Sem_3*SEMF_A1_3 + 
    b_ACC_0*ACC_A1_0 + b_ACC_1*ACC_A1_1 + b_ACC_2*ACC_A1_2 + 
    b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +
    b_NO_PANEL * NO_PANEL_A1 + b_SI_PANEL * SI_PANEL_A1 + 
    b_NO_ZER * NO_ZER_A1 + b_SI_ZER * SI_ZER_A1 + 
    b_NO_MTRP * NO_MTRP_A1 + b_SI_MTRP * SI_MTRP_A1)
  
  V[['Op2']]  = (asc_Op2  + b_tt * TIEMPOAlt23+ b_dt * DISTAlt23 + 
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
    b_SI_MTRP * (SI_MTRP_A2 + SI_MTRP_A3 - SI_MTRP_A2*SI_MTRP_A3))
  
  V[['Op3']] = (asc_Op3 + b_tt * TIEMPOEC   + b_dt * DISTEC + b_CongAB*CONG_AB_EC + 
    b_CongCD*CONG_CD_EC + b_CongEF*CONG_EF_EC +
    b_Sem_1*SEMF_EC_1 + b_Sem_2*SEMF_EC_2 + b_Sem_3*SEMF_EC_3 + 
    b_ACC_0*ACC_EC_0 + b_ACC_1*ACC_EC_1 + b_ACC_2*ACC_EC_2 + 
    b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +
    b_NO_PANEL * NO_PANEL_EC + b_SI_PANEL * SI_PANEL_EC + 
    b_NO_ZER * NO_ZER_EC + b_SI_ZER * SI_ZER_EC + b_NO_MTRP * NO_MTRP_EC +
    b_SI_MTRP * SI_MTRP_EC + 
    lambda1 * LV_1 + lambda2 * LV_2 + lambda3 * LV_3)
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(Op1=1, Op2=2, Op3=3), 
    avail         = list(Op1=1, Op2=1, Op3=1), 
    choiceVar     = CHOICE,
    V             = V
  )
  
  ### Compute probabilities for MNL model component
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Optional: calculate LL before model estimation
# apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

### Estimate model
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

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS                                          ----
# ----------------------------------------------------------------- #

forecast <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              modelComponent="indic_FRbr")

# ----------------------------------------------------------------- #
#---- CONDITIONALS AND UNCONDITIONALS                            ----
# ----------------------------------------------------------------- #

conditionals <- apollo_conditionals(model,apollo_probabilities,apollo_inputs)

unconditionals <- apollo_unconditionals(model,apollo_probabilities,apollo_inputs)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()

