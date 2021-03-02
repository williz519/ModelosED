
# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

## Modelo sin la ruta 3
### Limpiar memoria
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/3. ICLV_Escenarios/ICVL_3/Resultados/Personalidad/"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "ICLV3_ModoCond Fusion 2-3 Personalidad",
  modelDescr = "ICLV modelo sobre datos de elección de ruta",
  indivID    = "ViajeId",
  mixing     = TRUE,
  nCores     = 3
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)
names(database)

#table(database$CHOICE)

#Eliminar Eleccion 3
#database <-  database %>%
  # Eliminar la elección 3
#  filter(!(CHOICE %in% c("3")))

#Reemplazar la ruta 3 en la ruta 2
database$CHOICE[database$CHOICE == 3 ]<-2
database$CHOICE[database$CHOICE == 4]<- 3

table(database$CHOICE)

### Create new variable with time

for (i in 1:nrow(database)) {
  database$TIEMPOAlt23[i] = max(database$TIEMPOAlt2[i], database$TIEMPOAlt3[i])
  database$DISTAlt23[i] = max(database$DISTAlt2[i], database$DISTAlt3[i])
}
database$TIEMPOAlt23
database$DISTAlt23

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
              lambda4        = 1,
              lambda5        = 1, 
              gamma_USOCINTURON = 0,
              gamma_USODISPMOB =0,
              gamma_EXP_2 = 0,
              gamma_LV4 = 0,
              zeta_PrPer   = 1, 
              zeta_AmbTr   = 1, 
              zeta_ComVrb  = 1,
              zeta_ComAfec = 1, 
              zeta_ConCl   = 1, 
              zeta_Ans     = 1,  
              zeta_StrC    = 1,
              tau_PrPer_1   =-2, 
              tau_PrPer_2   =-1, 
              tau_PrPer_3   = 1, 
              tau_PrPer_4   = 2,
              tau_AmbTr_1   =-2, 
              tau_AmbTr_2   =-1, 
              tau_AmbTr_3   = 1, 
              tau_AmbTr_4   = 2, 
              tau_ComVrb_1  =-2, 
              tau_ComVrb_2  =-1, 
              tau_ComVrb_3  = 1, 
              tau_ComVrb_4  = 2, 
              tau_ComAfec_1 =-2, 
              tau_ComAfec_2 =-1, 
              tau_ComAfec_3 = 1, 
              tau_ComAfec_4 = 2,
              tau_ConCl_1   =-4, 
              tau_ConCl_2   =-2, 
              tau_ConCl_3   = 2, 
              tau_ConCl_4   = 4,
              tau_Ans_1     =-2, 
              tau_Ans_2     =-1, 
              tau_Ans_3     = 1, 
              tau_Ans_4     = 2,
              tau_StrC_1    =-2, 
              tau_StrC_2    =-1, 
              tau_StrC_3    = 1, 
              tau_StrC_4    = 2)

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_Op2", "b_CongEF", "b_Sem_3", "b_ACC_2", "b_NO_CAMFD", "b_NO_PANEL", "b_NO_ZER",
                 "b_NO_MTRP" )

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
  interNormDraws=c("eta4","eta5")
)

### Crear parametros aleatorios
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV_4"]] = gamma_EXP_2* EXP_2 + gamma_USOCINTURON * USOCINTURON + gamma_USODISPMOB * USODISPMOB + eta4
  
  randcoeff[["LV_5"]] =  gamma_USOCINTURON * USOCINTURON + gamma_LV4*randcoeff[["LV_4"]] + eta5
  

  
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
  ol_settings1 = list(outcomeOrdered=ComVrb, 
                      V=zeta_ComVrb * LV_4, 
                      tau=c(tau_ComVrb_1, tau_ComVrb_2, tau_ComVrb_3, tau_ComVrb_4))
  ol_settings2 = list(outcomeOrdered=Ans, 
                      V=zeta_Ans * LV_4, 
                      tau=c(tau_Ans_1, tau_Ans_2, tau_Ans_3, tau_Ans_4))
  ol_settings3 = list(outcomeOrdered=ComAfec, 
                      V=zeta_ComAfec * LV_4, 
                      tau=c(tau_ComAfec_1, tau_ComAfec_2, tau_ComAfec_3, tau_ComAfec_4))
  ol_settings4 = list(outcomeOrdered=StrC, 
                      V=zeta_StrC * LV_4, 
                      tau=c(tau_StrC_1, tau_StrC_2, tau_StrC_3, tau_StrC_4))
  ol_settings5 = list(outcomeOrdered=ConCl, 
                      V=zeta_ConCl * LV_4, 
                      tau=c(tau_ConCl_1, tau_ConCl_2, tau_ConCl_3, tau_ConCl_4))
  
  
  ol_settings6 = list(outcomeOrdered=PrPer, 
                      V=zeta_PrPer * LV_5, 
                      tau=c(tau_PrPer_1, tau_PrPer_2, tau_PrPer_3, tau_PrPer_4))
  ol_settings7 = list(outcomeOrdered=AmbTr, 
                      V=zeta_AmbTr * LV_5, 
                      tau=c(tau_AmbTr_1, tau_AmbTr_2, tau_AmbTr_3, tau_AmbTr_4))
  ol_settings8 = list(outcomeOrdered=Ans, 
                      V=zeta_Ans * LV_5, 
                      tau=c(tau_Ans_1, tau_Ans_2, tau_Ans_3, tau_Ans_4))
  
  P[["indic_ComVrb"]]   = apollo_ol(ol_settings1, functionality)
  P[["indic_Ans"]]      = apollo_ol(ol_settings2, functionality)
  P[["indic_ComAfec"]]  = apollo_ol(ol_settings3, functionality)
  P[["indic_StrC"]]     = apollo_ol(ol_settings4, functionality)
  P[["indic_ConCl"]]    = apollo_ol(ol_settings5, functionality)
  P[["indic_PrPer"]]    = apollo_ol(ol_settings6, functionality)
  P[["indic_AmbTr"]]    = apollo_ol(ol_settings7, functionality)
  P[["indic_Ans"]]      = apollo_ol(ol_settings8, functionality)
  
  
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
    lambda4 * LV_4 + lambda5 * LV_5)
  
  
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

