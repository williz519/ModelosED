

# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

#install.packages("apollo")
### Limpiar memoria
rm(list = ls())

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "ICLV_DosFactores_alpha_0.05",
  modelDescr = "ICLV modelo sobre datos de elección de ruta",
  indivID    = "ViajeId",
  mixing     = TRUE,
  nCores     = 3
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

names(database)
database$EXP_1 <- ifelse((database$Experiencia == 1),1,0)
database$EXP_2 <- ifelse((database$Experiencia == 2),1,0)
database$EXP_3 <- ifelse((database$Experiencia == 3),1,0)
database$EXP_4 <- ifelse((database$Experiencia == 4),1,0)
database$EXP_5 <- ifelse((database$Experiencia == 5),1,0)


database = subset(database, database$MODELO == 1)
dplyr::glimpse(database)  
#summary(database)


# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0, asc_ruta2   = 0, asc_ruta3   = 0, asc_rutaEC  = 0,
              b_time      = 0, b_dist      = 0, b_cong      = 0, b_Sem_0     = 0,
              b_Sem_1     = 0, b_Sem_2     = 0, b_Sem_3     = 0, b_Sem_4     = 0,
              b_ACC_0     = 0, b_ACC_1     = 0, b_ACC_2     = 0, b_NO_CAMFD  = 0,
              b_SI_CAMFD  = 0, b_NO_PANEL  = 0, b_SI_PANEL  = 0, b_NO_ZER    = 0,
              b_SI_ZER    = 0,
              b_CSECO     = 0,
              b_CLLUVIA   = 0,
              b_Hpico     = 0,
              b_Hvalle    = 0,
              b_Joven30   = 0,
              b_Adulto40  = 0,
              b_Adulto60  = 0,
              b_AdultoMayor   = 0,
              b_CONG_AB   = 0,
              b_CONG_CD   = 0,
              b_CONG_EF   = 0,
              b_USOCINTURON   = 0,
              b_NOUSOCINTURON = 0,
              b_USODISPMOB    = 0, 
              b_NOUSODISPMOB  = 0,
              b_HTRB_1      = 0,
              b_HTRB_2      = 0,
              b_HTRB_3      = 0,
              b_HTRB_4      = 0,
              b_EduBasica   = 0,
              b_EduSuperior = 0,
              b_Exp_1     = 0,
              b_Exp_2     = 0,
              b_Exp_3     = 0,
              b_Exp_4     = 0,
              b_Exp_5     = 0,
              b_Info_traf   = 0,
              b_NoInfo_traf = 0,
              lambda_1       = 1,
              lambda_2       = 1,
              gamma_CONG_EF   = 0.222,
              gamma_USOCINTURON  = 0,
              gamma_EDUBASICA = -0.265,
              gamma_EXP_2   = 0.169,
              gamma_CONG_CD  = 0.083,
              gamma_USODISPMOB  = -0.236,
              gamma_LV1     = 0.109,
              zeta_FRbr     = 1, 
              zeta_EnfCond  = 1, 
              zeta_AFrSem   = 1, 
              zeta_CulFr    = 1, 
              zeta_OmLmVel  = 1, 
              zeta_IgPare    = 1,  
              zeta_UsoCel    = 1, 
              zeta_PasoPeaton = 1, 
              zeta_UsoDirec = 1, 
              tau_FRbr_1     =-2, 
              tau_FRbr_2     =-1, 
              tau_FRbr_3     = 1, 
              tau_FRbr_4     = 2,
              tau_EnfCond_1   =-2, 
              tau_EnfCond_2   =-1, 
              tau_EnfCond_3   = 1, 
              tau_EnfCond_4   = 2, 
              tau_AFrSem_1    =-2, 
              tau_AFrSem_2    =-1, 
              tau_AFrSem_3    = 1, 
              tau_AFrSem_4    = 2, 
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
              tau_UsoDirec_4    = 2
)


### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_rutaEC", "b_EduSuperior", "b_AdultoMayor","b_Exp_5","b_HTRB_1","b_Hvalle",
                 "b_CLLUVIA","b_CONG_AB", "b_Info_traf","b_NOUSOCINTURON","b_NOUSODISPMOB" )


# ################################################################# #
#### DEFINE COMPONENTES ALEATORIOS                              ####
# ################################################################# #

### Establecer parámetros para generar sorteos
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=100,          
  interUnifDraws=c(),      
  interNormDraws=c("eta1","eta2"),
  
  intraDrawsType = '',
  intraNDraws = 0,
  intraUnifDraws = c(),
  interNormDraws=c()
)

### Crear parametros aleatorios
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV_1"]] = gamma_CONG_EF * CONG_EF + gamma_USOCINTURON * USOCINTURON + eta1
  
  randcoeff[["LV_2"]] = gamma_EDUBASICA * EDUBASICA + gamma_EXP_2 * EXP_2 + 
    gamma_CONG_CD * CONG_CD + gamma_USOCINTURON * USOCINTURON + gamma_USODISPMOB * USODISPMOB  + 
    gamma_LV1*randcoeff[["LV_1"]] + eta2

  
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
  
  #LV_2 = gamma_LV1 * LV_1
  
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
  ol_settings5 = list(outcomeOrdered=OmLmVel, 
                      V=zeta_OmLmVel*LV_1, 
                      tau=c(tau_OmLmVel_1, tau_OmLmVel_2, tau_OmLmVel_3, tau_OmLmVel_4))
  ol_settings6 = list(outcomeOrdered=IgPare, 
                      V=zeta_IgPare*LV_1, 
                      tau=c(tau_IgPare_1, tau_IgPare_2, tau_IgPare_3, tau_IgPare_4))
  ol_settings7 = list(outcomeOrdered=UsoCel, 
                      V=zeta_UsoCel*LV_1, 
                      tau=c(tau_UsoCel_1, tau_UsoCel_2,tau_UsoCel_3,tau_UsoCel_4))
  ol_settings8 = list(outcomeOrdered=PasoPeaton, 
                      V=zeta_PasoPeaton*LV_2, 
                      tau=c(tau_PasoPeaton_1, tau_PasoPeaton_2,tau_PasoPeaton_3,tau_PasoPeaton_4))
  ol_settings9 = list(outcomeOrdered=UsoDirec, 
                      V=zeta_UsoDirec*LV_2, 
                      tau=c(tau_UsoDirec_1, tau_UsoDirec_2,tau_UsoDirec_3,tau_UsoDirec_4))
  
  
  P[["indic_FRbr"]]     = apollo_ol(ol_settings1, functionality)
  P[["indic_EnfCond"]]  = apollo_ol(ol_settings2, functionality)
  P[["indic_AFrSem"]]   = apollo_ol(ol_settings3, functionality)
  P[["indic_CulFr"]]    = apollo_ol(ol_settings4, functionality)
  P[["indic_OmLmVel"]]  = apollo_ol(ol_settings5, functionality)
  P[["indic_IgPare"]]   = apollo_ol(ol_settings6, functionality)
  P[["indic_UsoCel1"]]   = apollo_ol(ol_settings7, functionality)
  P[["indic_PasoPeaton"]]   = apollo_ol(ol_settings8, functionality)
  P[["indic_UsoDirec"]]   = apollo_ol(ol_settings9, functionality)
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['ruta1']]  = (asc_ruta1  + b_time * TIEMPOAlt1 + b_dist * DISTAlt1 + b_cong * CONG_A1 +
                     b_Sem_0 * SEMF_A1_0 + b_Sem_1 * SEMF_A1_1 + b_Sem_2 * SEMF_A1_2 + b_Sem_3 * SEMF_A1_3 +
                     b_Sem_4  * SEMF_A1_4 + b_ACC_0 * ACC_A1_0 + b_ACC_1 * ACC_A1_1 + b_ACC_2 * ACC_A1_2 +
                     b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +b_NO_PANEL * NO_PANEL_A1 +
                     b_SI_PANEL * SI_PANEL_A1 + b_NO_ZER * NO_ZER_A1 + b_SI_ZER * SI_ZER_A1)
  
  V[['ruta2']]  = (asc_ruta2  + b_time * TIEMPOAlt2 + b_dist * DISTAlt2 + b_cong * CONG_A2 +
                     b_Sem_0 * SEMF_A2_0 + b_Sem_1 * SEMF_A2_1 + b_Sem_2 * SEMF_A2_2 + b_Sem_3 * SEMF_A2_3 +
                     b_Sem_4  * SEMF_A2_4 + b_ACC_0 * ACC_A2_0 + b_ACC_1 * ACC_A2_1 + b_ACC_2 * ACC_A2_2 +
                     b_NO_CAMFD * NO_CAMFD_A2 + b_SI_CAMFD * SI_CAMFD_A2 + b_NO_PANEL * NO_PANEL_A2 +
                     b_SI_PANEL * SI_PANEL_A2 + b_NO_ZER * NO_ZER_A2 + b_SI_ZER * SI_ZER_A2)
  
  V[['ruta3']]  = (asc_ruta3  + b_time * TIEMPOAlt3 + b_dist * DISTAlt3 + b_cong * CONG_A3 +
                     b_Sem_0 * SEMF_A3_0 + b_Sem_1 * SEMF_A3_1 + b_Sem_2 * SEMF_A3_2 + b_Sem_3 * SEMF_A3_3 +
                     b_Sem_4  * SEMF_A3_4 + b_ACC_0 * ACC_A3_0 + b_ACC_1 * ACC_A3_1 + b_ACC_2 * ACC_A3_2 +
                     b_NO_CAMFD * NO_CAMFD_A3 + b_SI_CAMFD * SI_CAMFD_A3 +b_NO_PANEL * NO_PANEL_A3 +
                     b_SI_PANEL * SI_PANEL_A3 + b_NO_ZER * NO_ZER_A3 + b_SI_ZER * SI_ZER_A3)
  
  V[['rutaEC']] = (asc_rutaEC + b_time * TIEMPOEC   + b_dist * DISTEC   + b_cong * CONGESTION +
                     b_Sem_0 * SEMF_EC_0 + b_Sem_1 * SEMF_EC_1 + b_Sem_2 * SEMF_EC_2 + b_Sem_3 * SEMF_EC_3 +
                     b_Sem_4  * SEMF_EC_4 + b_ACC_0 * ACC_EC_0 + b_ACC_1 * ACC_EC_1 + b_ACC_2 * ACC_EC_2 +
                     b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +b_NO_PANEL * NO_PANEL_EC +
                     b_SI_PANEL * SI_PANEL_EC + b_NO_ZER * NO_ZER_EC + b_SI_ZER * SI_ZER_EC +
                     b_Hpico * HPICO + b_Hvalle * HVALLE + b_CSECO * CSECO + b_CLLUVIA * CLLUVIA + 
                     b_Joven30 * JOVEN30 + b_Adulto40 * ADULTO40 + b_Adulto60 *ADULTO60 + b_AdultoMayor * ADULTOMAYOR +
                     b_USOCINTURON * USOCINTURON + b_NOUSOCINTURON * NOUSOCINTURON + b_USODISPMOB * USODISPMOB + 
                     b_NOUSODISPMOB * NOUSODISPMOB +
                     b_HTRB_1 * HTRB_1 + b_HTRB_2 * HTRB_2 + b_HTRB_3 * HTRB_3 + b_HTRB_4 * HTRB_4 +
                     b_EduBasica * EDUBASICA + b_EduSuperior * EDUSUP + 
                     b_Exp_1 * EXP_1  +   b_Exp_2 * EXP_2 + b_Exp_3 * EXP_3 + b_Exp_4 * EXP_4 + 
                     b_Exp_5 * EXP_5 +b_Info_traf *(INFOTRAFICO == 2) + b_NoInfo_traf * (INFOTRAFICO == 1) +
                     lambda_1 * LV_1 + lambda_2 * LV_2)
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(ruta1=1, ruta2=2, ruta3=3, rutaEC=4), 
    avail         = list(ruta1=1, ruta2=1, ruta3=1, rutaEC=1), 
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
#model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, 
                        estimate_settings = list(maxIterations = 400))

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
