

# ################################################################# #
#### CARGAR BIBLIOTECA Y DEFINIR AJUSTES BÁSICOS                 ####
# ################################################################# #

## Modelo sin la ruta 3
### Limpiar memoria
rm(list = ls())

workingDirectory="/Users/williz/Desktop/ModelosED/2. Articulo 2/1. Scripts/Modelos de Prueba/ICVL_MCond_4F/Secuencial"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)

### Inicializar código
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "ICLV_MCond_4F",
  modelDescr = "ICLV Modo Conduccion de 4F",
  indivID    = "ViajeId",
  mixing     = TRUE,
  nCores     = 3
)

# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

# Normalización de los viajes
for (i in 1:nrow(database)){
  # Normalización de las variables tiempo
  database$T_Alt_1[i] = (database$TIEMPOAlt1[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.05)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.05))
  database$T_Alt_2[i] = (database$TIEMPOAlt2[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.05)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.05))
  database$T_Alt_3[i] = (database$TIEMPOAlt3[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.05)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.05))
  database$T_Alt_4[i] = (database$TIEMPOEC[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.05)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-0.05))
  
  # Normalización de la variable distancia
  database$D_Alt_1[i] = (database$DISTAlt1[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))
  database$D_Alt_2[i] = (database$DISTAlt2[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))
  database$D_Alt_3[i] = (database$DISTAlt3[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))
  database$D_Alt_4[i] = (database$DISTEC[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.15)))
}




# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1   = 0, asc_ruta2   = 0, asc_ruta3   = 0, asc_ruta4  = 0,
              b_tt  = 0,  
              b_dt  = 0,
              b_CongAB  = 0, b_CongCD  = 0, b_CongEF  = 0,
              b_Sem = 0,
              b_ACC_0 = 0, b_ACC_1 = 0, b_ACC_2 = 0,
              b_NO_CAMFD = 0, b_SI_CAMFD = 0, 
              b_NO_PANEL = 0, b_SI_PANEL = 0, 
              b_NO_ZER = 0, b_SI_ZER = 0, 
              b_No_MTRP = 0, b_Si_MTRP = 0,
              lambda1        = 1,
              lambda2        = 1, 
              lambda3       = 1,
              lambda4       = 1,
              
              #Regresion LV_1
              gamma_EXP_3_LV_1 = -0.358,
              gamma_HTRB_2_LV_1 = 0.306,
              gamma_CSECO_LV_1 = 0.174,
              gamma_SININFOTRF_LV_1 = -0.175,
              gamma_USODISPMOB_LV_1 = 0.210,
              
              #Regresion LV_2
              gamma_ADULTO40_LV_2 = -0.190,
              gamma_EXP_2_LV_2 = -0.139,
              gamma_EXP_5_LV_2 = -0.148,
              gamma_HPICO_LV_2 = 0.150,
              gamma_CSECO_LV_2 = -0.216,
              gamma_SININFOTRF_LV_2 = 0.140,
              gamma_USODISPMOB_LV_2 = 0.096,
              gamma_LV1_LV_2 = 0.631,
              
              #Regresion LV_3
              gamma_EDUBASICA_LV_3 = -0.358,
              gamma_JOVEN30_LV_3 = -0.570,
              gamma_ADULTO40_LV_3 = -0.415,
              gamma_EXP_2_LV_3 = 0.410,
              gamma_HTRB_2_LV_3 = -1.741,
              gamma_HTRB_3_LV_3 = -1.361,
              gamma_USODISPMOB_LV_3 = -0.352,
              
              #Regresion LV_4
              gamma_JOVEN30_LV_4 = -0.239,
              gamma_ADULTO40_LV_4 = -0.165,
              gamma_ADULTO60_LV_4 = -0.167,
              gamma_EXP_2_LV_4 = -0.122,
              gamma_EXP_5_LV_4 = -0.079,
              gamma_CSECO_LV_4 = -0.073,
              gamma_LV1_LV_4 = 0.496,
              
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
apollo_fixed = c("asc_ruta3", "b_CongAB", "b_ACC_0", "b_NO_CAMFD", "b_No_MTRP", "b_NO_PANEL", "b_NO_ZER",
                 "gamma_EXP_3_LV_1",
                 "gamma_HTRB_2_LV_1",
                 "gamma_CSECO_LV_1",
                 "gamma_SININFOTRF_LV_1",
                 "gamma_USODISPMOB_LV_1",
                 "gamma_ADULTO40_LV_2",
                 "gamma_EXP_2_LV_2",
                 "gamma_EXP_5_LV_2",
                 "gamma_HPICO_LV_2",
                 "gamma_CSECO_LV_2",
                 "gamma_SININFOTRF_LV_2",
                 "gamma_USODISPMOB_LV_2",
                 "gamma_LV1_LV_2",
                 "gamma_EDUBASICA_LV_3",
                 "gamma_JOVEN30_LV_3",
                 "gamma_ADULTO40_LV_3",
                 "gamma_EXP_2_LV_3",
                 "gamma_HTRB_2_LV_3",
                 "gamma_HTRB_3_LV_3",
                 "gamma_USODISPMOB_LV_3",
                 "gamma_JOVEN30_LV_4",
                 "gamma_ADULTO40_LV_4",
                 "gamma_ADULTO60_LV_4",
                 "gamma_EXP_2_LV_4",
                 "gamma_EXP_5_LV_4",
                 "gamma_CSECO_LV_4",
                 "gamma_LV1_LV_4")

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
  interNormDraws=c("eta1","eta2","eta3", "eta4")
)

### Crear parametros aleatorios
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV_1"]] = gamma_EXP_3_LV_1*EXP_3 + gamma_HTRB_2_LV_1*HTRB_2 + gamma_CSECO_LV_1*CSECO +
    gamma_SININFOTRF_LV_1*SININFOTRF + gamma_USODISPMOB_LV_1 * USODISPMOB + eta1
  
  randcoeff[["LV_2"]] = gamma_ADULTO40_LV_2 * ADULTO40 + gamma_EXP_2_LV_2*EXP_2 + gamma_EXP_5_LV_2*EXP_5
    gamma_HPICO_LV_2*HPICO + gamma_CSECO_LV_2*CSECO + gamma_SININFOTRF_LV_2*SININFOTRF + gamma_USODISPMOB_LV_2 * USODISPMOB + 
    gamma_LV1_LV_2 * randcoeff[["LV_1"]] + eta2
  
  
  randcoeff[["LV_3"]] = gamma_EDUBASICA_LV_3 * EDUBASICA + gamma_JOVEN30_LV_3 * JOVEN30 + 
    gamma_ADULTO40_LV_3 * ADULTO40 + gamma_EXP_2_LV_3*EXP_2 + gamma_HTRB_2_LV_3 * HTRB_2 + gamma_HTRB_3_LV_3*HTRB_3 +
    gamma_USODISPMOB_LV_3 * USODISPMOB + eta3

  
  randcoeff[["LV_4"]] = gamma_JOVEN30_LV_4 * JOVEN30 + gamma_ADULTO40_LV_4 * ADULTO40 + gamma_ADULTO60_LV_4*ADULTO60 +
    gamma_EXP_2_LV_4*EXP_2 +  gamma_EXP_5_LV_4*EXP_5 + gamma_CSECO_LV_4*CSECO + 
    gamma_LV1_LV_4 * randcoeff[["LV_1"]] + eta4
  
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
  ol_settings2 = list(outcomeOrdered=AFrSem, 
                      V=zeta_AFrSem*LV_1, 
                      tau=c(tau_AFrSem_1, tau_AFrSem_2, tau_AFrSem_3, tau_AFrSem_4))
  ol_settings3 = list(outcomeOrdered=CulFr, 
                      V=zeta_CulFr*LV_1, 
                      tau=c(tau_CulFr_1, tau_CulFr_2, tau_CulFr_3, tau_CulFr_4))
  
  ol_settings4 = list(outcomeOrdered=OmLmVel, 
                      V=zeta_OmLmVel*LV_2, 
                      tau=c(tau_OmLmVel_1, tau_OmLmVel_2, tau_OmLmVel_3, tau_OmLmVel_4))
  ol_settings5 = list(outcomeOrdered=IgPare, 
                      V=zeta_IgPare*LV_2, 
                      tau=c(tau_IgPare_1, tau_IgPare_2, tau_IgPare_3, tau_IgPare_4))
  ol_settings6 = list(outcomeOrdered=UsoCel, 
                      V=zeta_UsoCel*LV_2, 
                      tau=c(tau_UsoCel_1, tau_UsoCel_2,tau_UsoCel_3,tau_UsoCel_4))
  ol_settings7 = list(outcomeOrdered=UsoPito, 
                      V=zeta_UsoPito*LV_2, 
                      tau=c(tau_UsoPito_1, tau_UsoPito_2, tau_UsoPito_3, tau_UsoPito_4))
  
  
  ol_settings8 = list(outcomeOrdered=PasoPeaton, 
                       V=zeta_PasoPeaton*LV_3, 
                       tau=c(tau_PasoPeaton_1, tau_PasoPeaton_2,tau_PasoPeaton_3,tau_PasoPeaton_4))
  ol_settings9 = list(outcomeOrdered=UsoDirec, 
                      V=zeta_UsoDirec*LV_3, 
                      tau=c(tau_UsoDirec_1, tau_UsoDirec_2,tau_UsoDirec_3,tau_UsoDirec_4))
  ol_settings10 = list(outcomeOrdered=UsoCel, 
                       V=zeta_UsoCel*LV_3, 
                       tau=c(tau_UsoCel_1, tau_UsoCel_2,tau_UsoCel_3,tau_UsoCel_4))
  
  ol_settings11 = list(outcomeOrdered=UsoPito, 
                      V=zeta_UsoPito*LV_4, 
                      tau=c(tau_UsoPito_1, tau_UsoPito_2, tau_UsoPito_3, tau_UsoPito_4))
  ol_settings12 = list(outcomeOrdered=EnfCond, 
                      V=zeta_EnfCond*LV_4, 
                      tau=c(tau_EnfCond_1, tau_EnfCond_2, tau_EnfCond_3, tau_EnfCond_4))
  ol_settings13 = list(outcomeOrdered=UsoCel, 
                       V=zeta_UsoCel*LV_4, 
                       tau=c(tau_UsoCel_1, tau_UsoCel_2,tau_UsoCel_3,tau_UsoCel_4))
  
  
  P[["indic_FRbr"]]       = apollo_ol(ol_settings1, functionality)
  P[["indic_AFrSem"]]     = apollo_ol(ol_settings2, functionality)
  P[["indic_CulFr"]]      = apollo_ol(ol_settings3, functionality)
  
  P[["indic_OmLmVel"]]    = apollo_ol(ol_settings4, functionality)
  P[["indic_IgPare"]]     = apollo_ol(ol_settings5, functionality)
  P[["indic_UsoCel"]]     = apollo_ol(ol_settings6, functionality)
  P[["indic_UsoPito"]]    = apollo_ol(ol_settings7, functionality)
  
  P[["indic_PasoPeaton"]] = apollo_ol(ol_settings8, functionality)
  P[["indic_UsoDirec"]]   = apollo_ol(ol_settings9, functionality)
  P[["indic_UsoCel"]]     = apollo_ol(ol_settings10, functionality)
  
  P[["indic_UsoPito"]]    = apollo_ol(ol_settings11, functionality)
  P[["indic_EnfCond"]]    = apollo_ol(ol_settings12, functionality)
  P[["indic_UsoCel"]]     = apollo_ol(ol_settings13, functionality)
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['ruta1']]  = (asc_ruta1  + b_tt * T_Alt_1 + b_dt * D_Alt_1 + 
                   b_CongAB*CONG_AB_A1 + b_CongCD*CONG_CD_A1 + b_CongEF*CONG_EF_A1 +
                   b_Sem*SEM_A1_km + 
                   b_ACC_0*ACC_A1_0 + b_ACC_1*ACC_A1_1 + b_ACC_2*ACC_A1_2 + 
                   b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +
                   b_NO_PANEL * NO_PANEL_A1 + b_SI_PANEL * SI_PANEL_A1 + 
                   b_NO_ZER * NO_ZER_A1 + b_SI_ZER * SI_ZER_A1 + 
                   b_No_MTRP * NO_MTRP_A1 + b_Si_MTRP * SI_MTRP_A1)
  
  V[['ruta2']]  = (asc_ruta2  + b_tt * T_Alt_2 + b_dt * D_Alt_2 + 
                     b_CongAB*CONG_AB_A2 + b_CongCD*CONG_CD_A2 + b_CongEF*CONG_EF_A2 + 
                     b_Sem*SEM_A2_km + 
                     b_ACC_0*ACC_A2_0 + b_ACC_1*ACC_A2_1 + b_ACC_2*ACC_A2_2 + 
                     b_NO_CAMFD * NO_CAMFD_A2 + b_SI_CAMFD * SI_CAMFD_A2 +
                     b_NO_PANEL * NO_PANEL_A2 + b_SI_PANEL * SI_PANEL_A2 + 
                     b_NO_ZER * NO_ZER_A2 + b_SI_ZER * SI_ZER_A2 + 
                     b_No_MTRP * NO_MTRP_A2 + b_Si_MTRP * SI_MTRP_A2)
  
  V[['ruta3']]  = (asc_ruta3  + b_tt * T_Alt_3 + b_dt * D_Alt_3 + 
                     b_CongAB*CONG_AB_A3 + b_CongCD*CONG_CD_A3 + b_CongEF*CONG_EF_A3 +  
                     b_Sem*SEM_A3_km +
                     b_ACC_0*ACC_A3_0 + b_ACC_1*ACC_A3_1 + b_ACC_2*ACC_A3_2 +  
                     b_NO_CAMFD * NO_CAMFD_A3 + b_SI_CAMFD * SI_CAMFD_A3 +
                     b_NO_PANEL * NO_PANEL_A3 + b_SI_PANEL * SI_PANEL_A3 + 
                     b_NO_ZER * NO_ZER_A3 + b_SI_ZER * SI_ZER_A3 + 
                     b_No_MTRP * NO_MTRP_A3 + b_Si_MTRP * SI_MTRP_A3)
  
  V[['rutaEC']] = (asc_ruta4 + b_tt * T_Alt_4   + b_dt * D_Alt_4 + 
                     b_CongAB*CONG_AB_EC + b_CongCD*CONG_CD_EC + b_CongEF*CONG_EF_EC +
                     b_Sem*SEM_EC_km +
                     b_ACC_0*ACC_EC_0 + b_ACC_1*ACC_EC_1 + b_ACC_2*ACC_EC_2 + 
                     b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +
                     b_NO_PANEL * NO_PANEL_EC + b_SI_PANEL * SI_PANEL_EC + 
                     b_NO_ZER * NO_ZER_EC + b_SI_ZER * SI_ZER_EC + 
                     b_No_MTRP * NO_MTRP_EC + b_Si_MTRP * SI_MTRP_EC +
                     lambda1 * LV_1 + lambda2 * LV_2 + lambda3 * LV_3 + lambda4 * LV_4)
  
  
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