# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
#library(dplyr)
rm(list = ls())

## Semilla
set.seed(1234)


workingDirectory="/Users/williz/Desktop/ModelosED/3. Articulo 3/4. Resultados/Prospectiva/3Rutas"
setwd(workingDirectory)

### Cargar libreria Apollo
library(apollo)
library(caret)

### Initialise code
apollo_initialise()

## Establecer controles principales
apollo_control = list(
  modelName  = "Prospective Simple 3Rutas",
  modelDescr = "Modelos Prospectivo en Eleccion de Ruta",
  indivID    = "ViajeId"
)


# ################################################################# #
#### CARGAR DATOS Y APLICAR CUALQUIER TRANSFORMACIÓN             ####
# ################################################################# #

database = read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBMuestraArt3_3Rutas.csv",sep="\t", dec=".",header=TRUE)

## Parametros Teoría Prospectiva




for (i in 1:nrow(database)){ 
  alpha = 0.72
  beta  = 0.74
  lamda = 3.2
  sigma = 0.25
  gamma = 0.61
  delta = 0.69
  theta = 0
  eta = 0.18         # Estan Calibrados ... No Cambiar
  eta1 = 0.20
  
  database$DT1[i] <- database$T_Alt_1[i] - (database$T_rf_Alt1[i]+ eta)
  #database$DT1[i] <- database$T_Alt_4[i] - database$T_Alt_1[i]
  
  if(database$DT1[i] > sigma)
    {database$VN1[i] <- -lamda*(database$DT1[i])**beta}
  else
    {database$VN1[i]<- 0}
  if(database$DT1[i] < -sigma)
    {database$VP1[i] <- (-database$DT1[i])**alpha}
  else
    {database$VP1[i]<- 0}
  if (abs(database$DT1[i]) <= sigma)
  {database$VP1[i] <- 0; database$VN1[i]<- 0}
  
  
  
  database$DT2[i] <- database$T_Alt_2[i] - (database$T_rf_Alt2[i] + eta)
  #database$DT2[i] <- database$T_Alt_4[i]- database$T_Alt_2[i]
  
  if(database$DT2[i] > sigma)
  {database$VN2[i] <- -lamda*(database$DT2[i])**beta}
  else 
  {database$VN2[i]<- 0}
  if(database$DT2[i] < -sigma)
  {database$VP2[i] <- (-database$DT2[i])**alpha}
  else
  {database$VP2[i]<- 0}
  if (abs(database$DT2[i]) <= sigma)
  {database$VP2[i] <- 0; database$VN2[i]<- 0}
  
  database$DT4[i] <- database$T_Alt_4[i] - (database$T_rf_Alt4[i]+ eta1)
  
  if(database$DT4[i] > sigma + theta)
  {database$VN4[i] <- -lamda*(database$DT4[i])**beta}
  else 
  {database$VN4[i]<- 0}
  if(database$DT4[i] < -sigma )
  {database$VP4[i] <- (-database$DT4[i])**alpha}
  else
  {database$VP4[i]<- 0}
  if (abs(database$DT4[i]) <= sigma)
  {database$VP4[i] <- 0; database$VN4[i]<- 0}
}


## Probabilidades 


for (i in 1:nrow(database)){ 
  B1 = 0.05
  B2 = 0.1
  B3 = 0.15
  B4 = 0.2
  C4 = 0.2
  C3 = 0.15
  C2 = 0.1
  C1 = 0.05
  ## Probabilidad Alt_1
  if (database$T_Alt_4[i] < database$T_Alt_1[i])
  {if (database$T_Alt_4[i] < database$T_Alt_1[i]-3*sigma)
      {database$P_Alt1[i] <- B1}
   else {if (database$T_Alt_4[i] < database$T_Alt_1[i]-2*sigma)
            {database$P_Alt1[i] <- B2}
         else {if (database$T_Alt_4[i] < database$T_Alt_1[i]-sigma)
                  {database$P_Alt1[i] <- B3}
               else {database$P_Alt1[i] <- B4}}}}
  else {if (database$T_Alt_4[i] > database$T_Alt_1[i]+3*sigma)
           {database$P_Alt1[i] <- C1}
        else {if (database$T_Alt_4[i] > database$T_Alt_1[i]+2*sigma)
                 {database$P_Alt1[i] <- C2}
              else {if (database$T_Alt_4[i] > database$T_Alt_1[i]+sigma)
                      {database$P_Alt1[i] <- C3}
                    else {database$P_Alt1[i] <- C4}}}}
  
  ## Probabilidad Alt_2
  if (database$T_Alt_4[i] < database$T_Alt_2[i])
      {if (database$T_Alt_4[i] < database$T_Alt_2[i]-3*sigma)
          {database$P_Alt2[i] <- B1}
      else {if (database$T_Alt_4[i] < database$T_Alt_2[i]-2*sigma)
                {database$P_Alt2[i] <- B2}
            else {if (database$T_Alt_4[i] < database$T_Alt_2[i]-sigma)
                      {database$P_Alt2[i] <- B3}
                  else {database$P_Alt2[i] <- B4}}}}
  else {if (database$T_Alt_4[i] > database$T_Alt_2[i]+3*sigma)
            {database$P_Alt2[i] <- C1}
        else {if (database$T_Alt_4[i] > database$T_Alt_2[i]+2*sigma)
                  {database$P_Alt2[i] <- C2}
              else {if (database$T_Alt_4[i] > database$T_Alt_2[i]+sigma)
                      {database$P_Alt2[i] <- C3}
                   else {database$P_Alt2[i] <- C4}}}}
  
  ## Probabilidad Alt_4
  if (database$T_Alt_4[i] < database$T_rf_Alt4[i])
      {if (database$T_Alt_4[i] < database$T_rf_Alt4[i]-3*sigma)
          {database$P_Alt4[i] <- B1}
      else {if (database$T_Alt_4[i] < database$T_rf_Alt4[i]-2*sigma)
                {database$P_Alt4[i] <- B2}
            else {if (database$T_Alt_4[i] < database$T_rf_Alt4[i]-sigma)
                      {database$P_Alt4[i] <- B3}
                  else {database$P_Alt4[i] <- B4}}}}
  else {if (database$T_Alt_4[i] > database$T_rf_Alt4[i]+3*sigma)
            {database$P_Alt4[i] <- C1}
        else {if (database$T_Alt_4[i] > database$T_rf_Alt4[i]+2*sigma)
                  {database$P_Alt4[i] <- C2}
              else {if (database$T_Alt_4[i] > database$T_rf_Alt4[i]+sigma)
                        {database$P_Alt4[i] <- C3}
                    else {database$P_Alt4[i] <- C4}}}}
}

## Función de peso

for (i in 1:nrow(database)){
  
  # Pi_Alt_1
  if (database$T_Alt_4[i] < database$T_Alt_1[i])
    {if (database$P_Alt1[i] == B1)
        {database$Pi_Alt1[i] = ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}
    if (database$P_Alt1[i] == B2)
      {database$Pi_Alt1[i] = ((B1+B2)**gamma)/(((B1+B2)**gamma+(1-(B1+B2))**gamma)**(1/gamma)) - ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}
    if (database$P_Alt1[i] == B3)
      {database$Pi_Alt1[i] = ((B1+B2+B3)**gamma)/(((B1+B2+B3)**gamma+(1-(B1+B2+B3))**gamma)**(1/gamma)) - ((B1+B2)**gamma)/(((B1+B2)**gamma+(1-(B1+B2))**gamma)**(1/gamma))}
    if (database$P_Alt1[i] == B4)
      {database$Pi_Alt1[i] = ((B1+B2+B3+B4)**gamma)/(((B1+B2+B3+B4)**gamma+(1-(B1+B2+B3+B4))**gamma)**(1/gamma)) - ((B1+B2+B3)**gamma)/(((B1+B2+B3)**gamma+(1-(B1+B2+B3))**gamma)**(1/gamma))}}
  else {if (database$P_Alt1[i] == C1)
            {database$Pi_Alt1[i] = ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}
        if (database$P_Alt1[i] == C2)
            {database$Pi_Alt1[i] = ((C1+C2)**delta)/(((C1+C2)**delta+(1-(C1+C2))**delta)**(1/delta)) - ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}
        if (database$P_Alt1[i] == C3)
            {database$Pi_Alt1[i] = ((C1+C2+C3)**delta)/(((C1+C2+C3)**delta+(1-(C1+C2+C3))**delta)**(1/delta)) - ((C1+C2)**delta)/(((C1+C2)**delta+(1-(C1+C2))**delta)**(1/delta))}
        if (database$P_Alt1[i] == C4)
            {database$Pi_Alt1[i] = ((C1+C2+C3+C4)**delta)/(((C1+C2+C3+C4)**delta+(1-(C1+C2+C3+C4))**delta)**(1/delta)) - ((C1+C2+C3)**delta)/(((C1+C2+C3)**delta+(1-(C1+C2+C3))**delta)**(1/delta))}}

  ## Pi_Alt_2
  if (database$T_Alt_4[i] < database$T_Alt_2[i])
    {if (database$P_Alt2[i] == B1)
      {database$Pi_Alt2[i] = ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}
    if (database$P_Alt2[i] == B2)
    {database$Pi_Alt2[i] = ((B1+B2)**gamma)/(((B1+B2)**gamma+(1-(B1+B2))**gamma)**(1/gamma)) - ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}
    if (database$P_Alt2[i] == B3)
    {database$Pi_Alt2[i] = ((B1+B2+B3)**gamma)/(((B1+B2+B3)**gamma+(1-(B1+B2+B3))**gamma)**(1/gamma)) - ((B1+B2)**gamma)/(((B1+B2)**gamma+(1-(B1+B2))**gamma)**(1/gamma))}
    if (database$P_Alt2[i] == B4)
    {database$Pi_Alt2[i] = ((B1+B2+B3+B4)**gamma)/(((B1+B2+B3+B4)**gamma+(1-(B1+B2+B3+B4))**gamma)**(1/gamma)) - ((B1+B2+B3)**gamma)/(((B1+B2+B3)**gamma+(1-(B1+B2+B3))**gamma)**(1/gamma))}}
  else {if (database$P_Alt2[i] == C1)
  {database$Pi_Alt2[i] = ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}
    if (database$P_Alt2[i] == C2)
    {database$Pi_Alt2[i] = ((C1+C2)**delta)/(((C1+C2)**delta+(1-(C1+C2))**delta)**(1/delta)) - ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}
    if (database$P_Alt2[i] == C3)
    {database$Pi_Alt2[i] = ((C1+C2+C3)**delta)/(((C1+C2+C3)**delta+(1-(C1+C2+C3))**delta)**(1/delta)) - ((C1+C2)**delta)/(((C1+C2)**delta+(1-(C1+C2))**delta)**(1/delta))}
    if (database$P_Alt2[i] == C4)
    {database$Pi_Alt2[i] = ((C1+C2+C3+C4)**delta)/(((C1+C2+C3+C4)**delta+(1-(C1+C2+C3+C4))**delta)**(1/delta)) - ((C1+C2+C3)**delta)/(((C1+C2+C3)**delta+(1-(C1+C2+C3))**delta)**(1/delta))}}
  
  
  ## Pi_Alt_4
  if (database$T_Alt_4[i] < database$T_rf_Alt4[i])
  {if (database$P_Alt4[i] == B1)
  {database$Pi_Alt4[i] = ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}
    if (database$P_Alt4[i] == B2)
    {database$Pi_Alt4[i] = ((B1+B2)**gamma)/(((B1+B2)**gamma+(1-(B1+B2))**gamma)**(1/gamma)) - ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}
    if (database$P_Alt4[i] == B3)
    {database$Pi_Alt4[i] = ((B1+B2+B3)**gamma)/(((B1+B2+B3)**gamma+(1-(B1+B2+B3))**gamma)**(1/gamma)) - ((B1+B2)**gamma)/(((B1+B2)**gamma+(1-(B1+B2))**gamma)**(1/gamma))}
    if (database$P_Alt4[i] == B4)
    {database$Pi_Alt4[i] = ((B1+B2+B3+B4)**gamma)/(((B1+B2+B3+B4)**gamma+(1-(B1+B2+B3+B4))**gamma)**(1/gamma)) - ((B1+B2+B3)**gamma)/(((B1+B2+B3)**gamma+(1-(B1+B2+B3))**gamma)**(1/gamma))}}
  else {if (database$P_Alt1[i] == C1)
  {database$Pi_Alt4[i] = ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}
    if (database$P_Alt4[i] == C2)
    {database$Pi_Alt4[i] = ((C1+C2)**delta)/(((C1+C2)**delta+(1-(C1+C2))**delta)**(1/delta)) - ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}
    if (database$P_Alt4[i] == C3)
    {database$Pi_Alt4[i] = ((C1+C2+C3)**delta)/(((C1+C2+C3)**delta+(1-(C1+C2+C3))**delta)**(1/delta)) - ((C1+C2)**delta)/(((C1+C2)**delta+(1-(C1+C2))**delta)**(1/delta))}
    if (database$P_Alt4[i] == C4)
    {database$Pi_Alt4[i] = ((C1+C2+C3+C4)**delta)/(((C1+C2+C3+C4)**delta+(1-(C1+C2+C3+C4))**delta)**(1/delta)) - ((C1+C2+C3)**delta)/(((C1+C2+C3)**delta+(1-(C1+C2+C3))**delta)**(1/delta))}}
  
}
  

# ################################################################# #
#### DEFINE PARAMETROS DEL MODELO                                ####
# ################################################################# #

### Vector de parametros, incluidos los que se mantienen fijos en la estimación
apollo_beta=c(asc_ruta1 = 0, asc_ruta2 = 0, asc_ruta4 = 0,
              b_gain  = 0,  b_loss = 0,
              b_dt  = 0,
              b_Sem = 0,
              b_ACC_0 = 0, b_ACC_1 = -1, b_ACC_2 = 0,
              b_NO_CAMFD = 0, b_SI_CAMFD = 0, 
              b_NO_PANEL = 0, b_SI_PANEL = -1, 
              b_NO_ZER = 0, b_SI_ZER = 0, 
              b_No_MTRP = 0, b_Si_MTRP = 0,
              b_No_Info = 0, b_Si_Info = 0,
              b_UsoCel_P = 0, #b_UsoCel_A = 0, 
              b_UsoCel_F = 0)
              

### Vector con nombres (entre comillas) de los parámetros que se mantendrán fijos en su valor inicial en apollo_beta, use apollo_beta_fixed = c () si ninguno
apollo_fixed = c("asc_ruta2", "b_ACC_0", "b_NO_CAMFD","b_NO_PANEL" ,"b_No_MTRP", "b_NO_ZER", 
                 "b_Si_Info", "b_UsoCel_P")


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
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['ruta1']]  = asc_ruta1  + b_gain*(VP1) + b_loss*(VN1) + 
    b_dt * D_Alt_1 + b_Sem*SEM_A1_km + 
    b_ACC_0*ACC_A1_0 + b_ACC_1*ACC_A1_1 + b_ACC_2*ACC_A1_2 + 
    b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +
    b_NO_PANEL * NO_PANEL_A1 + b_SI_PANEL * SI_PANEL_A1 + 
    b_NO_ZER * NO_ZER_A1 + b_SI_ZER * SI_ZER_A1 + 
    b_No_MTRP * NO_MTRP_A1 + b_Si_MTRP * SI_MTRP_A1
  
  V[['ruta2']]  = asc_ruta2  + b_gain*(VP2) + b_loss*(VN2) + 
    b_dt * D_Alt_2 + b_Sem*SEM_A2_km + 
    b_ACC_0*ACC_A2_0 + b_ACC_1*ACC_A2_1 + b_ACC_2*ACC_A2_2 + 
    b_NO_CAMFD * NO_CAMFD_A2 + b_SI_CAMFD * SI_CAMFD_A2 +
    b_NO_PANEL * NO_PANEL_A2 + b_SI_PANEL * SI_PANEL_A2 + 
    b_NO_ZER * NO_ZER_A2 + b_SI_ZER * SI_ZER_A2 + 
    b_No_MTRP * NO_MTRP_A2 + b_Si_MTRP * SI_MTRP_A2
  
  
  V[['rutaEC']] =  asc_ruta4 + b_gain*(VP4) + b_loss*(VN4) + 
    b_dt * D_Alt_4 + b_Sem*SEM_EC_km +
    b_ACC_0*ACC_EC_0 + b_ACC_1*ACC_EC_1 + b_ACC_2*ACC_EC_2 + 
    b_NO_PANEL * NO_PANEL_EC + b_SI_PANEL * SI_PANEL_EC + 
    b_NO_ZER * NO_ZER_EC + b_SI_ZER * SI_ZER_EC + 
    b_No_MTRP * NO_MTRP_EC + b_Si_MTRP * SI_MTRP_EC +
    b_No_Info * SININFOTRF + b_Si_Info * CONINFOTRF +
    b_UsoCel_P * UsoCel_Poco +  b_UsoCel_F * UsoCel_Frec
 
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(ruta1=1, ruta2=2, rutaEC=4), 
    avail         = list(ruta1=1, ruta2=1, rutaEC=1), 
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
#### MODEL ESTIMATION                                            ####
# ################################################################# #

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)


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

database$V1 = database$VP1 + database$VN1
database$V2 = database$VP2 + database$VN2
database$V4 = database$VP4 + database$VN4
plot(database$DT1,database$V1)
plot(database$DT2,database$V2)
plot(database$DT4,database$V4)

