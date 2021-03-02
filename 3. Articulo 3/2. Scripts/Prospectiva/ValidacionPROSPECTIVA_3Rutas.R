library(readxl)

# Modelo MNL

#Cargas las Bases de Datos
rm(list = ls())

database = read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBMuestraArt3_3Rutas.csv",sep="\t", dec=".",header=TRUE)

db_test = read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBValidacion_3Rutas.csv",sep="\t", dec=".",header=TRUE)

resultados <-  read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/4. Resultados/Prospectiva/Sigma0.1/Prospective Simple 3Rutas_estimates.csv",sep=",", dec=".",header=TRUE )

### 
## Parametros Teoría Prospectiva


for (i in 1:nrow(database)){ 
  alpha = 0.72
  beta  = 0.74
  lamda = 3.2
  sigma = 0.1
  gamma = 0.61
  delta = 0.69
  theta = 0
  eta = 0.18
  
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
  
  database$DT4[i] <- database$T_Alt_4[i] - (database$T_rf_Alt4[i]+ eta)
  
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


#### VALIDACION DEL MODELO ##########


## Aciertos sobre la base del modelo
db <- database
ce <- resultados$Estimate        # coefficients b1, b2

V_1 = array()
V_2 = array()
V_4 = array()


V_1 = ce[1]  + ce[4]*(db$VP1) + ce[5]*(db$VN1) + 
  ce[6] * db$D_Alt_1 + ce[7]*db$SEM_A1_km + 
  ce[8] * db$ACC_A1_0 + ce[9]*db$ACC_A1_1 + ce[10]*db$ACC_A1_2 + 
  ce[11] * db$NO_CAMFD_A1 + ce[12] * db$SI_CAMFD_A1 +
  ce[13] * db$NO_PANEL_A1 + ce[14] * db$SI_PANEL_A1 + 
  ce[15] * db$NO_ZER_A1 + ce[16] * db$SI_ZER_A1 + 
  ce[17] * db$NO_MTRP_A1 + ce[18] * db$SI_MTRP_A1


V_2 = ce[2]  + ce[4]*(db$VP2) + ce[5]*(db$VN2) + 
  ce[6] * db$D_Alt_2 + ce[7]*db$SEM_A2_km + 
  ce[8] * db$ACC_A2_0 + ce[9]* db$ACC_A2_1 + ce[10]* db$ACC_A2_2 + 
  ce[11] * db$NO_CAMFD_A2 + ce[12] * db$SI_CAMFD_A2 +
  ce[13] * db$NO_PANEL_A2 + ce[14] * db$SI_PANEL_A2 + 
  ce[15] * db$NO_ZER_A2 + ce[16] * db$SI_ZER_A2 + 
  ce[17] * db$NO_MTRP_A2 + ce[18] * db$SI_MTRP_A2


V_4 = ce[3] + ce[4]*(db$VP4) + ce[5]*(db$VN4) +
  ce[6] * db$D_Alt_4 + ce[7] * db$SEM_EC_km +
  ce[8] * db$ACC_EC_0 + ce[9] * db$ACC_EC_1 + ce[10] * db$ACC_EC_2 + 
  ce[11] * db$NO_CAMFD_EC + ce[12] * db$SI_CAMFD_EC +
  ce[13] * db$NO_PANEL_EC + ce[14] * db$SI_PANEL_EC + 
  ce[15] * db$NO_ZER_EC + ce[16] * db$SI_ZER_EC + 
  ce[17] * db$NO_MTRP_EC + ce[18] * db$SI_MTRP_EC +
  ce[19] * db$SININFOTRF + ce[20] * db$CONINFOTRF +
  ce[21] * db$UsoCel_Poco +  ce[22] * db$UsoCel_Frec 
  

P_r1 = array()
P_r2 = array()
P_r4 = array()

for (i in 1:nrow(db)){
  P_r1[i] = exp(V_1[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_4[i]))
  P_r2[i] = exp(V_2[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_4[i]))
  P_r4[i] = exp(V_4[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_4[i]))}

db$P_r1 <- P_r1
db$P_r2 <- P_r2
db$P_r4 <- P_r4


Eleccion = array()

for (i in 1:nrow(db)){
  if (max(db$P_r1[i],db$P_r2[i],db$P_r4[i]) == db$P_r1[i]) {Eleccion[i] = 1}
  else
  {if (max(db$P_r1[i],db$P_r2[i],db$P_r4[i]) == db$P_r2[i]) {Eleccion[i] = 2}
    else
      {Eleccion[i] = 4
      }}}

Prob <- cbind(P_r1, P_r2, P_r4)
Prob
table(Real = db$CHOICE, predicted = Eleccion)
mean(factor(Eleccion, ordered = TRUE) == db$CHOICE)


Umbral = array()
err = 0.15

for (i in 1:nrow(db)){
  if (db$CHOICE[i] != Eleccion[i]){
    if (Eleccion[i] == 1){
      if (abs(db$P_r1[i]-db$P_r2[i]) < err){Umbral[i] = 2}
      else
      {if (abs(db$P_r1[i]-db$P_r4[i]) < err){Umbral[i] = 4}
        else
          {Umbral[i] = 1}}}
    else {if (Eleccion[i] == 2){
      if (abs(db$P_r2[i]-db$P_r1[i]) < err){Umbral[i] = 1}
      else
      {if (abs(db$P_r2[i]-db$P_r4[i]) < err){Umbral[i] = 4}
        else
          {Umbral[i] = 2}}}
      else
      { if (Eleccion[i] == 4){
        if (abs(db$P_r4[i]-db$P_r1[i]) < err){Umbral[i] = 1}
        else
        {if (abs(db$P_r4[i]-db$P_r2[i]) < err){Umbral[i] = 2}
          else
            {Umbral[i] = 4}}}}}}
  else
  {Umbral[i] = db$CHOICE[i]}}

#Umbral
#Eleccion
#NewProb <- cbind(P_r1, P_r2, P_r4, db$CHOICE, Eleccion, Umbral)
#NewProb

table(Real = db$CHOICE, predicted = Umbral)
mean(factor(Umbral, ordered = TRUE) == db$CHOICE)



#### VALIDACION DEL MODELO EN TESTING  ##########

## Aciertos sobre la base de testeo

database <- db_test


for (i in 1:nrow(database)){
  
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
  
  database$DT4[i] <- database$T_Alt_4[i] - (database$T_rf_Alt4[i]+ eta)
  
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

db <- database

VT_1 = array()
VT_2 = array()
VT_4 = array()

VT_1 = ce[1]  + ce[4]*(db$VP1) + ce[5]*(db$VN1) + 
  ce[6] * db$D_Alt_1 + ce[7]*db$SEM_A1_km + 
  ce[8] * db$ACC_A1_0 + ce[9]*db$ACC_A1_1 + ce[10]*db$ACC_A1_2 + 
  ce[11] * db$NO_CAMFD_A1 + ce[12] * db$SI_CAMFD_A1 +
  ce[13] * db$NO_PANEL_A1 + ce[14] * db$SI_PANEL_A1 + 
  ce[15] * db$NO_ZER_A1 + ce[16] * db$SI_ZER_A1 + 
  ce[17] * db$NO_MTRP_A1 + ce[18] * db$SI_MTRP_A1


VT_2 = ce[2]  + ce[4]*(db$VP2) + ce[5]*(db$VN2) + 
  ce[6] * db$D_Alt_2 + ce[7]*db$SEM_A2_km + 
  ce[8] * db$ACC_A2_0 + ce[9]* db$ACC_A2_1 + ce[10]* db$ACC_A2_2 + 
  ce[11] * db$NO_CAMFD_A2 + ce[12] * db$SI_CAMFD_A2 +
  ce[13] * db$NO_PANEL_A2 + ce[14] * db$SI_PANEL_A2 + 
  ce[15] * db$NO_ZER_A2 + ce[16] * db$SI_ZER_A2 + 
  ce[17] * db$NO_MTRP_A2 + ce[18] * db$SI_MTRP_A2


VT_4 = ce[3] + ce[4]*(db$VP4) + ce[5]*(db$VN4) +
  ce[6] * db$D_Alt_4 + ce[7] * db$SEM_EC_km +
  ce[8] * db$ACC_EC_0 + ce[9] * db$ACC_EC_1 + ce[10] * db$ACC_EC_2 + 
  ce[11] * db$NO_CAMFD_EC + ce[12] * db$SI_CAMFD_EC +
  ce[13] * db$NO_PANEL_EC + ce[14] * db$SI_PANEL_EC + 
  ce[15] * db$NO_ZER_EC + ce[16] * db$SI_ZER_EC + 
  ce[17] * db$NO_MTRP_EC + ce[18] * db$SI_MTRP_EC +
  ce[19] * db$SININFOTRF + ce[20] * db$CONINFOTRF +
  ce[21] * db$UsoCel_Poco +  ce[22] * db$UsoCel_Frec


PT_r1 = array()
PT_r2 = array()
PT_r4 = array()

for (i in 1:nrow(db)){
  PT_r1[i] = exp(VT_1[i])/(exp(VT_1[i])+exp(VT_2[i])+exp(VT_4[i]))
  PT_r2[i] = exp(VT_2[i])/(exp(VT_1[i])+exp(VT_2[i])+exp(VT_4[i]))
  PT_r4[i] = exp(VT_4[i])/(exp(VT_1[i])+exp(VT_2[i])+exp(VT_4[i]))}

db$PT_r1 <- PT_r1
db$PT_r2 <- PT_r2
db$PT_r4 <- PT_r4


EleccionTest = array()

for (i in 1:nrow(db)){
  if (max(db$PT_r1[i],db$PT_r2[i],db$PT_r4[i]) == db$PT_r1[i]) {EleccionTest[i] = 1}
  else
  {if (max(db$PT_r1[i],db$PT_r2[i],db$PT_r4[i]) == db$PT_r2[i]) {EleccionTest[i] = 2}
    else
      {EleccionTest[i] = 4
      }}}

ProbT <- cbind(PT_r1, PT_r2, PT_r4)
ProbT
table(predicted = EleccionTest, Real = db$CHOICE)
mean(factor(EleccionTest, ordered = TRUE) == db$CHOICE)


UmbralTest = array()
errT = 0.15

for (i in 1:nrow(db)){
  if (db$CHOICE[i] != EleccionTest[i]){
    if (EleccionTest[i] == 1){
      if (abs(db$PT_r1[i]-db$PT_r2[i]) < errT){UmbralTest[i] = 2}
      else
      {if (abs(db$PT_r1[i]-db$PT_r4[i]) < err){UmbralTest[i] = 4}
        else
          {UmbralTest[i] = 1}}}
    else {if (EleccionTest[i] == 2){
      if (abs(db$PT_r2[i]-db$PT_r1[i]) < err){UmbralTest[i] = 1}
      else
      {if (abs(db$PT_r2[i]-db$PT_r4[i]) < err){UmbralTest[i] = 4}
        else
          {UmbralTest[i] = 2}}}
      else
      { if (EleccionTest[i] == 4){
        if (abs(db$PT_r4[i]-db$PT_r1[i]) < err){UmbralTest[i] = 1}
        else
        {if (abs(db$PT_r4[i]-db$PT_r2[i]) < err){UmbralTest[i] = 2}
          else
            {UmbralTest[i] = 4}}}}}}
  else
  {UmbralTest[i] = db$CHOICE[i]}}

#UmbralTest
#EleccionTest
#NewProbT <- cbind(PT_r1, PT_r2, PT_r4, db$CHOICE, EleccionTest, UmbralTest)
#NewProbT

table(predicted = UmbralTest, Real = db$CHOICE)
mean(factor(UmbralTest, ordered = TRUE) == db$CHOICE)


