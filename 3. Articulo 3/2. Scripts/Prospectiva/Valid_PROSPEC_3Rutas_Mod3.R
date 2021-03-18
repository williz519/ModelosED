library(readxl)

# Modelo MNL

#Cargas las Bases de Datos
rm(list = ls())

database = read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBMuestraArt3_3Rutas.csv",sep="\t", dec=".",header=TRUE)

db_test = read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBValidacion_3Rutas.csv",sep="\t", dec=".",header=TRUE)

resultados <-  read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/4. Resultados/Prospectiva/3Rutas/Modelo_3/Modelo_3_Prospec_Simple_3Rutas_estimates.csv",sep=",", dec=".",header=TRUE )

### 
## Parametros Teoría Prospectiva

for (i in 1:nrow(database)){ 
  alpha = 0.88
  beta  = 0.88
  lamda = 2.25
  gamma = 0.61
  delta = 0.69
  sigma = 0.08
  phi   = 0.24
  theta = 0
  eta   = 0.0       # Estan Calibrados ... No Cambiar
  eta1  = 0.0         # No cambiar
  
  # Ruta 1
  database$DT1[i] <- database$T_Alt_1[i] - (database$T_rf_Alt1[i]+ eta)
  #database$DT1[i] <- database$T_Alt_4[i] - database$T_Alt_1[i]
  
  if(database$DT1[i] < 0)
  {if (database$DT1[i] < -phi)
  {database$VN1[i]<- -lamda*(-database$DT1[i])**beta; database$VP1[i]<- 0}
    else {database$VP1[i]<- -(-database$DT1[i])**alpha; database$VN1[i]<- 0}}
  else
  {if (database$DT1[i] >= sigma)
  {database$VN1[i] <- -lamda*(database$DT1[i])**beta; database$VP1[i]<- 0}
    else {database$VP1[i] <- -(database$DT1[i])**alpha; database$VN1[i]<- 0}}
  
  # Ruta 2
  database$DT2[i] <- database$T_Alt_2[i] - (database$T_rf_Alt2[i] + eta)
  #database$DT2[i] <- database$T_Alt_4[i]- database$T_Alt_2[i]
  
  if(database$DT2[i] < 0)
  {if (database$DT2[i] < -phi)
  {database$VN2[i]<- -lamda*(-database$DT2[i])**beta; database$VP2[i]<- 0}
    else {database$VP2[i]<- -(-database$DT2[i])**alpha; database$VN2[i]<- 0}}
  else
  {if (database$DT2[i] >= sigma)
  {database$VN2[i] <- -lamda*(database$DT2[i])**beta; database$VP2[i]<- 0}
    else {database$VP2[i] <- -(database$DT2[i])**alpha; database$VN2[i]<- 0}}
  
  # Ruta 4
  database$DT4[i] <- database$T_Alt_4[i] - (database$T_rf_Alt4[i]+ eta1)
  
  if(database$DT4[i] < 0)
  {if (database$DT4[i] < -phi)
  {database$VN4[i]<- -lamda*(-database$DT4[i])**beta; database$VP4[i]<- 0}
    else {database$VP4[i]<- -(-database$DT4[i])**alpha; database$VN4[i]<- 0}}
  else
  {if (database$DT4[i] >= sigma + theta)
  {database$VN4[i] <- -lamda*(database$DT4[i])**beta; database$VP4[i]<- 0}
    else {database$VP4[i] <- -(database$DT4[i])**alpha; database$VN4[i]<- 0}}
}


## Probabilidades 

for (i in 1:nrow(database)){ 
  B1 = 0.4
  B2 = 0.25
  C2 = 0.15
  C1 = 0.2
  ## Probabilidad Alt_1
  if (database$T_Alt_4[i] < database$T_Alt_1[i])
  {if (database$T_Alt_4[i] < database$T_Alt_1[i]-phi)
  {database$P_Alt1[i] <- B1}
    else {database$P_Alt1[i] <- B2}}
  else {if (database$T_Alt_4[i] > database$T_Alt_1[i]+sigma)
  {database$P_Alt1[i] <- C1}
    else {database$P_Alt1[i] <- C2}}
  
  ## Probabilidad Alt_2
  if (database$T_Alt_4[i] < database$T_Alt_2[i])
  {if (database$T_Alt_4[i] < database$T_Alt_2[i]-phi)
  {database$P_Alt2[i] <- B1}
    else {database$P_Alt2[i] <- B2}}
  else {if (database$T_Alt_4[i] > database$T_Alt_2[i]+sigma)
  {database$P_Alt2[i] <- C1}
    else {database$P_Alt2[i] <- C2}}
  
  ## Probabilidad Alt_4
  if (database$T_Alt_4[i] < database$T_rf_Alt4[i])
  {if (database$T_Alt_4[i] < database$T_rf_Alt4[i]-phi)
  {database$P_Alt4[i] <- B1}
    else {database$P_Alt4[i] <- B2}}
  else {if (database$T_Alt_4[i] > database$T_rf_Alt4[i]+3*sigma)
  {database$P_Alt4[i] <- C1}
    else {database$P_Alt4[i] <- C2}}
}

###################################################################################

## Función de ponderación  Kahneman & Tverky

for (i in 1:nrow(database)){
  # Pi_Alt_1
  if (database$T_Alt_4[i] < database$T_Alt_1[i])
  {if (database$P_Alt1[i] == B1)
  {database$Pi_Alt1[i] = ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}
    if (database$P_Alt1[i] == B2)
    {database$Pi_Alt1[i] = ((B1+B2)**gamma)/(((B1+B2)**gamma+(1-(B1+B2))**gamma)**(1/gamma)) - ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}}
  else {if (database$P_Alt1[i] == C1)
  {database$Pi_Alt1[i] = ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}
    if (database$P_Alt1[i] == C2)
    {database$Pi_Alt1[i] = ((C1+C2)**delta)/(((C1+C2)**delta+(1-(C1+C2))**delta)**(1/delta)) - ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}}
  
  ## Pi_Alt_2
  if (database$T_Alt_4[i] < database$T_Alt_2[i])
  {if (database$P_Alt2[i] == B1)
  {database$Pi_Alt2[i] = ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}
    if (database$P_Alt2[i] == B2)
    {database$Pi_Alt2[i] = ((B1+B2)**gamma)/(((B1+B2)**gamma+(1-(B1+B2))**gamma)**(1/gamma)) - ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}}
  else {if (database$P_Alt2[i] == C1)
  {database$Pi_Alt2[i] = ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}
    if (database$P_Alt2[i] == C2)
    {database$Pi_Alt2[i] = ((C1+C2)**delta)/(((C1+C2)**delta+(1-(C1+C2))**delta)**(1/delta)) - ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}}
  
  ## Pi_Alt_4
  if (database$T_Alt_4[i] < database$T_rf_Alt4[i])
  {if (database$P_Alt4[i] == B1)
  {database$Pi_Alt4[i] = ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}
    if (database$P_Alt4[i] == B2)
    {database$Pi_Alt4[i] = ((B1+B2)**gamma)/(((B1+B2)**gamma+(1-(B1+B2))**gamma)**(1/gamma)) - ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}}
  else {if (database$P_Alt1[i] == C1)
  {database$Pi_Alt4[i] = ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}
    if (database$P_Alt4[i] == C2)
    {database$Pi_Alt4[i] = ((C1+C2)**delta)/(((C1+C2)**delta+(1-(C1+C2))**delta)**(1/delta)) - ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}}
}



#### VALIDACION DEL MODELO ##########

## Aciertos sobre la base del modelo
db <- database
ce <- resultados$Estimate        # coefficients b1, b2

V_1 = array()
V_2 = array()
V_4 = array()


V_1 = ce[1]  + 
  ce[4]*(db$Pi_Alt1*db$VP1*(1 + db$CG_Alt_1)) + ce[5]*(db$Pi_Alt1*db$VN1*(1 + db$CG_Alt_1)) + 
  ce[6] * db$D_Alt_1 + ce[7]*db$SEM_A1_km + 
  ce[8] * db$ACC_A1_0 + ce[9]*db$ACC_A1_1 + ce[10]*db$ACC_A1_2 + 
  ce[11] * db$NO_CAMFD_A1 + ce[12] * db$SI_CAMFD_A1 +
  ce[13] * db$NO_PANEL_A1 + ce[14] * db$SI_PANEL_A1 + 
  ce[15] * db$NO_ZER_A1 + ce[16] * db$SI_ZER_A1 + 
  ce[17] * db$NO_MTRP_A1 + ce[18] * db$SI_MTRP_A1


V_2 = ce[2]  + 
  ce[4]*(db$Pi_Alt2*db$VP2*(1 + db$CG_Alt_2)) + ce[5]*(db$Pi_Alt2*db$VN2*(1 + db$CG_Alt_2)) + 
  ce[6] * db$D_Alt_2 + ce[7]*db$SEM_A2_km + 
  ce[8] * db$ACC_A2_0 + ce[9]* db$ACC_A2_1 + ce[10]* db$ACC_A2_2 + 
  ce[11] * db$NO_CAMFD_A2 + ce[12] * db$SI_CAMFD_A2 +
  ce[13] * db$NO_PANEL_A2 + ce[14] * db$SI_PANEL_A2 + 
  ce[15] * db$NO_ZER_A2 + ce[16] * db$SI_ZER_A2 + 
  ce[17] * db$NO_MTRP_A2 + ce[18] * db$SI_MTRP_A2


V_4 = ce[3] + 
  ce[4]*(db$Pi_Alt4*db$VP4*(1 + db$CG_Alt_4)) + ce[5]*(db$Pi_Alt4*db$VN4*(1 + db$CG_Alt_4)) +
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

Prob <- cbind(P_r1, P_r2, P_r4, db$CHOICE, Eleccion )
Prob
table(Real = db$CHOICE, predicted = Eleccion)
mean(factor(Eleccion, ordered = TRUE) == db$CHOICE)


Umbral = array()
err = 0.1

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


## Parametros Teoría Prospectiva

for (i in 1:nrow(database)){ 
  alpha = 0.88
  beta  = 0.88
  lamda = 2.25
  gamma = 0.61
  delta = 0.69
  sigma = 0.1
  phi   = 0.18
  theta = 0
  eta   = 0.15        # Estan Calibrados ... No Cambiar
  eta1  = 0.0         # No cambiar
  
  # Ruta 1
  database$DT1[i] <- database$T_Alt_1[i] - (database$T_rf_Alt1[i]+ eta)
  #database$DT1[i] <- database$T_Alt_4[i] - database$T_Alt_1[i]
  
  if(database$DT1[i] < 0)
  {if (database$DT1[i] < -phi)
  {database$VN1[i]<- -lamda*(-database$DT1[i])**beta; database$VP1[i]<- 0}
    else {database$VP1[i]<- -(-database$DT1[i])**alpha; database$VN1[i]<- 0}}
  else
  {if (database$DT1[i] >= sigma)
  {database$VN1[i] <- -lamda*(database$DT1[i])**beta; database$VP1[i]<- 0}
    else {database$VP1[i] <- -(database$DT1[i])**alpha; database$VN1[i]<- 0}}
  
  # Ruta 2
  database$DT2[i] <- database$T_Alt_2[i] - (database$T_rf_Alt2[i] + eta)
  #database$DT2[i] <- database$T_Alt_4[i]- database$T_Alt_2[i]
  
  if(database$DT2[i] < 0)
  {if (database$DT2[i] < -phi)
  {database$VN2[i]<- -lamda*(-database$DT2[i])**beta; database$VP2[i]<- 0}
    else {database$VP2[i]<- -(-database$DT2[i])**alpha; database$VN2[i]<- 0}}
  else
  {if (database$DT2[i] >= sigma)
  {database$VN2[i] <- -lamda*(database$DT2[i])**beta; database$VP2[i]<- 0}
    else {database$VP2[i] <- -(database$DT2[i])**alpha; database$VN2[i]<- 0}}
  
  # Ruta 4
  database$DT4[i] <- database$T_Alt_4[i] - (database$T_rf_Alt4[i]+ eta1)
  
  if(database$DT4[i] < 0)
  {if (database$DT4[i] < -phi)
  {database$VN4[i]<- -lamda*(-database$DT4[i])**beta; database$VP4[i]<- 0}
    else {database$VP4[i]<- -(-database$DT4[i])**alpha; database$VN4[i]<- 0}}
  else
  {if (database$DT4[i] >= sigma + theta)
  {database$VN4[i] <- -lamda*(database$DT4[i])**beta; database$VP4[i]<- 0}
    else {database$VP4[i] <- -(database$DT4[i])**alpha; database$VN4[i]<- 0}}
}


## Probabilidades 

for (i in 1:nrow(database)){ 
  B1 = 0.4
  B2 = 0.25
  C2 = 0.15
  C1 = 0.2
  ## Probabilidad Alt_1
  if (database$T_Alt_4[i] < database$T_Alt_1[i])
  {if (database$T_Alt_4[i] < database$T_Alt_1[i]-phi)
  {database$P_Alt1[i] <- B1}
    else {database$P_Alt1[i] <- B2}}
  else {if (database$T_Alt_4[i] > database$T_Alt_1[i]+sigma)
  {database$P_Alt1[i] <- C1}
    else {database$P_Alt1[i] <- C2}}
  
  ## Probabilidad Alt_2
  if (database$T_Alt_4[i] < database$T_Alt_2[i])
  {if (database$T_Alt_4[i] < database$T_Alt_2[i]-phi)
  {database$P_Alt2[i] <- B1}
    else {database$P_Alt2[i] <- B2}}
  else {if (database$T_Alt_4[i] > database$T_Alt_2[i]+sigma)
  {database$P_Alt2[i] <- C1}
    else {database$P_Alt2[i] <- C2}}
  
  ## Probabilidad Alt_4
  if (database$T_Alt_4[i] < database$T_rf_Alt4[i])
  {if (database$T_Alt_4[i] < database$T_rf_Alt4[i]-phi)
  {database$P_Alt4[i] <- B1}
    else {database$P_Alt4[i] <- B2}}
  else {if (database$T_Alt_4[i] > database$T_rf_Alt4[i]+3*sigma)
  {database$P_Alt4[i] <- C1}
    else {database$P_Alt4[i] <- C2}}
}

###################################################################################

## Función de ponderación  Kahneman & Tverky

for (i in 1:nrow(database)){
  # Pi_Alt_1
  if (database$T_Alt_4[i] < database$T_Alt_1[i])
  {if (database$P_Alt1[i] == B1)
  {database$Pi_Alt1[i] = ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}
    if (database$P_Alt1[i] == B2)
    {database$Pi_Alt1[i] = ((B1+B2)**gamma)/(((B1+B2)**gamma+(1-(B1+B2))**gamma)**(1/gamma)) - ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}}
  else {if (database$P_Alt1[i] == C1)
  {database$Pi_Alt1[i] = ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}
    if (database$P_Alt1[i] == C2)
    {database$Pi_Alt1[i] = ((C1+C2)**delta)/(((C1+C2)**delta+(1-(C1+C2))**delta)**(1/delta)) - ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}}
  
  ## Pi_Alt_2
  if (database$T_Alt_4[i] < database$T_Alt_2[i])
  {if (database$P_Alt2[i] == B1)
  {database$Pi_Alt2[i] = ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}
    if (database$P_Alt2[i] == B2)
    {database$Pi_Alt2[i] = ((B1+B2)**gamma)/(((B1+B2)**gamma+(1-(B1+B2))**gamma)**(1/gamma)) - ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}}
  else {if (database$P_Alt2[i] == C1)
  {database$Pi_Alt2[i] = ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}
    if (database$P_Alt2[i] == C2)
    {database$Pi_Alt2[i] = ((C1+C2)**delta)/(((C1+C2)**delta+(1-(C1+C2))**delta)**(1/delta)) - ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}}
  
  ## Pi_Alt_4
  if (database$T_Alt_4[i] < database$T_rf_Alt4[i])
  {if (database$P_Alt4[i] == B1)
  {database$Pi_Alt4[i] = ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}
    if (database$P_Alt4[i] == B2)
    {database$Pi_Alt4[i] = ((B1+B2)**gamma)/(((B1+B2)**gamma+(1-(B1+B2))**gamma)**(1/gamma)) - ((B1)**gamma)/((B1**gamma+(1-B1)**gamma)**(1/gamma))}}
  else {if (database$P_Alt1[i] == C1)
  {database$Pi_Alt4[i] = ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}
    if (database$P_Alt4[i] == C2)
    {database$Pi_Alt4[i] = ((C1+C2)**delta)/(((C1+C2)**delta+(1-(C1+C2))**delta)**(1/delta)) - ((C1)**delta)/((C1**delta+(1-C1)**delta)**(1/delta))}}
}


db <- database

VT_1 = array()
VT_2 = array()
VT_4 = array()

VT_1 = ce[1]  + 
  ce[4]*(db$Pi_Alt1*db$VP1*(1 + db$CG_Alt_1)) + ce[5]*(db$Pi_Alt1*db$VN1*(1 + db$CG_Alt_1)) + 
  ce[6] * db$D_Alt_1 + ce[7]*db$SEM_A1_km + 
  ce[8] * db$ACC_A1_0 + ce[9]*db$ACC_A1_1 + ce[10]*db$ACC_A1_2 + 
  ce[11] * db$NO_CAMFD_A1 + ce[12] * db$SI_CAMFD_A1 +
  ce[13] * db$NO_PANEL_A1 + ce[14] * db$SI_PANEL_A1 + 
  ce[15] * db$NO_ZER_A1 + ce[16] * db$SI_ZER_A1 + 
  ce[17] * db$NO_MTRP_A1 + ce[18] * db$SI_MTRP_A1


VT_2 = ce[2]  + 
  ce[4]*(db$Pi_Alt2*db$VP2*(1 + db$CG_Alt_2)) + ce[5]*(db$Pi_Alt2*db$VN2*(1 + db$CG_Alt_2)) + 
  ce[6] * db$D_Alt_2 + ce[7]*db$SEM_A2_km + 
  ce[8] * db$ACC_A2_0 + ce[9]* db$ACC_A2_1 + ce[10]* db$ACC_A2_2 + 
  ce[11] * db$NO_CAMFD_A2 + ce[12] * db$SI_CAMFD_A2 +
  ce[13] * db$NO_PANEL_A2 + ce[14] * db$SI_PANEL_A2 + 
  ce[15] * db$NO_ZER_A2 + ce[16] * db$SI_ZER_A2 + 
  ce[17] * db$NO_MTRP_A2 + ce[18] * db$SI_MTRP_A2


VT_4 = ce[3] + 
  ce[4]*(db$Pi_Alt4*db$VP4*(1 + db$CG_Alt_4)) + ce[5]*(db$Pi_Alt4*db$VN4*(1 + db$CG_Alt_4)) +
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

#ProbT <- cbind(PT_r1, PT_r2, PT_r4)
#ProbT
table(predicted = EleccionTest, Real = db$CHOICE)
mean(factor(EleccionTest, ordered = TRUE) == db$CHOICE)


UmbralTest = array()
errT = 0.1

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

table(Real = db$CHOICE, predicted = UmbralTest)
mean(factor(UmbralTest, ordered = TRUE) == db$CHOICE)


