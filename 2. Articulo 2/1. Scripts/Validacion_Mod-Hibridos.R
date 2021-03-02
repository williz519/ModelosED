
#### VALIDACION DEL MODELO ##########

### Limpiar memoria
rm(list = ls())

## Aciertos sobre la base del modelo

database <- read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Bases de datos/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

# Normalización de las variables tiempo
for (i in 1:nrow(database)){ 
  database$T_Alt_1[i] = (database$TIEMPOAlt1[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_2[i] = (database$TIEMPOAlt2[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_3[i] = (database$TIEMPOAlt3[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_4[i] = (database$TIEMPOEC[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  
  # Normalización de la variable distancia
  database$D_Alt_1[i] = (database$DISTAlt1[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_2[i] = (database$DISTAlt2[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_3[i] = (database$DISTAlt3[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0-1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_4[i] = (database$DISTEC[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  
  if(database$HPICOHVALLE[i] == 1){
    database$Vel_fl_Alt1[i] = 28/60;
    database$Vel_fl_Alt2[i] = 28/60;
    database$Vel_fl_Alt3[i] = 28/60;
    database$Vel_fl_Alt4[i] = 28/60}
  else{database$Vel_fl_Alt1[i] = 22/60;
  database$Vel_fl_Alt2[i] = 22/60;
  database$Vel_fl_Alt3[i]= 22/60; 
  database$Vel_fl_Alt4[i]= 22/60}
  
  database$T_fl_Alt1[i] = database$DISTAlt1[i]/database$Vel_fl_Alt1[i]
  database$T_fl_Alt2[i] = database$DISTAlt2[i]/database$Vel_fl_Alt2[i]
  database$T_fl_Alt3[i] = database$DISTAlt3[i]/database$Vel_fl_Alt3[i]
  database$T_fl_Alt4[i] = database$DISTEC[i]/database$Vel_fl_Alt4[i]
  
  database$CG_Alt_1[i] = database$TIEMPOAlt1[i]/database$T_fl_Alt1[i]
  database$CG_Alt_2[i] = database$TIEMPOAlt2[i]/database$T_fl_Alt2[i]
  database$CG_Alt_3[i] = database$TIEMPOAlt3[i]/database$T_fl_Alt3[i]
  database$CG_Alt_4[i] = database$TIEMPOEC[i]/database$T_fl_Alt4[i]
}

database$UsoCel_Poco <- (ifelse((database$UsoCel == 1), 1,0))
#database$UsoCel_AlgVec <- (ifelse((database$UsoCel == 3 ), 1,0))
database$UsoCel_Frec <- (ifelse((database$UsoCel > 1 ), 1,0))


resultados <-  read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/3. Resultados/ICLV_MCond_3F_MOriginal _estimates.csv",sep=",", dec=".",header=TRUE )
db <- database


ce <- resultados$Estimate        # coefficients b1, b2
eta1 = rnorm(nrow(db), 0, 0)
#eta1 = apollo_inputs[["draws"]][["eta1"]]
eta2 = rnorm(nrow(db), 0, 0)
#eta2 = apollo_inputs[["draws"]][["eta2"]]
eta3 = rnorm(nrow(db), 0, 0)
#eta3 = apollo_inputs[["draws"]][["eta3"]]

## Variables latentes
LV_1 = array()
LV_2 = array()
LV_3 = array()

LV_2 = ce[32]*db$ADULTO40 + ce[33]*db$EXP_3 + ce[34]*db$EXP_4 +
  ce[35]*db$EXP_5 + ce[36]*db$HPICO + ce[37] * db$USODISPMOB + eta2

LV_1 = ce[26]*db$EDUBASICA + ce[27]*db$EDUSUP + ce[28]*db$HPICO + 
  ce[29]*db$CSECO + ce[30]*db$SININFOTRF + ce[31]*LV_2 + eta1

LV_3 = ce[38]*db$EDUBASICA + ce[39]*db$JOVEN30 + ce[40]*db$ADULTO40 +
  ce[41]*db$EXP_2 + ce[42] * db$USODISPMOB + eta3


V_1 = array()
V_2 = array()
V_3 = array()
V_4 = array()


V_1 = ce[1]  + ce[5] * db$T_Alt_1*(1 + db$CG_Alt_1) + ce[6] * db$D_Alt_1 +
  ce[7] * db$SEM_A1_km + 
  ce[8] * db$ACC_A1_0 + ce[9] * db$ACC_A1_1 + ce[10] * db$ACC_A1_2 + 
  ce[11] * db$NO_CAMFD_A1 + ce[12] * db$SI_CAMFD_A1 +
  ce[13] * db$NO_PANEL_A1 + ce[14] * db$SI_PANEL_A1 + 
  ce[15] * db$NO_ZER_A1 + ce[16] * db$SI_ZER_A1 + 
  ce[17] * db$NO_MTRP_A1 + ce[18] * db$SI_MTRP_A1

V_2 = ce[2]  + ce[5] * db$T_Alt_2*(1 + db$CG_Alt_2) + ce[6] * db$D_Alt_2 +
  ce[7]*db$SEM_A2_km + 
  ce[8] * db$ACC_A2_0 + ce[9]* db$ACC_A2_1 + ce[10]* db$ACC_A2_2 + 
  ce[11] * db$NO_CAMFD_A2 + ce[12] * db$SI_CAMFD_A2 +
  ce[13] * db$NO_PANEL_A2 + ce[14] * db$SI_PANEL_A2 + 
  ce[15] * db$NO_ZER_A2 + ce[16] * db$SI_ZER_A2 + 
  ce[17] * db$NO_MTRP_A2 + ce[18] * db$SI_MTRP_A2

V_3 = ce[3]  + ce[5] * db$T_Alt_3*(1 + db$CG_Alt_3) + ce[6] * db$D_Alt_3 +
  ce[7] * db$SEM_A3_km +
  ce[8] * db$ACC_A3_0 + ce[9]* db$ACC_A3_1 + ce[10]* db$ACC_A3_2 +  
  ce[11] * db$NO_CAMFD_A3 + ce[12] * db$SI_CAMFD_A3 +
  ce[13] * db$NO_PANEL_A3 + ce[14] * db$SI_PANEL_A3 + 
  ce[15] * db$NO_ZER_A3 + ce[16] * db$SI_ZER_A3 + 
  ce[17] * db$NO_MTRP_A3 + ce[18] * db$SI_MTRP_A3

V_4 = ce[4] + ce[5] * db$T_Alt_4*(1 + db$CG_Alt_4) + ce[6] * db$D_Alt_4 + 
  ce[7] * db$SEM_EC_km +
  ce[8] * db$ACC_EC_0 + ce[9] * db$ACC_EC_1 + ce[10] * db$ACC_EC_2 + 
  ce[11] * db$NO_CAMFD_EC + ce[12] * db$SI_CAMFD_EC +
  ce[13] * db$NO_PANEL_EC + ce[14] * db$SI_PANEL_EC + 
  ce[15] * db$NO_ZER_EC + ce[16] * db$SI_ZER_EC + 
  ce[17] * db$NO_MTRP_EC + ce[18] * db$SI_MTRP_EC +
  ce[19] * db$SININFOTRF + ce[20] * db$CONINFOTRF + 
  ce[21] * db$UsoCel_Poco +  ce[22] * db$UsoCel_Frec +
  ce[23] * LV_1 + ce[24] * LV_2 + ce[25] * LV_3


P_r1 = array()
P_r2 = array()
P_r3 = array()
P_r4 = array()

for (i in 1:nrow(db)){
  P_r1[i] = exp(V_1[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_3[i])+exp(V_4[i]))
  P_r2[i] = exp(V_2[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_3[i])+exp(V_4[i]))
  P_r3[i] = exp(V_3[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_3[i])+exp(V_4[i]))
  P_r4[i] = exp(V_4[i])/(exp(V_1[i])+exp(V_2[i])+exp(V_3[i])+exp(V_4[i]))}

db$P_r1 <- P_r1
db$P_r2 <- P_r2
db$P_r3 <- P_r3
db$P_r4 <- P_r4


Eleccion = array()

for (i in 1:nrow(db)){
  if (max(db$P_r1[i],db$P_r2[i],db$P_r3[i],db$P_r4[i]) == db$P_r1[i]) {Eleccion[i] = 1}
  else
  {if (max(db$P_r1[i],db$P_r2[i],db$P_r3[i],db$P_r4[i]) == db$P_r2[i]) {Eleccion[i] = 2}
    else
    {if (max(db$P_r1[i],db$P_r2[i],db$P_r3[i],db$P_r4[i]) == db$P_r3[i]) {Eleccion[i] = 3}
      else
      {Eleccion[i] = 4
      }}}}

Prob <- cbind(P_r1, P_r2, P_r3, P_r4)
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
      {if (abs(db$P_r1[i]-db$P_r3[i]) < err){Umbral[i] = 3}
        else
        {if (abs(db$P_r1[i]-db$P_r4[i]) < err){Umbral[i] = 4}
          else
          {Umbral[i] = 1}}}}
    else {if (Eleccion[i] == 2){
      if (abs(db$P_r2[i]-db$P_r1[i]) < err){Umbral[i] = 1}
      else
      {if (abs(db$P_r2[i]-db$P_r3[i]) < err){Umbral[i] = 3}
        else
        {if (abs(db$P_r2[i]-db$P_r4[i]) < err){Umbral[i] = 4}
          else
          {Umbral[i] = 2}}}}
      else
      { if (Eleccion[i] == 3){
        if (abs(db$P_r3[i]-db$P_r1[i]) < err){Umbral[i] = 1}
        else
        {if (abs(db$P_r3[i]-db$P_r2[i]) < err){Umbral[i] = 2}
          else
          {if (abs(db$P_r3[i]-db$P_r4[i]) < err){Umbral[i] = 4}
            else
            {Umbral[i] = 3}}}}
        else
        {if (Eleccion[i] == 4){
          if (abs(db$P_r4[i]-db$P_r1[i]) < err){Umbral[i] = 1}
          else
          {if (abs(db$P_r4[i]-db$P_r2[i]) < err){Umbral[i] = 2}
            else
            {if (abs(db$P_r4[i]-db$P_r3[i]) < err){Umbral[i] = 3}
              else
              {Umbral[i] = 4}}}}}}}}
  else
  {Umbral[i] = db$CHOICE[i]}}

Umbral
Eleccion
NewProb <- cbind(P_r1, P_r2, P_r3, P_r4, db$CHOICE, Eleccion, Umbral)
NewProb

table(Real = db$CHOICE,predicted = Umbral)
mean(factor(Umbral, ordered = TRUE) == db$CHOICE)




#### VALIDACION DEL MODELO EN TESTING  ##########

## Aciertos sobre la base de testeo

database <- read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Bases de datos/DBValidacion.csv",sep="\t", dec=".",header=TRUE)
 
# Normalización de las variables tiempo
for (i in 1:nrow(database)){ 
  database$T_Alt_1[i] = (database$TIEMPOAlt1[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_2[i] = (database$TIEMPOAlt2[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_3[i] = (database$TIEMPOAlt3[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  database$T_Alt_4[i] = (database$TIEMPOEC[i]- (min(c(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3)))/(max(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-(min(database$TIEMPOAlt1[i],database$TIEMPOAlt2[i],database$TIEMPOAlt3[i],database$TIEMPOEC[i])-3))
  
  # Normalización de la variable distancia
  database$D_Alt_1[i] = (database$DISTAlt1[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_2[i] = (database$DISTAlt2[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_3[i] = (database$DISTAlt3[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0-1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  database$D_Alt_4[i] = (database$DISTEC[i]- (min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))/(max(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i]))-(min(c(database$DISTAlt1[i],database$DISTAlt2[i],database$DISTAlt3[i],database$DISTEC[i])-0.1)))
  
  if(database$HPICOHVALLE[i] == 1){
    database$Vel_fl_Alt1[i] = 28/60;
    database$Vel_fl_Alt2[i] = 28/60;
    database$Vel_fl_Alt3[i] = 28/60;
    database$Vel_fl_Alt4[i] = 28/60}
  else{database$Vel_fl_Alt1[i] = 22/60;
  database$Vel_fl_Alt2[i] = 22/60;
  database$Vel_fl_Alt3[i]= 22/60; 
  database$Vel_fl_Alt4[i]= 22/60}
  
  database$T_fl_Alt1[i] = database$DISTAlt1[i]/database$Vel_fl_Alt1[i]
  database$T_fl_Alt2[i] = database$DISTAlt2[i]/database$Vel_fl_Alt2[i]
  database$T_fl_Alt3[i] = database$DISTAlt3[i]/database$Vel_fl_Alt3[i]
  database$T_fl_Alt4[i] = database$DISTEC[i]/database$Vel_fl_Alt4[i]
  
  database$CG_Alt_1[i] = database$TIEMPOAlt1[i]/database$T_fl_Alt1[i]
  database$CG_Alt_2[i] = database$TIEMPOAlt2[i]/database$T_fl_Alt2[i]
  database$CG_Alt_3[i] = database$TIEMPOAlt3[i]/database$T_fl_Alt3[i]
  database$CG_Alt_4[i] = database$TIEMPOEC[i]/database$T_fl_Alt4[i]
}

database$UsoCel_Poco <- (ifelse((database$UsoCel == 1), 1,0))
#database$UsoCel_AlgVec <- (ifelse((database$UsoCel == 3 ), 1,0))
database$UsoCel_Frec <- (ifelse((database$UsoCel > 1 ), 1,0))

db <- database

etaT1 = rnorm(nrow(db), 0, 0)
etaT2 = rnorm(nrow(db), 0, 0)
etaT3 = rnorm(nrow(db), 0, 0)

## Variables latentes
LVT_1 = array()
LVT_2 = array()
LVT_3 = array()

LVT_2 = ce[32]*db$ADULTO40 + ce[33]*db$EXP_3 + ce[34]*db$EXP_4 +
  ce[35]*db$EXP_5 + ce[36]*db$HPICO + ce[37] * db$USODISPMOB 

LVT_1 = ce[26]*db$EDUBASICA + ce[27]*db$EDUSUP + ce[28]*db$HPICO + 
  ce[29]*db$CSECO + ce[30]*db$SININFOTRF + ce[31]*LVT_2 

LVT_3 = ce[38]*db$EDUBASICA + ce[39]*db$JOVEN30 + ce[40]*db$ADULTO40 +
  ce[41]*db$EXP_2 + ce[42] * db$USODISPMOB 


VT_1 = array()
VT_2 = array()
VT_3 = array()
VT_4 = array()


VT_1 = ce[1]  + ce[5] * db$T_Alt_1*(1 + db$CG_Alt_1) + ce[6] * db$D_Alt_1 +
  ce[7] * db$SEM_A1_km + 
  ce[8] * db$ACC_A1_0 + ce[9] * db$ACC_A1_1 + ce[10] * db$ACC_A1_2 + 
  ce[11] * db$NO_CAMFD_A1 + ce[12] * db$SI_CAMFD_A1 +
  ce[13] * db$NO_PANEL_A1 + ce[14] * db$SI_PANEL_A1 + 
  ce[15] * db$NO_ZER_A1 + ce[16] * db$SI_ZER_A1 + 
  ce[17] * db$NO_MTRP_A1 + ce[18] * db$SI_MTRP_A1

VT_2 = ce[2]  + ce[5] * db$T_Alt_2*(1 + db$CG_Alt_2) + ce[6] * db$D_Alt_2 +
  ce[7]*db$SEM_A2_km + 
  ce[8] * db$ACC_A2_0 + ce[9]* db$ACC_A2_1 + ce[10]* db$ACC_A2_2 + 
  ce[11] * db$NO_CAMFD_A2 + ce[12] * db$SI_CAMFD_A2 +
  ce[13] * db$NO_PANEL_A2 + ce[14] * db$SI_PANEL_A2 + 
  ce[15] * db$NO_ZER_A2 + ce[16] * db$SI_ZER_A2 + 
  ce[17] * db$NO_MTRP_A2 + ce[18] * db$SI_MTRP_A2

VT_3 = ce[3]  + ce[5] * db$T_Alt_3*(1 + db$CG_Alt_3) + ce[6] * db$D_Alt_3 +
  ce[7] * db$SEM_A3_km +
  ce[8] * db$ACC_A3_0 + ce[9]* db$ACC_A3_1 + ce[10]* db$ACC_A3_2 +  
  ce[11] * db$NO_CAMFD_A3 + ce[12] * db$SI_CAMFD_A3 +
  ce[13] * db$NO_PANEL_A3 + ce[14] * db$SI_PANEL_A3 + 
  ce[15] * db$NO_ZER_A3 + ce[16] * db$SI_ZER_A3 + 
  ce[17] * db$NO_MTRP_A3 + ce[18] * db$SI_MTRP_A3

VT_4 = ce[4] + ce[5] * db$T_Alt_4*(1 + db$CG_Alt_4) + ce[6] * db$D_Alt_4 + 
  ce[7] * db$SEM_EC_km +
  ce[8] * db$ACC_EC_0 + ce[9] * db$ACC_EC_1 + ce[10] * db$ACC_EC_2 + 
  ce[11] * db$NO_CAMFD_EC + ce[12] * db$SI_CAMFD_EC +
  ce[13] * db$NO_PANEL_EC + ce[14] * db$SI_PANEL_EC + 
  ce[15] * db$NO_ZER_EC + ce[16] * db$SI_ZER_EC + 
  ce[17] * db$NO_MTRP_EC + ce[18] * db$SI_MTRP_EC +
  ce[19] * db$SININFOTRF + ce[20] * db$CONINFOTRF + 
  ce[21] * db$UsoCel_Poco +  ce[22] * db$UsoCel_Frec +
  ce[23] * LVT_1 + ce[24] * LVT_2 + ce[25] * LVT_3


PT_r1 = array()
PT_r2 = array()
PT_r3 = array()
PT_r4 = array()

for (i in 1:nrow(db)){
  PT_r1[i] = exp(VT_1[i])/(exp(VT_1[i])+exp(VT_2[i])+exp(VT_3[i])+exp(VT_4[i]))
  PT_r2[i] = exp(VT_2[i])/(exp(VT_1[i])+exp(VT_2[i])+exp(VT_3[i])+exp(VT_4[i]))
  PT_r3[i] = exp(VT_3[i])/(exp(VT_1[i])+exp(VT_2[i])+exp(VT_3[i])+exp(VT_4[i]))
  PT_r4[i] = exp(VT_4[i])/(exp(VT_1[i])+exp(VT_2[i])+exp(VT_3[i])+exp(VT_4[i]))}

db$PT_r1 <- PT_r1
db$PT_r2 <- PT_r2
db$PT_r3 <- PT_r3
db$PT_r4 <- PT_r4


EleccionTest = array()

for (i in 1:nrow(db)){
  if (max(db$PT_r1[i],db$PT_r2[i],db$PT_r3[i],db$PT_r4[i]) == db$PT_r1[i]) {EleccionTest[i] = 1}
  else
  {if (max(db$PT_r1[i],db$PT_r2[i],db$PT_r3[i],db$PT_r4[i]) == db$PT_r2[i]) {EleccionTest[i] = 2}
    else
    {if (max(db$PT_r1[i],db$PT_r2[i],db$PT_r3[i],db$PT_r4[i]) == db$PT_r3[i]) {EleccionTest[i] = 3}
      else
      {EleccionTest[i] = 4
      }}}}

ProbT <- cbind(PT_r1, PT_r2, PT_r3, PT_r4)
ProbT
table(Real = db$CHOICE, predicted = EleccionTest)
mean(factor(EleccionTest, ordered = TRUE) == db$CHOICE)


UmbralTest = array()
errT = 0.15

for (i in 1:nrow(db)){
  if (db$CHOICE[i] != EleccionTest[i]){
    if (EleccionTest[i] == 1){
      if (abs(db$PT_r1[i]-db$PT_r2[i]) < errT){UmbralTest[i] = 2}
      else
      {if (abs(db$PT_r1[i]-db$PT_r3[i]) < err){UmbralTest[i] = 3}
        else
        {if (abs(db$PT_r1[i]-db$PT_r4[i]) < err){UmbralTest[i] = 4}
          else
          {UmbralTest[i] = 1}}}}
    else {if (EleccionTest[i] == 2){
      if (abs(db$PT_r2[i]-db$PT_r1[i]) < err){UmbralTest[i] = 1}
      else
      {if (abs(db$PT_r2[i]-db$PT_r3[i]) < err){UmbralTest[i] = 3}
        else
        {if (abs(db$PT_r2[i]-db$PT_r4[i]) < err){UmbralTest[i] = 4}
          else
          {UmbralTest[i] = 2}}}}
      else
      { if (EleccionTest[i] == 3){
        if (abs(db$PT_r3[i]-db$PT_r1[i]) < err){UmbralTest[i] = 1}
        else
        {if (abs(db$PT_r3[i]-db$PT_r2[i]) < err){UmbralTest[i] = 2}
          else
          {if (abs(db$PT_r3[i]-db$PT_r4[i]) < err){UmbralTest[i] = 4}
            else
            {UmbralTest[i] = 3}}}}
        else
        {if (EleccionTest[i] == 4){
          if (abs(db$PT_r4[i]-db$PT_r1[i]) < err){UmbralTest[i] = 1}
          else
          {if (abs(db$PT_r4[i]-db$PT_r2[i]) < err){UmbralTest[i] = 2}
            else
            {if (abs(db$PT_r4[i]-db$PT_r3[i]) < err){UmbralTest[i] = 3}
              else
              {UmbralTest[i] = 4}}}}}}}}
  else
  {UmbralTest[i] = db$CHOICE[i]}}

UmbralTest
EleccionTest
NewProbT <- cbind(PT_r1, PT_r2, PT_r3, PT_r4, db$CHOICE, EleccionTest, UmbralTest)
NewProbT

table(Real = db$CHOICE, predicted = UmbralTest)
mean(factor(UmbralTest, ordered = TRUE) == db$CHOICE)

