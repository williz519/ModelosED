library(readxl)

# Modelo MNL

#Cargas las Bases de Datos
rm(list = ls())

db = read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBMuestraArt3.csv",sep="\t", dec=".",header=TRUE)

db_test = read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/3. Base de datos/DBValidacion.csv",sep="\t", dec=".",header=TRUE)

resultados <-  read.csv("/Users/williz/Desktop/ModelosED/3. Articulo 3/4. Resultados/MNL/MNL_Modelo Art3_estimates.csv",sep=",", dec=".",header=TRUE )


#### VALIDACION DEL MODELO ##########

## Aciertos sobre la base del modelo

ce <- resultados$Estimate        # coefficients b1, b2

V_1 = array()
V_2 = array()
V_3 = array()
V_4 = array()


V_1 = ce[1]  + ce[5] * db$T_Alt_1*(1 + db$CG_Alt_1) + ce[6] * db$D_Alt_1 +
  ce[7]*db$SEM_A1_km + 
  ce[8] * db$ACC_A1_0 + ce[9]*db$ACC_A1_1 + ce[10]*db$ACC_A1_2 + 
  ce[11] * db$NO_CAMFD_A1 + ce[12] * db$SI_CAMFD_A1 +
  ce[13] * db$NO_PANEL_A1 + ce[14] * db$SI_PANEL_A1 + 
  ce[15] * db$NO_ZER_A1 + ce[16] * db$SI_ZER_A1 + 
  ce[17] * db$NO_MTRP_A1 + ce[18] * db$SI_MTRP_A1


V_2 = ce[2]  +ce[5] * db$T_Alt_2*(1 + db$CG_Alt_2) + ce[6] * db$D_Alt_2 +
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
  ce[21] * db$UsoCel_Poco +  ce[22] * db$UsoCel_Frec

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
table(predicted = Eleccion, Real = db$CHOICE)
mean(factor(Eleccion, ordered = TRUE) == db$CHOICE)


Umbral = array()
err = 0.10

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

table(Real = db$CHOICE, predicted = Umbral)
mean(factor(Umbral, ordered = TRUE) == db$CHOICE)



#### VALIDACION DEL MODELO EN TESTING  ##########

## Aciertos sobre la base de testeo

db <-db_test

VT_1 = array()
VT_2 = array()
VT_3 = array()
VT_4 = array()

VT_1 = ce[1]  + ce[5] * db$T_Alt_1*(1 + db$CG_Alt_1) + ce[6] * db$D_Alt_1 +
  ce[7]*db$SEM_A1_km + 
  ce[8] * db$ACC_A1_0 + ce[9]*db$ACC_A1_1 + ce[10]*db$ACC_A1_2 + 
  ce[11] * db$NO_CAMFD_A1 + ce[12] * db$SI_CAMFD_A1 +
  ce[13] * db$NO_PANEL_A1 + ce[14] * db$SI_PANEL_A1 + 
  ce[15] * db$NO_ZER_A1 + ce[16] * db$SI_ZER_A1 + 
  ce[17] * db$NO_MTRP_A1 + ce[18] * db$SI_MTRP_A1


VT_2 = ce[2]  +ce[5] * db$T_Alt_2*(1 + db$CG_Alt_2) + ce[6] * db$D_Alt_2 +
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
  ce[21] * db$UsoCel_Poco +  ce[22] * db$UsoCel_Frec

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
table(predicted = EleccionTest, Real = db$CHOICE)
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

table(predicted = UmbralTest, Real = db$CHOICE)
mean(factor(UmbralTest, ordered = TRUE) == db$CHOICE)


