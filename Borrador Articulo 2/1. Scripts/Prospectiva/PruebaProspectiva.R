
### Clear memory
rm(list = ls())

database = read.csv("/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv",sep="\t", dec=".",header=TRUE)

alpha=0.88
beta=0.88
lamda= 2.25

T_rp = c()
DT1 = c()
VP1 = c()
VN1 = c()
DT2 = c()
VP2 = c()
VN2 = c()
DT3 = c()
VP3 = c()
VN3 = c()
DT4 = c()
VP4 = c()
VN4 = c()
V1 = c()

for(z in 1:nrow(database)){
  T_rp[z] = mean(c(database$TIEMPOAlt1[z],database$TIEMPOAlt2[z],database$TIEMPOAlt3[z]))
  
  DT1[z] <- database$TIEMPOAlt1[z]-T_rp[z]
  
  if(DT1[z]>0.5)
  {VN1[z] <- -lamda*(DT1[z])**beta}
  else {VN1[z]<- 0}
  if(DT1[z]<0.5)
  {VP1[z] <- (-DT1[z])**alpha}
  else
  {VP1[z]<- 0}
  if (abs(DT1[z])<=0.5)
  {VP1[z] <- 0}
  
  
  DT2[z] <- database$TIEMPOAlt2[z]- T_rp[z]
  if(DT2[z]>=0)
  {VN2[z] <- -lamda*(DT2[z])**beta}
  else {VN2[z]<-0}
  if(DT2[z]<0)
  {VP2[z] <- (-DT2[z])**alpha}
  else
  {VP2[z]<- 0}
  
  DT3[z] <- database$TIEMPOAlt3[z]-T_rp[z]
  if(DT3[z]>=0)
  {VN3[z] <- -lamda*(DT3[z])**beta}
  else {VN3[z]<-0}
  if(DT3[z]<0)
  {VP3[z] <- (-DT3[z])**beta}
  else
  {VP3[z]<- 0}
  
  DT4[z] <- database$TIEMPOEC[z]-T_rp[z]
  if(DT4[z]>2.5)
  {VN4[z] <- -lamda*(DT4[z])**beta}
  else {VN4[z]<- 0}
  if(DT4[z]<2.5)
  {VP4[z] <- (-DT4[z])**beta}
  else
  {VP4[z]<- 0}
  if (abs(DT4[z])<=2.5)
  {VP4[z]<- 0; VN4[z]<-0}
}


V1 = VP1 + VN1
V2 = VP2 + VN2
V3 = VP3 + VN3
V4 = VP4 + VN4
plot(DT1,V1)
plot(DT2,V2)
plot(DT3,V3)
plot(DT4,V4)

summary(T_rp)

qqnorm(T_rp)
hist(T_rp)
boxplot(T_rp)
mean(T_rp)
