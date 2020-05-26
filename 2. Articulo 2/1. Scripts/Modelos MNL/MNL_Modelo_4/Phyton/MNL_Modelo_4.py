#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Apr 10 02:51:12 2020

@author: williz
"""

# Cargar Paquetes

import pandas as pd
import numpy as np
import biogeme.database as db
import biogeme.biogeme as bio
from biogeme.models import piecewise
import biogeme.loglikelihood as ll


#Cargar DataBase

df1 = pd.read_csv('/Users/williz/Desktop/ModelosED/1. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv', sep='\t')
df= df1.drop(['ViajeId'], axis=1)

# normalizar variables continuas para evitar zero log-likelihood 
#pandas["TIEMPO_PROFESION"]=(pandas["TIEMPO_PROFESION"]-pandas["TIEMPO_PROFESION"].min())/(pandas["TIEMPO_PROFESION"].max()-pandas["TIEMPO_PROFESION"].min())
#pandas["DISTAlt1"]=(pandas["DISTAlt1"]-pandas["DISTAlt1"].min())/(pandas["DISTAlt1"].max()-pandas["DISTAlt1"].min())
#pandas["DISTAlt2"]=(pandas["DISTAlt2"]-pandas["DISTAlt2"].min())/(pandas["DISTAlt2"].max()-pandas["DISTAlt2"].min())
#pandas["DISTAlt3"]=(pandas["DISTAlt3"]-pandas["DISTAlt3"].min())/(pandas["DISTAlt3"].max()-pandas["DISTAlt3"].min())
#pandas["DISTEC"]=(pandas["DISTEC"]-pandas["DISTEC"].min())/(pandas["DISTEC"].max()-pandas["DISTEC"].min())
#pandas["TIEMPOAlt1"]=(pandas["TIEMPOAlt1"]-pandas["TIEMPOAlt1"].min())/(pandas["TIEMPOAlt1"].max()-pandas["TIEMPOAlt1"].min())
#pandas["TIEMPOAlt2"]=(pandas["TIEMPOAlt2"]-pandas["TIEMPOAlt2"].min())/(pandas["TIEMPOAlt2"].max()-pandas["TIEMPOAlt2"].min())
#pandas["TIEMPOAlt3"]=(pandas["TIEMPOAlt3"]-pandas["TIEMPOAlt3"].min())/(pandas["TIEMPOAlt3"].max()-pandas["TIEMPOAlt3"].min())
#pandas["TIEMPOEC"]=(pandas["TIEMPOEC"]-pandas["TIEMPOEC"].min())/(pandas["TIEMPOEC"].max()-pandas["TIEMPOEC"].min())


database = db.Database("DBMuestra_ModeloLogitVL",df)

# The following statement allows you to use the names of the variable
# as Python variable.

globals().update(database.variables)



# Definicion de Variables




# Coeficientes


asc_ruta1 = Beta('asc_ruta1',0,None,None,0)
asc_ruta2  = Beta('asc_ruta2',0,None,None,0)
asc_ruta3 = Beta('asc_ruta3',0,None,None,1)
asc_rutaEC = Beta('asc_rutaEC',0,None,None,0)
b_TT = Beta('b_TT',0,None,None,0)
b_DT = Beta('b_DT',0,None,None,0)
b_CongAB = Beta('b_CongAB',0,None,None,0)
b_CongCD = Beta('b_CongCD',0,None,None,0)
b_CongEF = Beta('b_CongEF',0,None,None,1)

b_Sem_1 = Beta('b_Sem_1',0,None,None,0) 
b_Sem_2 = Beta('b_Sem_2',0,None,None,0) 
b_Sem_3 = Beta('b_Sem_3',0,None,None,1) 

b_ACC_0 = Beta('b_ACC_0',0,None,None,0) 
b_ACC_1 = Beta('b_ACC_1',0,None,None,0) 
b_ACC_2 = Beta('b_ACC_2',0,None,None,1) 


b_NO_CAMFD = Beta('b_NO_CAMFD',0,None,None,1)  
b_SI_CAMFD = Beta('b_SI_CAMFD',0,None,None,0)  
b_NO_PANEL = Beta('b_NO_PANEL',0,None,None,1)  
b_SI_PANEL = Beta('b_SI_PANEL',0,None,None,0)  
b_NO_ZER = Beta('b_NO_ZER',0,None,None,0) 
b_SI_ZER = Beta('b_SI_ZER',0,None,None,1)  
b_No_MTRP = Beta('b_No_MTRP',0,None,None,1)  
b_Si_MTRP = Beta('b_Si_MTRP',0,None,None,0) 

b_CSECO = Beta('b_CSECO',0,None,None,0)  
b_CLLUVIA = Beta('b_CLLUVIA',0,None,None,1)           
b_Hpico = Beta('b_Hpico',0,None,None,0)           
b_Hvalle  = Beta('b_Hvalle ',0,None,None,1)           
b_Joven30 = Beta('b_Joven30',0,None,None,0)
b_Adulto40 = Beta('b_Adulto40',0,None,None,0)
b_Adulto60 = Beta('b_Adulto60',0,None,None,0)
b_AdultoMayor = Beta('b_AdultoMayor',0,None,None,1)
b_USOCINTURON = Beta('b_USOCINTURON',0,None,None,0)
b_NOUSOCINTURON = Beta('b_NOUSOCINTURON',0,None,None,1)
b_USODISPMOB = Beta('b_USODISPMOB',0,None,None,0)
b_NOUSODISPMOB = Beta('b_NOUSODISPMOB',0,None,None,1)
b_EdBasica = Beta('b_EdBasica',0,None,None,0)
b_EdSuperior = Beta('b_EdSuperior',0,None,None,1)
b_Htrab_1 = Beta('b_Htrab_1',0,None,None,0)
b_Htrab_2 = Beta('b_Htrab_2',0,None,None,0)
b_Htrab_3 = Beta('b_Htrab_3',0,None,None,0)
b_Htrab_4 = Beta('b_Htrab_4',0,None,None,4)
b_Exp_1 = Beta('b_Exp_1',0,None,None,0)
b_Exp_2 = Beta('b_Exp_2',0,None,None,0)
b_Exp_3 = Beta('b_Exp_3',0,None,None,0)
b_Exp_4 = Beta('b_Exp_4',0,None,None,1)
b_Exp_5 = Beta('b_Exp_5',0,None,None,0)





#Especificacion de las Funciones de Utilidad


V1 = asc_ruta1 + b_TT * TIEMPOAlt1 + b_DT * DISTAlt1 + b_CongAB*CONG_AB_A1 +\
     b_CongCD * CONG_CD_A1 + b_CongEF*CONG_EF_A1 +\
     b_Sem_1 * SEMF_A1_1 + b_Sem_2 * SEMF_A1_2 + b_Sem_3 * SEMF_A1_3 +\
     b_ACC_0 * ACC_A1_0 + b_ACC_1 * ACC_A1_1 + b_ACC_2 * ACC_A1_2 +\
     b_NO_CAMFD * NO_CAMFD_A1 + b_SI_CAMFD * SI_CAMFD_A1 +\
     b_NO_PANEL * NO_PANEL_A1 + b_SI_PANEL * SI_PANEL_A1 +\
     b_No_MTRP * NO_MTRP_A1 + b_Si_MTRP * SI_MTRP_A1
    

V2 = asc_ruta2 + b_TT*TIEMPOAlt2 + b_DT*DISTAlt2 + b_CongAB*CONG_AB_A2 +\
     b_CongCD*CONG_CD_A2 + b_CongEF*CONG_EF_A2  +\
     b_Sem_1*SEMF_A2_1 + b_Sem_2*SEMF_A2_2 + b_Sem_3*SEMF_A2_3  +\
     b_ACC_0*ACC_A2_0 + b_ACC_1*ACC_A2_1 + b_ACC_2*ACC_A2_2 +\
     b_NO_CAMFD * NO_CAMFD_A2 + b_SI_CAMFD * SI_CAMFD_A2 +\
     b_NO_PANEL * NO_PANEL_A2 + b_SI_PANEL * SI_PANEL_A2 +\
     b_No_MTRP * NO_MTRP_A2 + b_Si_MTRP * SI_MTRP_A2


V3 = asc_ruta3 + b_TT*TIEMPOAlt3 + b_DT*DISTAlt3 + b_CongAB*CONG_AB_A3 +\
     b_CongCD*CONG_CD_A3 + b_CongEF*CONG_EF_A3 +\
     b_Sem_1*SEMF_A3_1 + b_Sem_2*SEMF_A3_2 + b_Sem_3*SEMF_A3_3 +\
     b_ACC_0*ACC_A3_0 + b_ACC_1*ACC_A3_1 + b_ACC_2*ACC_A3_2  +\
     b_NO_CAMFD * NO_CAMFD_A3 + b_SI_CAMFD * SI_CAMFD_A3 +\
     b_NO_PANEL * NO_PANEL_A3 + b_SI_PANEL * SI_PANEL_A3 +\
     b_No_MTRP * NO_MTRP_A3 + b_Si_MTRP * SI_MTRP_A3


V4 = asc_rutaEC + b_TT*TIEMPOEC + b_DT*DISTEC + b_CongAB*CONG_AB_EC +\
     b_CongCD*CONG_CD_EC + b_CongEF*CONG_EF_EC +\
     b_Sem_1*SEMF_EC_1 + b_Sem_2*SEMF_EC_2 + b_Sem_3*SEMF_EC_3 +\
     b_ACC_0*ACC_EC_0 + b_ACC_1*ACC_EC_1 + b_ACC_2*ACC_EC_2  +\
     b_NO_CAMFD * NO_CAMFD_EC + b_SI_CAMFD * SI_CAMFD_EC +\
     b_NO_PANEL * NO_PANEL_EC + b_SI_PANEL * SI_PANEL_EC +\
     b_No_MTRP * NO_MTRP_EC + b_Si_MTRP * SI_MTRP_EC +\
     b_CSECO * CSECO + b_CLLUVIA * CLLUVIA + b_Hpico * HPICO + b_Hvalle * HVALLE +\
     b_Joven30 * JOVEN30 + b_Adulto40 * ADULTO40 + b_Adulto60 *ADULTO60 + b_AdultoMayor * ADULTOMAYOR +\
     b_USOCINTURON * USOCINTURON + b_NOUSOCINTURON * NOUSOCINTURON + b_USODISPMOB * USODISPMOB +\
     b_NOUSODISPMOB * NOUSODISPMOB + b_EdBasica * EDUBASICA + b_EdSuperior * EDUSUP +\
     b_Htrab_1 * HTRB_1 + b_Htrab_2 * HTRB_2 + b_Htrab_3 * HTRB_3 + b_Htrab_4 * HTRB_4 +\
     b_Exp_1 * EXP_1 + b_Exp_2 * EXP_2 + b_Exp_3 * EXP_3 + b_Exp_4 * EXP_4 + b_Exp_5 * EXP_5
    


# Asociacion de las funciones de utilidad con el numero de alternativas
V = {1:V1, 2:V2, 3:V3, 4:V4}

#ASOCIAR LAS CONDICIONES DE DISPONIBILIDAD CON LAS ALTERNATIVAS PARA QUE SIEMPRE ESTEN DISPONIBLES
AV = {1:1, 2:1, 3:1, 4:1}

#Contribucion a la función de verosimilitud como el logaritmo de un modelo logit
logprob = models.loglogit(V,AV,CHOICE)

#BIOGEME
biogeme  = bio.BIOGEME(database,logprob,numberOfThreads=1)

biogeme.modelName = "MNL_Modelo_4"
results = biogeme.estimate()
# Get the results in a pandas table
pandasResults = results.getEstimatedParameters()
print(pandasResults)
print(f"Nbr of observations: {database.getNumberOfObservations()}")
print(f"LL(0) =    {results.data.initLogLike:.3f}")
print(f"LL(beta) = {results.data.logLike:.3f}")
print(f"rho bar square = {results.data.rhoBarSquare:.3g}")
print(f"Output file: {results.data.htmlFileName}")
results.writeLaTeX()
print(f"LaTeX file: {results.data.latexFileName}")