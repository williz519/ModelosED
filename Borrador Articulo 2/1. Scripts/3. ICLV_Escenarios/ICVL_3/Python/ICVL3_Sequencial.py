#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed May 20 19:30:49 2020

@author: williz
"""

# Cargar Paquetes

import pandas as pd
import numpy as np
import biogeme.database as db
import biogeme.biogeme as bio
import biogeme.models as models
import biogeme.distributions as dist
import biogeme.results as res
from biogeme.expressions import Beta, DefineVariable, bioDraws, log, MonteCarlo


#Cargar DataBase

df1 = pd.read_csv('/Users/williz/Desktop/ModelosED/2. Articulo 2/2. Database/DBMuestra_ModeloLogitVL.csv', sep='\t')
df= df1.drop(['ViajeId'], axis=1)


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
b_NO_ZER = Beta('b_NO_ZER',0,None,None,1) 
b_SI_ZER = Beta('b_SI_ZER',0,None,None,0)  
b_No_MTRP = Beta('b_No_MTRP',0,None,None,1)  
b_Si_MTRP = Beta('b_Si_MTRP',0,None,None,0) 

lambda1 = Beta('lambda1',1,None,None,0)
lambda2 = Beta('lambda2',1,None,None,0)
lambda3 = Beta('lambda3',1,None,None,0)

gamma_USOCINTURON = Beta('gamma_USOCINTURON',0,None,None,0)
gamma_USODISPMOB = Beta('gamma_USODISPMOB',0,None,None,0)
gamma_ADULTO40 = Beta('gamma_ADULTO40',0,None,None,0)
gamma_EXP_1 = Beta('gamma_EXP_1',0,None,None,0) 
gamma_HPICO = Beta('gamma_HPICO',0,None,None,0)
gamma_LV1 = Beta('gamma_LV1',0,None,None,0)
gamma_EDUBASICA = Beta('gamma_EDUBASICA',0,None,None,0)
gamma_JOVEN30 = Beta('gamma_JOVEN30',0,None,None,0)
gamma_EXP_2 = Beta('gamma_EXP_2',0,None,None,0)
gamma_HTRB_2 = Beta('gamma_HTRB_2',0,None,None,0)
gamma_HTRB_3 = Beta('gamma_HTRB_3',0,None,None,0)
gamma_HTRB_4 = Beta('gamma_HTRB_4',0,None,None,0)
gamma_LV2 = Beta('gamma_LV2',0,None,None,0)


# Latent variable: structural equation

# Note that the expression must be on a single line. In order to 
# write it across several lines, each line must terminate with 
# the \ symbol
# Define a random parameter, normally distributed, designed to be used
# for numerical integration



LV_1 = gamma_USOCINTURON * USOCINTURON + gamma_USODISPMOB * USODISPMOB 
  
LV_2 = gamma_ADULTO40*ADULTO40 + gamma_EXP_1*EXP_1 + gamma_HPICO*HPICO +\
    gamma_USOCINTURON*USOCINTURON  + gamma_LV1*LV_1 
  
LV_3 = gamma_EDUBASICA * EDUBASICA + gamma_JOVEN30*JOVEN30 +  gamma_ADULTO40*ADULTO40 +\
    gamma_EXP_2*EXP_2 + gamma_HTRB_2*HTRB_2 + gamma_HTRB_3*HTRB_3 + gamma_HTRB_4*HTRB_4 +\
    gamma_USOCINTURON * USOCINTURON + gamma_USODISPMOB * USODISPMOB + gamma_LV2*LV_2
  

### ECUACIONES DE MEDIDA

#LV_1
    
INTER_FRbr= Beta('INTER_FRbr',0,None,None,1)
INTER_EnfCond = Beta('INTER_EnfCond',0,None,None,0)
INTER_AFrSem = Beta('INTER_AFrSem',0,None,None,0)
INTER_CulFr = Beta('INTER_CulFr',0,None,None,0)
INTER_UsoPito = Beta('INTER_UsoPito',0,None,None,0)

BETA_FRbrF1 = Beta('BETA_FRbrF1',0,None,None,0)
BETA_EnfCondF1 = Beta('BETA_EnfCondF1',0,None,None,0)
BETA_AFrSemF1 = Beta('BETA_AFrSemF1',0,None,None,0)
BETA_CulFrF1 = Beta('BETA_CulFrF1',0,None,None,0)
BETA_UsoPitoF1 = Beta('BETA_CulFrF1',0,None,None,0)

MODEL_FRbr = INTER_FRbr + BETA_FRbrF1 * LV_1
MODEL_EnfCond = INTER_EnfCond + BETA_EnfCondF1 * LV_1
MODEL_AFrSem = INTER_AFrSem + BETA_AFrSemF1 * LV_1
MODEL_CulFr = INTER_CulFr + BETA_CulFrF1 * LV_1
MODEL_UsoPito = INTER_UsoPito + BETA_UsoPitoF1 * LV_1

SIGMA_STAR_FRbr = Beta('SIGMA_STAR_FRbr',0,None,None,1)
SIGMA_STAR_EnfCond = Beta('SIGMA_STAR_EnfCond', 0,None,None,0)
SIGMA_STAR_AFrSem = Beta('SIGMA_STAR_AFrSem',0,None,None,0)
SIGMA_STAR_CulFr = Beta('SIGMA_STAR_CulFr', 0,None,None,0)
SIGMA_STAR_UsoPito = Beta('SIGMA_STAR_UsoPito', 0,None,None,0)


#LV_2

INTER_OmLmVel = Beta('INTER_OmLmVel',0,None,None,1)
INTER_IgPare = Beta('INTER_IgPare',0,None,None,0)
INTER_UsoCel = Beta('INTER_UsoCel',0,None,None,0)


BETA_OmLmVelF2 = Beta('BETA_OmLmVelF2',0,None,None,0)
BETA_IgPareF2 = Beta('BETA_IgPareF2', 0,None,None,0)
BETA_UsoCelF2 = Beta('BETA_UsoCelF2',0,None,None,0)


MODEL_OmLmVel = INTER_OmLmVel + BETA_OmLmVelF2 * LV_2
MODEL_IgPare = INTER_IgPare + BETA_IgPareF2 * LV_2
MODEL_UsoCel = INTER_UsoCel + BETA_UsoCelF2 * LV_2

SIGMA_STAR_OmLmVel = Beta('SIGMA_STAR_OmLmVel', 0,None,None,0)
SIGMA_STAR_IgPare = Beta('SIGMA_STAR_IgPare', 0,None,None,0)
SIGMA_STAR_UsoCel = Beta('SIGMA_STAR_UsoCel', 0,None,None,0)


#LV_3

INTER_PasoPeaton= Beta('INTER_PasoPeaton',0,None,None,1)
INTER_UsoDirec= Beta('INTER_UsoDirec',0,None,None,0)

BETA_PasoPeatonF3 = Beta('BETA_PasoPeatonF3',0,None,None,0)
BETA_UsoDirecF3 = Beta('BETA_UsoDirecF3',0,None,None,0)
BETA_UsoCelF3 = Beta('BETA_UsoCelF3',0,None,None,0)


MODEL_PasoPeaton = INTER_PasoPeaton + BETA_PasoPeatonF3 * LV_3
MODEL_UsoDirec = INTER_UsoDirec + BETA_UsoDirecF3 * LV_3
MODEL_UsoCel = INTER_UsoCel + BETA_UsoCelF2 * LV_3

SIGMA_STAR_PasoPeaton = Beta('SIGMA_STAR_PasoPeaton',0,None,None,0)
SIGMA_STAR_UsoDirec = Beta('SIGMA_STAR_UsoDirec',0,None,None,0)


delta_1 = Beta('delta_1',0.1,0,10,0 )
delta_2 = Beta('delta_2',0.2,0,10,0 )
tau_1 = -delta_1 - delta_2
tau_2 = -delta_1 
tau_3 = delta_1
tau_4 = delta_1 + delta_2


FRbr_tau_1 = (tau_1-MODEL_FRbr) / SIGMA_STAR_FRbr
FRbr_tau_2 = (tau_2-MODEL_FRbr) / SIGMA_STAR_FRbr
FRbr_tau_3 = (tau_3-MODEL_FRbr) / SIGMA_STAR_FRbr
FRbr_tau_4 = (tau_4-MODEL_FRbr) / SIGMA_STAR_FRbr
IndFRbr = {
    1: bioNormalCdf(FRbr_tau_1),
    2: bioNormalCdf(FRbr_tau_2)-bioNormalCdf(FRbr_tau_1),
    3: bioNormalCdf(FRbr_tau_3)-bioNormalCdf(FRbr_tau_2),
    4: bioNormalCdf(FRbr_tau_4)-bioNormalCdf(FRbr_tau_3),
    5: 1-bioNormalCdf(FRbr_tau_4)
}

P_FRbr = Elem(IndFRbr, FRbr)


EnfCond_tau_1 = (tau_1-MODEL_EnfCond) / SIGMA_STAR_EnfCond
EnfCond_tau_2 = (tau_2-MODEL_EnfCond) / SIGMA_STAR_EnfCond
EnfCond_tau_3 = (tau_3-MODEL_EnfCond) / SIGMA_STAR_EnfCond
EnfCond_tau_4 = (tau_4-MODEL_EnfCond) / SIGMA_STAR_EnfCond
IndEnfCond = {
    1: bioNormalCdf(EnfCond_tau_1),
    2: bioNormalCdf(EnfCond_tau_2)-bioNormalCdf(EnfCond_tau_1),
    3: bioNormalCdf(EnfCond_tau_3)-bioNormalCdf(EnfCond_tau_2),
    4: bioNormalCdf(EnfCond_tau_4)-bioNormalCdf(EnfCond_tau_3),
    5: 1-bioNormalCdf(EnfCond_tau_4)
}

P_EnfCond = Elem(IndEnfCond, EnfCond)

AFrSem_tau_1 = (tau_1-MODEL_AFrSem) / SIGMA_STAR_AFrSem
AFrSem_tau_2 = (tau_2-MODEL_AFrSem) / SIGMA_STAR_AFrSem
AFrSem_tau_3 = (tau_3-MODEL_AFrSem) / SIGMA_STAR_AFrSem
AFrSem_tau_4 = (tau_4-MODEL_AFrSem) / SIGMA_STAR_AFrSem
IndAFrSem = {
    1: bioNormalCdf(AFrSem_tau_1),
    2: bioNormalCdf(AFrSem_tau_2)-bioNormalCdf(AFrSem_tau_1),
    3: bioNormalCdf(AFrSem_tau_3)-bioNormalCdf(AFrSem_tau_2),
    4: bioNormalCdf(AFrSem_tau_4)-bioNormalCdf(AFrSem_tau_3),
    5: 1-bioNormalCdf(AFrSem_tau_4)
}

P_AFrSem = Elem(IndAFrSem, AFrSem)

CulFr_tau_1 = (tau_1-MODEL_CulFr) / SIGMA_STAR_CulFr
CulFr_tau_2 = (tau_2-MODEL_CulFr) / SIGMA_STAR_CulFr
CulFr_tau_3 = (tau_3-MODEL_CulFr) / SIGMA_STAR_CulFr
CulFr_tau_4 = (tau_4-MODEL_CulFr) / SIGMA_STAR_CulFr
IndCulFr = {
    1: bioNormalCdf(CulFr_tau_1),
    2: bioNormalCdf(CulFr_tau_2)-bioNormalCdf(CulFr_tau_1),
    3: bioNormalCdf(CulFr_tau_3)-bioNormalCdf(CulFr_tau_2),
    4: bioNormalCdf(CulFr_tau_4)-bioNormalCdf(CulFr_tau_3),
    5: 1-bioNormalCdf(CulFr_tau_4)
}

P_CulFr = Elem(IndCulFr, CulFr)


UsoPito_tau_1 = (tau_1-MODEL_UsoPito) / SIGMA_STAR_UsoPito
UsoPito_tau_2 = (tau_2-MODEL_UsoPito) / SIGMA_STAR_UsoPito
UsoPito_tau_3 = (tau_3-MODEL_UsoPito) / SIGMA_STAR_UsoPito
UsoPito_tau_4 = (tau_4-MODEL_UsoPito) / SIGMA_STAR_UsoPito
IndUsoPito = {
    1: bioNormalCdf(PUsoPito_tau_1),
    2: bioNormalCdf(UsoPito_tau_2)-bioNormalCdf(UsoPito_tau_1),
    3: bioNormalCdf(UsoPito_tau_3)-bioNormalCdf(UsoPito_tau_2),
    4: bioNormalCdf(UsoPito_tau_4)-bioNormalCdf(UsoPito_tau_3),
    5: 1-bioNormalCdf(UsoPito_tau_4)
}

P_UsoPito = Elem(IndUsoPito, UsoPito)


OmLmVel_tau_1 = (tau_1-MODEL_OmLmVel) / SIGMA_STAR_OmLmVel
OmLmVel_tau_2 = (tau_2-MODEL_OmLmVel) / SIGMA_STAR_OmLmVel
OmLmVel_tau_3 = (tau_3-MODEL_OmLmVel) / SIGMA_STAR_OmLmVel
OmLmVel_tau_4 = (tau_4-MODEL_OmLmVel) / SIGMA_STAR_OmLmVel
IndOmLmVel = {
    1: bioNormalCdf(OmLmVel_tau_1),
    2: bioNormalCdf(OmLmVel_tau_2)-bioNormalCdf(OmLmVel_tau_1),
    3: bioNormalCdf(OmLmVel_tau_3)-bioNormalCdf(OmLmVel_tau_2),
    4: bioNormalCdf(OmLmVel_tau_4)-bioNormalCdf(OmLmVel_tau_3),
    5: 1-bioNormalCdf(OmLmVel_tau_4)
}

P_OmLmVel = Elem(IndOmLmVel, OmLmVel)

IgPare_tau_1 = (tau_1-MODEL_IgPare) / SIGMA_STAR_IgPare
IgPare_tau_2 = (tau_2-MODEL_IgPare) / SIGMA_STAR_IgPare
IgPare_tau_3 = (tau_3-MODEL_IgPare) / SIGMA_STAR_IgPare
IgPare_tau_4 = (tau_4-MODEL_IgPare) / SIGMA_STAR_IgPare
IndIgPare = {
    1: bioNormalCdf(IgPare_tau_1),
    2: bioNormalCdf(IgPare_tau_2)-bioNormalCdf(IgPare_tau_1),
    3: bioNormalCdf(IgPare_tau_3)-bioNormalCdf(IgPare_tau_2),
    4: bioNormalCdf(IgPare_tau_4)-bioNormalCdf(IgPare_tau_3),
    5: 1-bioNormalCdf(IgPare_tau_4)
}

P_IgPare = Elem(IndIgPare, IgPare)

UsoCel_tau_1 = (tau_1-MODEL_UsoCel) / SIGMA_STAR_UsoCel
UsoCel_tau_2 = (tau_2-MODEL_UsoCel) / SIGMA_STAR_UsoCel
UsoCel_tau_3 = (tau_3-MODEL_UsoCel) / SIGMA_STAR_UsoCel
UsoCel_tau_4 = (tau_4-MODEL_UsoCel) / SIGMA_STAR_UsoCel
IndUsoCel = {
    1: bioNormalCdf(UsoCel_tau_1),
    2: bioNormalCdf(UsoCel_tau_2)-bioNormalCdf(UsoCel_tau_1),
    3: bioNormalCdf(UsoCel_tau_3)-bioNormalCdf(UsoCel_tau_2),
    4: bioNormalCdf(UsoCel_tau_4)-bioNormalCdf(UsoCel_tau_3),
    5: 1-bioNormalCdf(UsoCel_tau_4)
}

P_UsoCel = Elem(IndUsoCel, UsoCel)



PasoPeaton_tau_1 = (tau_1-MODEL_PasoPeaton) / SIGMA_STAR_PasoPeaton
PasoPeaton_tau_2 = (tau_2-MODEL_PasoPeaton) / SIGMA_STAR_PasoPeaton
PasoPeaton_tau_3 = (tau_3-MODEL_PasoPeaton) / SIGMA_STAR_PasoPeaton
PasoPeaton_tau_4 = (tau_4-MODEL_PasoPeaton) / SIGMA_STAR_PasoPeaton
IndPasoPeaton = {
    1: bioNormalCdf(PasoPeaton_tau_1),
    2: bioNormalCdf(PasoPeaton_tau_2)-bioNormalCdf(PasoPeaton_tau_1),
    3: bioNormalCdf(PasoPeaton_tau_3)-bioNormalCdf(PasoPeaton_tau_2),
    4: bioNormalCdf(PasoPeaton_tau_4)-bioNormalCdf(PasoPeaton_tau_3),
    5: 1-bioNormalCdf(PasoPeaton_tau_4)
}

P_PasoPeaton = Elem(IndPasoPeaton, PasoPeaton)

UsoDirec_tau_1 = (tau_1-MODEL_UsoDirec) / SIGMA_STAR_UsoDirec
UsoDirec_tau_2 = (tau_2-MODEL_UsoDirec) / SIGMA_STAR_UsoDirec
UsoDirec_tau_3 = (tau_3-MODEL_UsoDirec) / SIGMA_STAR_UsoDirec
UsoDirec_tau_4 = (tau_4-MODEL_UsoDirec) / SIGMA_STAR_UsoDirec
IndUsoDirec = {
    1: bioNormalCdf(UsoDirec_tau_1),
    2: bioNormalCdf(UsoDirec_tau_2)-bioNormalCdf(UsoDirec_tau_1),
    3: bioNormalCdf(UsoDirec_tau_3)-bioNormalCdf(UsoDirec_tau_2),
    4: bioNormalCdf(UsoDirec_tau_4)-bioNormalCdf(UsoDirec_tau_3),
    5: 1-bioNormalCdf(UsoDirec_tau_4)
}

P_UsoDirec = Elem(IndUsoDirec, UsoDirec)


loglike = log(P_FRbr * P_EnfCond * P_AFrSem * P_CulFr * P_UsoPito * P_OmLmVel * P_IgPare *\
              P_UsoCel * P_PasoPeaton * P_UsoDirec)


# Define level of verbosity
import biogeme.messaging as msg
logger = msg.bioMessage()
#logger.setSilent()
#logger.setWarning()
logger.setGeneral()
#logger.setDetailed()


# Create the Biogeme object
biogeme  = bio.BIOGEME(database,loglike)
biogeme.modelName = "LVChoiceOrdered"

# Estimate the parameters
results = biogeme.estimate()

print(f"Estimated betas: {len(results.data.betaValues)}")
print(f"final log likelihood: {results.data.logLike:.3f}")
print(f"Output file: {results.data.htmlFileName}")
results.writeLaTeX()
print(f"LaTeX file: {results.data.latexFileName}")


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
     lambda1 * LV_1 + lambda2 * LV_2 + lambda3 * LV_3

# Associate utility functions with the numbering of alternatives
V = {1:V1, 2:V2, 3:V3, 4:V4}

# Associate the availability conditions with the alternatives.
# In this example all alternatives are available for each individual.
AV = {1:1, 2:1, 3:1, 4:1}

# Conditional to omega, we have a logit model (called the kernel)
condprob = models.logit(V,AV,CHOICE)

# We integrate over omega using numerical integration
loglike = log(Integrate(condprob * density,'omega'))

# Define level of verbosity
import biogeme.messaging as msg
logger = msg.bioMessage()
#logger.setSilent()
#logger.setWarning()
logger.setGeneral()
#logger.setDetailed()

# Create the Biogeme object
# Create the Biogeme object
biogeme = bio.BIOGEME(database,logprob,numberOfDraws=100)
biogeme.modelName = "LVChoiceSequencial"

# Estimate the parameters
# Estimate the parameters
biogeme.loadSavedIteration()
results = biogeme.estimate()
pandasResults = results.getEstimatedParameters()
print(pandasResults)

#print(f"Estimated betas: {len(results.data.betaValues)}")
#print(f"Final log likelihood: {results.data.logLike:.3f}")
print(f"Output file: {results.data.htmlFileName}")
results.writeLaTeX()
print(f"LaTeX file: {results.data.latexFileName}")

