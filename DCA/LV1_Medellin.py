#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat May 18 15:52:01 2019

@author: williz
"""

# Cargar Paquetes

import pandas as pd
import numpy as np
import biogeme.database as db
import biogeme.biogeme as bio
from biogeme.models import piecewise
import biogeme.loglikelihood as ll
import biogeme.models as models
import biogeme.distributions as dist
import biogeme.results as res

#Cargar DataBase

pandas = pd.read_csv('/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVL2.csv', sep='\t')
database = db.Database("DBModLogitVL2",pandas)

from headers import *

exclude = (CHOICE == -1.0)
database.remove(exclude)


#Variables
TIME1_SC = DefineVariable('TIME1_SC', TIEMPOAlt1/100, database)
TIME2_SC = DefineVariable('TIME2_SC', TIEMPOAlt2/100, database)
TIME3_SC = DefineVariable('TIME3_SC', TIEMPOAlt3/100, database)
TIME4_SC = DefineVariable('TIME4_SC', TIEMPOEC/100, database)
DIST1_SC = DefineVariable('DIST1_SC', DISTAlt1/100, database)
DIST2_SC = DefineVariable('DIST2_SC', DISTAlt2/100, database)
DIST3_SC = DefineVariable('DIST3_SC', DISTAlt3/100, database)
DIST4_SC = DefineVariable('DIST4_SC', DISTEC/100, database)
JOVEN30 = DefineVariable('JOVEN30', EDAD == 1, database)
ADULTO40 = DefineVariable('ADULTO40', EDAD == 2, database)
ADULTO60 = DefineVariable('ADULTO60', EDAD == 3, database)
ADULTOMAYOR = DefineVariable('ADULTOMAYOR', EDAD == 4, database)
EDUC_BASICA = DefineVariable('EDUC_BASICA', NIVELEDUCATIVO == 1, database)
EDUC_SUPERIOR = DefineVariable('EDUC_SUPERIOR', NIVELEDUCATIVO >= 2, database)
TPROF = DefineVariable('TPROF', TIEMPO_PROFESION*12, database)
HORASTRAB = DefineVariable('HORASTRAB', HORASTRABAJO, database)
HORAPICO = DefineVariable('HORAPICO', HPICOHVALLE == 1, database)
HORAVALLE = DefineVariable('HORAVALLE', HPICOHVALLE == 2, database)
CLIMASECO = DefineVariable('CLIMASECO', CLIMA == 1, database)
CLIMALLUVIA = DefineVariable('CLIMALLUVIA', CLIMA == 2, database)
CONGAB = DefineVariable('CONGAB', CONGESTION <= 2, database)
CONGCD = DefineVariable('CONGCD', (CONGESTION == 3)+(CONGESTION == 4), database)
CONGEF = DefineVariable('CONGEF', CONGESTION > 4, database)
SININFO = DefineVariable('SININFO', INFOTRAFICO == 1, database)
CONINFO = DefineVariable('CONINFO', INFOTRAFICO == 2, database)
USOCINTURON = DefineVariable ('USOCINTURON', CinSeg == 2, database)



# Coeficientes
COEF_INTER = Beta('COEF_INTER',-9.04,None,None,0)
COEF_JOVEN30 = Beta('COEF_JOVEN30',0,None,None,0)
COEF_ADULTO40 = Beta('COEF_ADULTO40',0,None,None,0)
COEF_ADULTO60 = Beta('COEF_ADULTO60',0,None,None,0)
COEF_ADULTOMAYOR = Beta('COEF_ADULTOMAYOR',0,None,None,0)
COEF_EDUC_BASICA = Beta('COEF_EDUC_BASICA',1.04,None,None,0)
COEF_EDUC_SUPERIOR = Beta('COEF_EDUC_SUPERIOR',0,None,None,0)
COEF_TPROF = Beta('COEF_TPROF',0,None,None,0)
COEF_HORASTRAB = Beta('COEF_HORASTRAB',-0.274,None,None,0)
COEF_HORAPICO = Beta('COEF_HORAPICO',0.67,None,None,0)
COEF_HVALLE = Beta('COEF_HVALLE',0,None,None,0)
COEF_CLIMASECO = Beta('COEF_CLIMASECO',0,None,None,0)
COEF_CLIMALLUVIA = Beta('COEF_CLIMALLUVIA',0,None,None,0)
COEF_CONGAB = Beta('COEF_CONGAB',0,None,None,0)
COEF_CONGCD = Beta('COEF_CONGCD',1.8,None,None,0)
COEF_CONGEF = Beta('COEF_CONGEF',2.83,None,None,0)
COEF_SININFO = Beta('COEF_SININFO',-0.955,None,None,0)
COEF_CONINFO = Beta('COEF_CONINFO',0,None,None,0)
COEF_USOCINTRURON = Beta('COEF_USOCINTURON',0,None,None,0)


# Ecuacion Estructural Variable Latente

omega = RandomVariable('omega')
density = dist.normalpdf(omega) 
sigma_s = Beta('sigma_s',1,None,None,0)

ACTAGR = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA+COEF_HORASTRAB*HORASTRAB +\
    COEF_HORAPICO*HORAPICO + COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF+COEF_SININFO*SININFO 



### ECUACIONES DE MEDIDA
INTER_FRbr= Beta('INTER_FRbr',0,None,None,1)
INTER_EnfCond = Beta('INTER_EnfCond',-0.0344,None,None,0)
INTER_AFrSem = Beta('INTER_AFrSem',3.79,None,None,0)
INTER_CulFr = Beta('INTER_CulFr',15.7,None,None,0)
INTER_OmLmVel = Beta('INTER_OmLmVel',-0.149,None,None,0)
INTER_IgPare = Beta('INTER_IgPare',-3.21,None,None,0)

BETA_FRbrF1 = Beta('BETA_FRbrF1',1,None,None,1)
BETA_EnfCondF1 = Beta('BETA_EnfCondF1',1.15,None,None,0)
BETA_AFrSemF1 = Beta('BETA_AFrSemF1',1.41,None,None,1)
BETA_CulFrF1 = Beta('BETA_CulFrF1',1.79,None,None,0)
BETA_OmLmVelF1 = Beta('BETA_OmLmVelF1',1.12,None,None,0)
BETA_IgPareF1 = Beta('BETA_IgPareF1',0.917,None,None,0)

MODEL_FRbr = INTER_FRbr + BETA_FRbrF1 * ACTAGR
MODEL_EnfCond = INTER_EnfCond + BETA_EnfCondF1 * ACTAGR
MODEL_AFrSem = INTER_AFrSem + BETA_AFrSemF1 * ACTAGR
MODEL_CulFr = INTER_CulFr + BETA_CulFrF1 * ACTAGR
MODEL_OmLmVel = INTER_OmLmVel + BETA_OmLmVelF1 * ACTAGR
MODEL_IgPare = INTER_IgPare + BETA_IgPareF1 * ACTAGR


SIGMA_STAR_FRbr = Beta('SIGMA_STAR_FRbr', 1,None,None,1)
SIGMA_STAR_EnfCond = Beta('SIGMA_STAR_EnfCond', 7.18,None,None,0)
SIGMA_STAR_AFrSem = Beta('SIGMA_STAR_AFrSem', 9.15,None,None,0)
SIGMA_STAR_CulFr = Beta('SIGMA_STAR_CulFr', 10.1,None,None,0)
SIGMA_STAR_OmLmVel = Beta('SIGMA_STAR_OmLmVel', 8.27,None,None,0)
SIGMA_STAR_IgPare = Beta('SIGMA_STAR_IgPare', 7.06,None,None,0)

delta_1 = Beta('delta_1',0.25196349820243613,0,None,0 )
delta_2 = Beta('delta_2',0.759172317380935,0,None,0 )
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
    5: 1-bioNormalCdf(FRbr_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
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
    5: 1-bioNormalCdf(EnfCond_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
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
    5: 1-bioNormalCdf(AFrSem_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
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
    5: 1-bioNormalCdf(CulFr_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
}

P_CulFr = Elem(IndCulFr, CulFr)

OmLmVel_tau_1 = (tau_1-MODEL_OmLmVel) / SIGMA_STAR_OmLmVel
OmLmVel_tau_2 = (tau_2-MODEL_OmLmVel) / SIGMA_STAR_OmLmVel
OmLmVel_tau_3 = (tau_3-MODEL_OmLmVel) / SIGMA_STAR_OmLmVel
OmLmVel_tau_4 = (tau_4-MODEL_OmLmVel) / SIGMA_STAR_OmLmVel
IndOmLmVel = {
    1: bioNormalCdf(OmLmVel_tau_1),
    2: bioNormalCdf(OmLmVel_tau_2)-bioNormalCdf(OmLmVel_tau_1),
    3: bioNormalCdf(OmLmVel_tau_3)-bioNormalCdf(OmLmVel_tau_2),
    4: bioNormalCdf(OmLmVel_tau_4)-bioNormalCdf(OmLmVel_tau_3),
    5: 1-bioNormalCdf(OmLmVel_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
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
    5: 1-bioNormalCdf(IgPare_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
}

P_IgPare = Elem(IndIgPare, IgPare)


# Modelo de Elección
# Lee las estimaciones de la estimación secuencial y usa estos como valores
# iniciales 

ASC_ALT1 = Beta('ASC_ALT1',0,None,None,1)
ASC_ALT2 = Beta('ASC_ALT2',0,None,None,0)
ASC_ALT3 = Beta('ASC_ALT3',0,None,None,0)
ASC_ER = Beta('ASC_ER',0,None,None,0)


B_TIME_ALT1_REF = Beta('B_TIME_ALT1_REF',0,None,None,0)
B_TIME_ALT1_CL = Beta('B_TIME_ALT1_CL',0,None,None,0)
B_DIST_ALT1_REF = Beta('B_DIST_ALT1_REF',0,None,None,0)
B_DIST_ALT1_CL = Beta('B_DIST_ALT1_CL',0,None,None,0)

B_TIME_ALT2_REF = Beta('B_TIME_ALT2_REF',0,None,None,0)
B_TIME_ALT2_CL = Beta('B_TIME_ALT2_CL',0,None,None,0)
B_DIST_ALT2_REF = Beta('B_DIST_ALT2_REF',0,None,None,0)
B_DIST_ALT2_CL = Beta('B_DIST_ALT2_CL',0,None,None,0)

B_TIME_ALT3_REF = Beta('B_TIME_ALT3_REF',0,None,None,0)
B_TIME_ALT3_CL = Beta('B_TIME_ALT3_CL',0,None,None,0)
B_DIST_ALT3_REF = Beta('B_DIST_ALT3_REF',0,None,None,0)
B_DIST_ALT3_CL = Beta('B_DIST_ALT3_CL',0,None,None,0)

B_TIME_ER_REF = Beta('B_TIME_ER_REF',0,None,None,0)
B_TIME_ER_CL = Beta('B_TIME_ER_CL',0,None,None,0)
B_DIST_ER_REF = Beta('B_DIST_ER_REF',0,None,None,0)
B_DIST_ER_CL = Beta('B_DIST_ER_CL',0,None,None,0)



### Definición de las funciones de Utilidad:

BETA_TIME_ALT1 = B_TIME_ALT1_REF * exp(B_TIME_ALT1_CL * ACTAGR)
BETA_DIST_ALT1 = B_DIST_ALT1_REF * exp(B_DIST_ALT1_CL * ACTAGR)


V1 = ASC_ALT1+BETA_TIME_ALT1*TIME1_SC+BETA_DIST_ALT1*DIST1_SC+COEF_JOVEN30*JOVEN30+\
    COEF_ADULTO40*ADULTO40+COEF_ADULTO60*ADULTO60+COEF_EDUC_BASICA*EDUC_BASICA+COEF_TPROF*TPROF+COEF_HORASTRAB*HORASTRAB+\
        COEF_HORAPICO*HORAPICO+COEF_CLIMASECO*CLIMASECO*CLIMASECO+COEF_CONGCD*CONGCD+COEF_CONGEF*CONGEF+\
            COEF_SININFO*SININFO

BETA_TIME_ALT2= B_TIME_ALT2_REF * exp(B_TIME_ALT2_CL * ACTAGR)
BETA_DIST_ALT2= B_DIST_ALT2_REF * exp(B_DIST_ALT2_CL * ACTAGR)
  
V2 = ASC_ALT2+BETA_TIME_ALT2*TIME2_SC+BETA_DIST_ALT2*DIST2_SC

BETA_TIME_ALT3= B_TIME_ALT3_REF * exp(B_TIME_ALT3_CL * ACTAGR)
BETA_DIST_ALT3= B_DIST_ALT3_REF * exp(B_DIST_ALT3_CL * ACTAGR)

V3 = ASC_ALT3+BETA_TIME_ALT3*TIME3_SC+BETA_DIST_ALT3*DIST3_SC

BETA_TIME_ER= B_TIME_ER_REF * exp(B_TIME_ER_CL * ACTAGR)
BETA_DIST_ER= B_DIST_ER_REF * exp(B_DIST_ER_CL * ACTAGR)

V4 = ASC_ER+BETA_TIME_ER*TIME4_SC+BETA_DIST_ER*DIST4_SC


# Associate utility functions with the numbering of alternatives
V = {1: V1,
     2: V2,
     3: V3,
     4: V4}

# Associate the availability conditions with the alternatives.
# In this example all alternatives are available for each individual.
av = {1: 1,
      2: 1,
      3: 1,
      4: 1}

# The choice model is a logit, conditional to the value of the latent variable
condprob = models.logit(V,av,CHOICE)
condlike = P_FRbr * \
          P_EnfCond * \
          P_AFrSem * \
          P_CulFr * \
          P_OmLmVel * \
          P_IgPare * \
          condprob

loglike = log(Integrate(condlike * density,'omega')+0.1)

biogeme  = bio.BIOGEME(database,loglike)
biogeme.modelName = "LV1_Optima"
results = biogeme.estimate()
# Get the results in a pandas table
pandasResults = results.getEstimatedParameters()
print(pandasResults)
print(f"Nbr of observations: {database.getNumberOfObservations()}")
print(f"LL(0) =    {results.data.initLogLike:.3f}")
print(f"LL(beta) = {results.data.logLike:.3f}")
print(f"rho bar square = {results.data.rhoBarSquare:.3g}")
print(f"Output file: {results.data.htmlFileName}")




