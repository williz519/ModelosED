#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon May 20 10:34:14 2019

@author: williz
"""


import pandas as pd
import numpy as np
import biogeme.database as db
import biogeme.biogeme as bio
from biogeme.models import piecewise
import biogeme.loglikelihood as ll


#Cargar DataBase

pandas = pd.read_csv('/Users/williz/Dropbox/Doctorado/Tesis Doctorado/Avances Tesis/Analisis Datos Tesis/Analisis en R/Modelo Logit DB/DBModLogitVL.csv', sep='\t')
database = db.Database("DBModLogitVL",pandas)

from headers import *

exclude = (CHOICE == -1.0)
database.remove(exclude)


#Variables
EDAD0 = DefineVariable('EDAD0',EDAD == 0, database)
EDAD1 = DefineVariable('EDAD1',EDAD == 1, database)
EDAD2 = DefineVariable('EDAD2',EDAD == 2, database)
EDAD3 = DefineVariable('EDAD3',EDAD == 3, database)
EDAD4 = DefineVariable('EDAD4',EDAD == 4, database)

EDUCAC0 = DefineVariable('EDUCAC0', NIVELEDUCATIVO == 0, database)
EDUCAC1 = DefineVariable('EDUCAC1', NIVELEDUCATIVO == 1, database)
EDUCAC2 = DefineVariable('EDUCAC2', NIVELEDUCATIVO == 2, database)
EDUCAC3 = DefineVariable('EDUCAC3', NIVELEDUCATIVO == 3, database)
EDUCAC4 = DefineVariable('EDUCAC4', NIVELEDUCATIVO == 4, database)

TPROF = DefineVariable('TPROF', TIEMPO_PROFESION, database)
HORASTRAB = DefineVariable('HORASTRAB', HORASTRABAJO, database)


# Coeficientes
COEF_INTER = Beta('COEF_INTER',0,None,None,0)
COEF_EDAD0 = Beta('COEF_EDAD0',0,None,None,0)
COEF_EDAD1 = Beta('COEF_EDAD1',0,None,None,0)
COEF_EDAD2 = Beta('COEF_EDAD2',0,None,None,0)
COEF_EDAD3 = Beta('COEF_EDAD3',0,None,None,0)
COEF_EDAD4 = Beta('COEF_EDAD4',0,None,None,0)

COEF_EDUCAC0 = Beta('COEF_EDUCAC0',0,None,None,0)
COEF_EDUCAC1 = Beta('COEF_EDUCAC1',0,None,None,0)
COEF_EDUCAC2 = Beta('COEF_EDUCAC2',0,None,None,0)
COEF_EDUCAC3 = Beta('COEF_EDUCAC3',0,None,None,0)
COEF_EDUCAC4 = Beta('COEF_EDUCAC4',0,None,None,0)

COEF_TPROF = Beta('COEF_TPROF',0,None,None,0)
COEF_HORASTRAB = Beta('COEF_HORASTRAB',0,None,None,0)


# Ecuacion Estructural Variable Latente

AMBLAB = \
COEF_INTER+\
COEF_EDAD0*EDAD0+\
COEF_EDAD1*EDAD1+\
COEF_EDAD2*EDAD2+\
COEF_EDAD3*EDAD3+\
COEF_EDAD4*EDAD4+\
COEF_EDUCAC0*EDUCAC0+\
COEF_EDUCAC1*EDUCAC1+\
COEF_EDUCAC2*EDUCAC2+\
COEF_EDUCAC3*EDUCAC3
COEF_EDUCAC4*EDUCAC4+\
COEF_EDAD4*EDAD4+COEF_TPROF*TPROF+\
COEF_HORASTRAB*HORASTRAB

SIGMA_S = Beta('SIGMA_S',1,0,None,1)


### ECUACIONES DE MEDIDA
INTER_PP = Beta('INTER_PP',0,None,None,1)
INTER_AT = Beta('INTER_AT',0,None,None,0)
INTER_HPS = Beta('INTER_HPS',0,None,None,0)

BETA_PP_F1 = Beta('BETA_PP_F1',-1,None,None,1)
BETA_AT_F1 = Beta('BETA_AT_F1',-1,None,None,0)
BETA_HPS_F1 = Beta('BETA_HPS_F1',0,None,None,0)


MODEL_PP = INTER_PP + BETA_PP_F1 * AMBLAB
MODEL_AT = INTER_AT + BETA_AT_F1 * AMBLAB
MODEL_HPS = INTER_HPS + BETA_HPS_F1 * AMBLAB

SIGMA_STAR_PP = Beta('SIGMA_STAR_PP', 10,None,None,0)
SIGMA_STAR_AT = Beta('SIGMA_STAR_AT', 10,None,None,0)
SIGMA_STAR_HPS = Beta('SIGMA_STAR_HPS', 10,None,None,0)


delta_1 = Beta('delta_1',0.1,0,10,0 )
#delta_2 = Beta('delta_2',0.2,0,10,0 )
tau_1 = -delta_1
tau_2 = delta_1


PP_tau_1 = (tau_1-MODEL_PP) / SIGMA_STAR_PP
PP_tau_2 = (tau_2-MODEL_PP) / SIGMA_STAR_PP

IndPP = {
    1: bioNormalCdf(PP_tau_1),
    2: bioNormalCdf(PP_tau_2)-bioNormalCdf(PP_tau_1),
    3: 1-bioNormalCdf(PP_tau_2),
    4: 1.0,
    0: 1.0,
    -1: 1.0
}

P_PP = Elem(IndPP, PresPersonal)


AT_tau_1 = (tau_1-MODEL_AT) / SIGMA_STAR_AT
AT_tau_2 = (tau_2-MODEL_AT) / SIGMA_STAR_AT

IndAT = {
    1: bioNormalCdf(AT_tau_1),
    2: bioNormalCdf(AT_tau_2)-bioNormalCdf(AT_tau_1),
    3: 1- bioNormalCdf(AT_tau_2),
    4: 1.0,
    0: 1.0,
    -1: 1.0
}

P_AT = Elem(IndAT, AmbTrabajo)

HPS_tau_1 = (tau_1-MODEL_HPS) / SIGMA_STAR_HPS
HPS_tau_2 = (tau_2-MODEL_HPS) / SIGMA_STAR_HPS

IndHPS = {
    1: bioNormalCdf(HPS_tau_1),
    2: bioNormalCdf(HPS_tau_2)-bioNormalCdf(HPS_tau_1),
    3: 1-bioNormalCdf(HPS_tau_2),
    4: 1.0,
    0: 1.0,
    -1: 1.0
}

P_HPS = Elem(IndHPS, HabProsoc)


# Choice model
# Read the estimates from the sequential estimation, and use
# them as starting values

choiceResults = res.bioResults(pickleFile='04latentChoiceSeq.pickle')
choiceBetas = choiceResults.getBetaValues()


ASC_ALT1 = Beta('ASC_ALT1',choiceBetas['ASC_ALT1'],None,None,0)
ASC_ALT2 = Beta('ASC_ALT2',choiceBetas['ASC_ALT2'],None,None,0)
ASC_ALT3 = Beta('ASC_ALT3',choiceBetas['ASC_ALT3'],None,None,0)
ASC_EC = Beta('ASC_ALT4',0,None,None,1)

B_TIME_ALT1_REF = Beta('B_TIME_ALT1_REF',choiceBetas['B_TIME_ALT1_REF'],None,0,0)
B_TIME_ALT1_CL = Beta('B_TIME_ALT1_CL',choiceBetas['B_TIME_ALT1_CL'],None,0,0)

B_TIME_ALT2_REF = Beta('B_TIME_ALT2_REF',choiceBetas['B_TIME_ALT2_REF'],None,0,0)
B_TIME_ALT2_CL = Beta('B_TIME_ALT2_CL',choiceBetas['B_TIME_ALT2_CL'],None,0,0)

B_TIME_ALT3_REF = Beta('B_TIME_ALT3_REF',choiceBetas['B_TIME_ALT3_REF'],None,0,0)
B_TIME_ALT3_CL = Beta('B_TIME_ALT3_CL',choiceBetas['B_TIME_ALT3_CL'],None,0,0)

B_TIME_ALT4_REF = Beta('B_TIME_ALT4_REF',choiceBetas['B_TIME_ALT4_REF'],None,0,0)
B_TIME_ALT4_CL = Beta('B_TIME_ALT4_CL',choiceBetas['B_TIME_ALT4_CL'],None,0,0)

B_DIST = Beta('B_DIST',choiceBetas['B_DIST'],None,None,0)
B_VEL = Beta('B_VEL',choiceBetas['B_VEL'],None,None,0)


TIME1_SC = DefineVariable('TIME1_SC', TIEMPOAlt1/100, database)
TIME2_SC = DefineVariable('TIME2_SC', TIEMPOAlt2/100, database)
TIME3_SC = DefineVariable('TIME3_SC', TIEMPOAlt3/100, database)
TIME4_SC = DefineVariable('TIME4_SC', TIEMPOEC/100, database)
DIST1_SC = DefineVariable('DIST1_SC', DISTAlt1/100, database)
DIST2_SC = DefineVariable('DIST2_SC', DISTAlt2/100, database)
DIST3_SC = DefineVariable('DIST3_SC', DISTAlt3/100, database)
DIST4_SC = DefineVariable('DIST4_SC', DISTEC/100, database)
VEL1_SC = DefineVariable('VEL1_SC', VELAlt1/100, database)
VEL2_SC = DefineVariable('VEL2_SC', VELAlt2/100, database)
VEL3_SC = DefineVariable('VEL3_SC', VELAlt3/100, database)
VEL4_SC = DefineVariable('VEL4_SC', VELPROMEC/100, database)



### DEFINITION OF UTILITY FUNCTIONS:

B_TIME_ALT1 = B_TIME_ALT1_REF * exp(B_TIME_ALT1_CL * AMBLAB)

V1 = ASC_ALT1+\
    B_TIME_ALT1*TIME1_SC+\
    B_DIST*DIST1_SC+\
    B_VEL*VEL1_SC

B_TIME_ALT2 = B_TIME_ALT2_REF * exp(B_TIME_ALT2_CL * AMBLAB)

V2 = ASC_ALT2+\
    B_TIME_ALT2*TIME2_SC+\
    B_DIST*DIST2_SC+\
    B_VEL*VEL2_SC

B_TIME_ALT3 = B_TIME_ALT3_REF * exp(B_TIME_ALT3_CL * AMBLAB)

V3 = ASC_ALT3+\
    B_TIME_ALT3*TIME3_SC+\
    B_DIST*DIST3_SC+\
    B_VEL*VEL3_SC 

B_TIME_ALT4 = B_TIME_ALT4_REF * exp(B_TIME_ALT4_CL * AMBLAB)

V4 = ASC_EC+\
    B_TIME_ALT4*TIME4_SC+\
    B_DIST*DIST4_SC+\
    B_VEL*VEL4_SC 

# Associate utility functions with the numbering of alternatives

V = {1:V1,
     2:V2,
     3:V3,
     4:V4}


# Associate the availability conditions with the alternatives.
# In this example all alternatives are available for each individual.
av = {1: 1,
      2: 1,
      3: 1,
      4:1}

# The choice model is a logit, conditional to the value of the latent variable
condprob = models.logit(V,av,CHOICE)
condlike = P_PP * \
          P_AT * \
          P_HPS * \
          condprob

loglike = log(Integrate(condlike * density,'omega'))

biogeme  = bio.BIOGEME(database,loglike)
biogeme.modelName = "05latentChoiceFull"
results = biogeme.estimate()
print(f"Estimated betas: {len(results.data.betaValues)}")
print(f"Final log likelihood: {results.data.logLike:.3f}")
print(f"Output file: {results.data.htmlFileName}")
results.writeLaTeX()
print(f"LaTeX file: {results.data.latexFileName}")
