#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat May 18 10:16:33 2019

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
#COEF_EDAD0 = Beta('COEF_EDAD0',0,None,None,0)
COEF_EDAD1 = Beta('COEF_EDAD1',0,None,None,0)
COEF_EDAD2 = Beta('COEF_EDAD2',0,None,None,0)
COEF_EDAD3 = Beta('COEF_EDAD3',0,None,None,0)
#COEF_EDAD4 = Beta('COEF_EDAD4',0,None,None,0)

#COEF_EDUCAC0 = Beta('COEF_EDUCAC0',0,None,None,0)
COEF_EDUCAC1 = Beta('COEF_EDUCAC1',0,None,None,0)
COEF_EDUCAC2 = Beta('COEF_EDUCAC2',0,None,None,0)
COEF_EDUCAC3 = Beta('COEF_EDUCAC3',0,None,None,0)
#COEF_EDUCAC4 = Beta('COEF_EDUCAC4',0,None,None,0)

#COEF_TPROF = Beta('COEF_TPROF',0,None,None,0)
#COEF_HORASTRAB = Beta('COEF_HORASTRAB',0,None,None,0)


# Ecuacion Estructural Variable Latente

AMBLAB = \
COEF_INTER+\
COEF_EDAD1*EDAD1+\
COEF_EDAD2*EDAD2+\
COEF_EDAD3*EDAD3+\
COEF_EDUCAC1*EDUCAC1+\
COEF_EDUCAC2*EDUCAC2+\
COEF_EDUCAC3*EDUCAC3

#COEF_EDAD4*EDAD4+COEF_TPROF*TPROF+\
#COEF_HORASTRAB*HORASTRAB

SIGMA_S = Beta('SIGMA_S',1,0,None,1)


### ECUACIONES DE MEDIDA
INTER_PP = Beta('INTER_PP',0,None,None,0)
INTER_AT = Beta('INTER_AT',0,None,None,0)
INTER_HPS = Beta('INTER_HPS',0,None,None,1)

BETA_PP_F1 = Beta('BETA_PP_F1',-1,None,None,0)
BETA_AT_F1 = Beta('BETA_AT_F1',-1,None,None,0)
BETA_HPS_F1 = Beta('BETA_HPS_F1',0,None,None,1)


MODEL_PP = INTER_PP + BETA_PP_F1 * AMBLAB
MODEL_AT = INTER_AT + BETA_AT_F1 * AMBLAB
MODEL_HPS = INTER_HPS + BETA_HPS_F1 * AMBLAB

SIGMA_STAR_PP = Beta('SIGMA_STAR_PP', 10,None,None,0)
SIGMA_STAR_AT = Beta('SIGMA_STAR_AT', 10,None,None,0)
SIGMA_STAR_HPS = Beta('SIGMA_STAR_HPS', 10,None,None,0)


F = {}
F['PresPersonal'] = Elem({0:0, \
 1:ll.loglikelihoodregression(PresPersonal,MODEL_PP,SIGMA_STAR_PP)},\
    (PresPersonal > 0)*(PresPersonal< 4))
F['AmbTrabajo'] = Elem({0:0, \
 1:ll.loglikelihoodregression(AmbTrabajo,MODEL_AT,SIGMA_STAR_AT)},
    (AmbTrabajo > 0)*(AmbTrabajo < 4))
F['HabProsoc'] = Elem({0:0, \
 1:ll.loglikelihoodregression(HabProsoc,MODEL_HPS,SIGMA_STAR_HPS)},
    (HabProsoc > 0)*(HabProsoc < 4))



loglike = bioMultSum(F)

biogeme  = bio.BIOGEME(database,loglike)
biogeme.modelName = "01oneLatentRegression"

results = biogeme.estimate()

print(f"Estimated betas: {len(results.data.betaValues)}")
print(f"final log likelihood: {results.data.logLike:.3f}")
print(f"Output file: {results.data.htmlFileName}")
results.writeLaTeX()
print(f"LaTeX file: {results.data.latexFileName}")