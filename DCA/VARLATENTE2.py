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


#Cargar DataBase

pandas = pd.read_csv('/Users/williz/Dropbox/Doctorado/Resultados Tesis/BasesDatos/DBModeloLogitVL1.cvs', sep='\t')
database = db.Database("DBModLogitVL",pandas)

from headers import *

exclude = (CHOICE == -1.0)
database.remove(exclude)


#Variables
MENOR_60 = DefineVariable('MENOR_60', MENOR_60, database)
EDUC_BASICA = DefineVariable('EDUC_BASICA', EDUC_BASICA, database)
TPROF = DefineVariable('TPROF', TIEMPO_PROFESION, database)
HORASTRAB = DefineVariable('HORASTRAB', HORASTRABAJO, database)


# Coeficientes
COEF_INTER = Beta('COEF_INTER',0,None,None,0)
COEF_MENOR_60 = Beta('MENOR_60',0,None,None,0)
COEF_EDUC_BASICA = Beta('EDUC_BASICA',0,None,None,0)
COEF_TPROF = Beta('COEF_TPROF',0,None,None,0)
COEF_HORASTRAB = Beta('COEF_HORASTRAB',0,None,None,0)


# Ecuacion Estructural Variable Latente

sigma_s = Beta('sigma_s',1,0.001,None,1)

ACTAGRE = \
COEF_INTER + COEF_MENOR_60*MENOR_60 + COEF_EDUC_BASICA*EDUC_BASICA + COEF_EDAD4*EDAD4+COEF_TPROF*TPROF+\
COEF_HORASTRAB*HORASTRAB


### ECUACIONES DE MEDIDA
INTER_FRB = Beta('INTER_FRB',0,None,None,0)
INTER_AFRB = Beta('INTER_AFRB',0,None,None,0)
INTER_CCF = Beta('INTER_CCF',0,None,None,0)
INTER_ISP = Beta('INTER_ISP',0,None,None,0)
INTER_OLV = Beta('INTER_OLV',0,None,None,0)


BETA_FRB_F1 = Beta('BETA_FRB_F1',-1,None,None,0)
BETA_AFRB_F1 = Beta('BETA_AFRB_F1',-1,None,None,0)
BETA_CCF_F1 = Beta('BETA_CCF_F1',0,None,None,0)
BETA_ISP_F1 = Beta('BETA_ISP_F1',0,None,None,0)
BETA_OLV_F1 = Beta('BETA_OLV_F1',0,None,None,0)


MODEL_FRB = INTER_FRB + BETA_FRB_F1 * ACTAGRE
MODEL_AFRB = INTER_AFRB + BETA_AFRB_F1 * ACTAGRE
MODEL_CCF = INTER_CCF + BETA_CCF_F1 * ACTAGRE
MODEL_ISP = INTER_ISP + BETA_ISP_F1 * ACTAGRE
MODEL_OLV = INTER_OLV + BETA_OLV_F1 * ACTAGRE


SIGMA_STAR_FRB = Beta('SIGMA_STAR_FRB', 10,None,None,0)
SIGMA_STAR_AFRB = Beta('SIGMA_STAR_AFRB', 10,None,None,0)
SIGMA_STAR_CCF = Beta('SIGMA_STAR_CCF', 10,None,None,0)
SIGMA_STAR_ISP = Beta('SIGMA_STAR_ISP', 10,None,None,0)
SIGMA_STAR_OLV = Beta('SIGMA_STAR_OLV', 10,None,None,0)


F = {}
F['FrenoRapidoBrusco'] = Elem({0:0, \
 1:ll.loglikelihoodregression(FrenoRapidoBrusco,MODEL_FRB,SIGMA_STAR_FRB)},\
  (EFrenoRapidoBrusco > 0)*(FrenoRapidoBrusco < 6))
F['AceleraFrenaBruscamenteSemaforo'] = Elem({0:0, \
 1:ll.loglikelihoodregression(AceleraFrenaBruscamenteSemaforo,MODEL_AFRB,SIGMA_STAR_AFRB)},\
  (AceleraFrenaBruscamenteSemaforo > 0)*(AceleraFrenaBruscamenteSemaforo < 6))
F['CulebreaConFrecuencia'] = Elem({0:0, \
 1:ll.loglikelihoodregression(CulebreaConFrecuencia,MODEL_CCF,SIGMA_STAR_CCF)},\
  (CulebreaConFrecuencia > 0)*(CulebreaConFrecuencia < 6))
F['IgnoraSenhalPare'] = Elem({0:0, \
 1:ll.loglikelihoodregression(IgnoraSenhalPare,MODEL_ISP,SIGMA_STAR_ISP)},\
  (IgnoraSenhalPare > 0)*(IgnoraSenhalPare < 6))
F['OmiteLimiteVelocidad'] = Elem({0:0, \
 1:ll.loglikelihoodregression(OmiteLimiteVelocidad,MODEL_OLV,SIGMA_STAR_OLV)},\
  (OmiteLimiteVelocidad > 0)*(OmiteLimiteVelocidad< 6))

  loglike = bioMultSum(F)

biogeme  = bio.BIOGEME(database,loglike)
biogeme.modelName = "01oneLatentRegression"
results = biogeme.estimate()
print(f"Estimated betas: {len(results.data.betaValues)}")
print(f"final log likelihood: {results.data.logLike:.3f}")
print(f"Output file: {results.data.htmlFileName}")
results.writeLaTeX()
print(f"LaTeX file: {results.data.latexFileName}")