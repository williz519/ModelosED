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



pandas = pd.read_csv('/Users/williz/Desktop/ModelosED/Database/DBModeloLogitVLCE.csv', sep='\t')
database = db.Database("DBModLogitVLCE",pandas)

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
COEF_INTER = Beta('COEF_INTER',0,None,None,0)
COEF_JOVEN30 = Beta('COEF_JOVEN30',0,None,None,0)
COEF_ADULTO40 = Beta('COEF_ADULTO40',0,None,None,0)
COEF_ADULTO60 = Beta('COEF_ADULTO60',0,None,None,0)
COEF_ADULTOMAYOR = Beta('COEF_ADULTOMAYOR',0,None,None,0)
COEF_EDUC_BASICA = Beta('COEF_EDUC_BASICA',0,None,None,0)
COEF_EDUC_SUPERIOR = Beta('COEF_EDUC_SUPERIOR',0,None,None,0)
COEF_TPROF = Beta('COEF_TPROF',0,None,None,0)
COEF_HORASTRAB = Beta('COEF_HORASTRAB',0,None,None,0)
COEF_HORAPICO = Beta('COEF_HORAPICO',0,None,None,0)
COEF_HVALLE = Beta('COEF_HVALLE',0,None,None,0)
COEF_CLIMASECO = Beta('COEF_CLIMASECO',0,None,None,0)
COEF_CLIMALLUVIA = Beta('COEF_CLIMALLUVIA',0,None,None,0)
COEF_CONGAB = Beta('COEF_CONGAB',0,None,None,0)
COEF_CONGCD = Beta('COEF_CONGCD',0,None,None,0)
COEF_CONGEF = Beta('COEF_CONGEF',0,None,None,0)
COEF_SININFO = Beta('COEF_SININFO',0,None,None,0)
COEF_CONINFO = Beta('COEF_CONINFO',0,None,None,0)
COEF_USOCINTRURON = Beta('COEF_USOCINTURON',0,None,None,0)


# Ecuacion Estructural Variable Latente

ACTAGR = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA + COEF_HORASTRAB*HORASTRAB+\
    COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF+COEF_SININFO*SININFO

sigma_s1 = Beta('sigma_s1',1,0.001,None,1)


AMBLABOR = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA+ COEF_ADULTO60*ADULTO60 + COEF_HORASTRAB*HORASTRAB+\
    COEF_HORAPICO*HORAPICO+COEF_CONGCD*CONGCD+COEF_CONGEF*CONGEF

sigma_s2 = Beta('sigma_s2',1,0.001,None,1)

### ECUACIONES DE MEDIDA FACTOR ACTITUD AGRESIVA

INTER_FRbr= Beta('INTER_FRbr',0,None,None,1)
INTER_EnfCond = Beta('INTER_EnfCond',0,None,None,0)
INTER_AFrSem = Beta('INTER_AFrSem',0,None,None,0)
INTER_CulFr = Beta('INTER_CulFr',0,None,None,0)
INTER_OmLmVel = Beta('INTER_OmLmVel',0,None,None,1)
INTER_IgPare = Beta('INTER_IgPare',0,None,None,0)

BETA_FRbrF1 = Beta('BETA_FRbrF1',0,None,None,1)
BETA_EnfCondF1 = Beta('BETA_EnfCondF1',0,None,None,0)
BETA_AFrSemF1 = Beta('BETA_AFrSemF1',0,None,None,0)
BETA_CulFrF1 = Beta('BETA_CulFrF1',0,None,None,0)
BETA_OmLmVelF1 = Beta('BETA_OmLmVelF1',0,None,None,0)
BETA_IgPareF1 = Beta('BETA_IgPareF1',0,None,None,0)

MODEL_FRbr = INTER_FRbr + BETA_FRbrF1 * ACTAGR
MODEL_EnfCond = INTER_EnfCond + BETA_EnfCondF1 * ACTAGR
MODEL_AFrSem = INTER_AFrSem + BETA_AFrSemF1 * ACTAGR
MODEL_CulFr = INTER_CulFr + BETA_CulFrF1 * ACTAGR
MODEL_OmLmVel = INTER_OmLmVel + BETA_OmLmVelF1 * ACTAGR
MODEL_IgPare = INTER_IgPare + BETA_IgPareF1 * ACTAGR


SIGMA_STAR_FRbr = Beta('SIGMA_STAR_FRbr', 10,0.0001,10000,0)
SIGMA_STAR_EnfCond = Beta('SIGMA_STAR_EnfCond',10,0.0001,10000,0)
SIGMA_STAR_AFrSem = Beta('SIGMA_STAR_AFrSem', 10,0.0001,10000,0)
SIGMA_STAR_CulFr = Beta('SIGMA_STAR_CulFr', 10,0.0001,10000,0)
SIGMA_STAR_OmLmVel = Beta('SIGMA_STAR_OmLmVel', 10,0.0001,10000,0)
SIGMA_STAR_IgPare = Beta('SIGMA_STAR_IgPare', 10,0.0001,10000,0)


### ECUACIONES DE MEDIDA FACTOR AMBIENTE LABORAL

INTER_PrPers= Beta('INTER_PrPers',0,None,None,0)
INTER_AmbT= Beta('INTER_AmbT',0,None,None,0)

BETA_PrPersF2 = Beta('BETA_PrPersF2',-1,None,None,0)
BETA_AmbTF2 = Beta('BETA_AmbTF2',-1,None,None,0)

MODEL_PrPers = INTER_PrPers + BETA_PrPersF2 * AMBLABOR
MODEL_AmbT = INTER_AmbT + BETA_AmbTF2 * AMBLABOR

SIGMA_STAR_PrPers = Beta('SIGMA_STAR_PrPers',10,0.001,None,0)
SIGMA_STAR_AmbT = Beta('SIGMA_STAR_AmbT',10,0.001,None,0)



F = {}
F['FRbr'] = Elem({0:0, \
 1:ll.loglikelihoodregression(FRbr,MODEL_FRbr,SIGMA_STAR_FRbr)},\
  (FRbr > 0)*(FRbr < 6))
F['EnfCond'] = Elem({0:0, \
 1:ll.loglikelihoodregression(EnfCond,MODEL_EnfCond,SIGMA_STAR_EnfCond)},\
  (EnfCond > 0)*(EnfCond < 6))
F['AFrSem'] = Elem({0:0, \
 1:ll.loglikelihoodregression(AFrSem,MODEL_AFrSem,SIGMA_STAR_AFrSem)},\
  (AFrSem > 0)*(AFrSem < 6))
F['CulFr'] = Elem({0:0, \
 1:ll.loglikelihoodregression(CulFr,MODEL_CulFr,SIGMA_STAR_CulFr)},\
  (CulFr > 0)*(CulFr < 6))
F['IgPare'] = Elem({0:0, \
 1:ll.loglikelihoodregression(IgPare,MODEL_IgPare,SIGMA_STAR_IgPare)},\
  (IgPare > 0)*(IgPare < 6))
F['OmLmVel'] = Elem({0:0, \
 1:ll.loglikelihoodregression(OmLmVel,MODEL_OmLmVel,SIGMA_STAR_OmLmVel)},\
  (OmLmVel > 0)*(OmLmVel< 6))

F['PrPers'] = Elem({0:0, \
 1:ll.loglikelihoodregression(PrPers,MODEL_PrPers,SIGMA_STAR_PrPers)},\
  (PrPers > 0)*(PrPers< 6))
F['AmbT'] = Elem({0:0, \
 1:ll.loglikelihoodregression(AmbT,MODEL_AmbT,SIGMA_STAR_AmbT)},\
  (AmbT > 0)*(AmbT< 6))


loglike = bioMultSum(F)

biogeme  = bio.BIOGEME(database,loglike)
biogeme.modelName = "01LV2Medellin"
results = biogeme.estimate()
print(f"Estimated betas: {len(results.data.betaValues)}")
print(f"final log likelihood: {results.data.logLike:.3f}")
print(f"Output file: {results.data.htmlFileName}")
results.writeLaTeX()
print(f"LaTeX file: {results.data.latexFileName}")