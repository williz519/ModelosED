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

pandas = pd.read_csv('/Users/williz/Desktop/ModelosED/Database/DBModLogitMuestraVLCE1.csv', sep='\t')
database = db.Database("DBModLogitMuestraVLCE1",pandas)

from headers import *

exclude = (CHOICE == -1.0)
database.remove(exclude)


#Variables
TIME1_SC = DefineVariable('TIME1_SC', TIEMPOAlt1, database)
TIME2_SC = DefineVariable('TIME2_SC', TIEMPOAlt2, database)
TIME3_SC = DefineVariable('TIME3_SC', TIEMPOAlt3, database)
TIME4_SC = DefineVariable('TIME4_SC', TIEMPOEC, database)
DIST1_SC = DefineVariable('DIST1_SC', DISTAlt1, database)
DIST2_SC = DefineVariable('DIST2_SC', DISTAlt2, database)
DIST3_SC = DefineVariable('DIST3_SC', DISTAlt3, database)
DIST4_SC = DefineVariable('DIST4_SC', DISTEC, database)
JOVEN30 = DefineVariable('JOVEN30', EDAD == 1, database)
ADULTO40 = DefineVariable('ADULTO40', EDAD == 2, database)
ADULTO60 = DefineVariable('ADULTO60', EDAD == 3, database)
ADULTOMAYOR = DefineVariable('ADULTOMAYOR', EDAD == 4, database)
EDUC_BASICA = DefineVariable('EDUC_BASICA', NIVELEDUCATIVO == 1, database)
EDUC_SUPERIOR = DefineVariable('EDUC_SUPERIOR', NIVELEDUCATIVO >= 2, database)
TPROF = DefineVariable('TPROF', TIEMPO_PROFESION, database)
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
CONDAGREF1 = DefineVariable('CONDAGRESF1', CondAgres, database)
AMBLABORF2 = DefineVariable('AMBLABORF2', AmbienteLaboral, database)
HABPROSOCF3 = DefineVariable('HABPROSOCF3', HabProsoc, database)
STRESSF4 = DefineVariable('STRESSF4', Stress, database)


### Coefficientes
# Coeficientes
COEF_INTER = Beta('COEF_INTER',-21.8,None,None,1)
COEF_EDUC_BASICA = Beta('COEF_EDUC_BASICA',2.05,None,None,1)
COEF_JOVEN30 = Beta('COEF_JOVEN30',2.05,None,None,1)
COEF_ADULTO40 = Beta('COEF_ADULTO40',-0.462,None,None,1)
COEF_ADULTO60 = Beta('COEF_ADULTO60',-1.06,None,None,1)
COEF_ADULTOMAYOR = Beta('COEF_ADULTOMAYOR',0,None,None,1)
COEF_EDUC_SUPERIOR = Beta('COEF_EDUC_SUPERIOR',0,None,None,1)
COEF_TPROF = Beta('COEF_TPROF',0.0238,None,None,1)
COEF_HORASTRAB = Beta('COEF_HORASTRAB',-6.96,None,None,1)
COEF_HORAPICO = Beta('COEF_HORAPICO',1.87,None,None,1)
COEF_HVALLE = Beta('COEF_HVALLE',0,None,None,1)
COEF_CLIMASECO = Beta('COEF_CLIMASECO',1.19,None,None,1)
COEF_CLIMALLUVIA = Beta('COEF_CLIMALLUVIA',0,None,None,1)
COEF_CONGAB = Beta('COEF_CONGAB',0,None,None,1)
COEF_CONGCD = Beta('COEF_CONGCD',2.43,None,None,1)
COEF_CONGEF = Beta('COEF_CONGEF',5.34,None,None,1)
COEF_SININFO = Beta('COEF_SININFO',-1.98,None,None,1)
COEF_CONINFO = Beta('COEF_CONINFO',0,None,None,1)
COEF_USOCINTRURON = Beta('COEF_USOCINTURON',0,None,None,1)
COEF_CONDAGREF1 = Beta('COEF_CONDAGREF1',0,None,None,1)
COEF_AMBLABORF2 = Beta('COEF_AMBLABORF2',0,None,None,1)
COEF_HABPROSOCF3 = Beta('COEF_HABPROSOCF3',12.5,None,None,1)
COEF_STRESSF4 = Beta('COEF_STRESSF4',0,None,None,1)



# Ecuacion Estructural Variable Latente

omega_LV1 = bioDraws('omega_LV1','NORMAL')
#density = dist.normalpdf(omega) 
Beta_sigma_s = Beta('Beta_sigma_s',10,None,None,0)


ACTAGR = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA+\
    COEF_JOVEN30*JOVEN30 + COEF_ADULTO40*ADULTO40+\
    COEF_ADULTO60*ADULTO60+COEF_TPROF*TPROF+\
    COEF_HORASTRAB*HORASTRAB+COEF_HORAPICO*HORAPICO+\
    COEF_CLIMASECO*CLIMASECO+COEF_CONGCD*CONGCD + \
    COEF_CONGEF*CONGEF+COEF_SININFO*SININFO+\
    COEF_HABPROSOCF3*HABPROSOCF3 + Beta_sigma_s * omega_LV1


# Modelo de Elección

ASC_ALT1 = Beta('ASC_ALT1',1,None,None,1)
ASC_ALT2 = Beta('ASC_ALT2',0,None,None,0)
ASC_ALT3 = Beta('ASC_ALT3',0,None,None,0)
ASC_EC = Beta('ASC_ALT4',0,None,None,0)
B_TIME_ALT1 = Beta('B_TIME_ALT1',0,None,None,0)
B_TIME_ALT2 = Beta('B_TIME_ALT2',0,None,None,0)
B_TIME_ALT3 = Beta('B_TIME_ALT3',0,None,None,0)
B_DIST = Beta('B_DIST',0,None,None,0)


B_TIME_ALT1_REF = Beta('B_TIME_ALT1_REF',0,None,None,0)
B_TIME_ALT1_CL = Beta('B_TIME_ALT1_CL',0,None,None,0)
B_TIME_ALT2_REF = Beta('B_TIME_ALT2_REF',0,None,None,0)
B_TIME_ALT2_CL = Beta('B_TIME_ALT2_CL',0,None,None,0)
B_TIME_ALT3_REF = Beta('B_TIME_ALT3_REF',0,None,None,0)
B_TIME_ALT3_CL = Beta('B_TIME_ALT3_CL',0,None,None,0)
B_TIME_EC_REF = Beta('B_TIME_EC_REF',0,None,None,0)
B_TIME_EC_CL = Beta('B_TIME_EC_CL',0,None,None,0)

#B_DIST_ALT1_REF = Beta('B_DIST_ALT1_REF',0,None,None,0)
#B_DIST_ALT1_CL = Beta('B_DIST_ALT1_CL',0,None,None,0)
#B_DIST_ALT2_REF = Beta('B_DIST_ALT2_REF',0,None,None,0)
#B_DIST_ALT2_CL = Beta('B_DIST_ALT2_CL',0,None,None,0)
#B_DIST_ALT3_REF = Beta('B_DIST_ALT3_REF',0,None,None,0)
#B_DIST_ALT3_CL = Beta('B_DIST_ALT3_CL',0,None,None,0)
#B_DIST_EC_REF = Beta('B_DIST_EC_REF',0,None,None,0)
#B_DIST_EC_CL = Beta('B_DIST_EC_CL',0,None,None,0)


#Especificacion de las Funciones de Utilidad

B_TIME_ALT1 = (B_TIME_ALT1_REF * exp(B_TIME_ALT1_CL* ACTAGR))
#B_DIST_ALT1 = B_DIST_ALT1_REF * exp(B_DIST_ALT1_CL * ACTAGR)

V1 = ASC_ALT1+\
    B_TIME_ALT1*TIME1_SC+\
    B_DIST*DIST1_SC
    
B_TIME_ALT2 = B_TIME_ALT2_REF * exp(B_TIME_ALT2_CL* ACTAGR)
#B_DIST_ALT2 = B_DIST_ALT2_REF * exp(B_DIST_ALT2_CL * ACTAGR)

V2 = ASC_ALT2+B_TIME_ALT2*TIME2_SC+B_DIST*DIST2_SC

B_TIME_ALT3 = B_TIME_ALT3_REF * exp(B_TIME_ALT3_CL* ACTAGR)
#B_DIST_ALT3 = B_DIST_ALT3_REF * exp(B_DIST_ALT3_CL * ACTAGR)

V3 = ASC_ALT3+B_TIME_ALT3 * TIME3_SC + B_DIST*DIST3_SC

B_TIME_EC = B_TIME_EC_REF * exp(B_TIME_EC_CL * ACTAGR)
#B_DIST_EC = B_DIST_EC_REF * exp(B_DIST_EC_CL * ACTAGR)


V4 = ASC_EC+B_TIME_EC*TIME4_SC+B_DIST*DIST4_SC+\
    COEF_JOVEN30*JOVEN30 +\
    COEF_ADULTO40*ADULTO40+\
    COEF_ADULTO60*ADULTO60+\
    COEF_TPROF*TPROF+\
    COEF_HORASTRAB*HORASTRAB+\
    COEF_HORAPICO*HORAPICO+\
    COEF_CLIMASECO*CLIMASECO+\
    COEF_CONGCD*CONGCD+\
    COEF_CONGEF*CONGEF+\
    COEF_SININFO*SININFO


# Asociacion de las funciones de utilidad con el numero de alternativas
V = {1:V1, 2:V2, 3:V3, 4:V4}

#ASOCIAR LAS CONDICIONES DE DISPONIBILIDAD CON LAS ALTERNATIVAS PARA QUE SIEMPRE ESTEN DISPONIBLES
AV = {1:1, 2:1, 3:1, 4:1}

# EL MODELO DE ELECCIÓN ES UN LOGIT, CONDICIONADO AL VALOR DE LA VARIABLE LATENTE 

condprob = models.logit(V,AV,CHOICE)

#prob = Integrate(condprob * density,'omega')

loglike = log(MonteCarlo(condprob)+0.5)

biogeme  = bio.BIOGEME(database,loglike, numberOfDraws=10000)
biogeme.DRAWS = {'omega_LV1': ('NORMAL')}

biogeme.modelName = "04LVMedellinSeq"
results = biogeme.estimate()
# Get the results in a pandas table
pandasResults = results.getEstimatedParameters()
print(pandasResults)
print(f"Nbr of observations: {database.getNumberOfObservations()}")
print(f"LL(0) =    {results.data.initLogLike:.3f}")
print(f"LL(beta) = {results.data.logLike:.3f}")
print(f"rho bar square = {results.data.rhoBarSquare:.3g}")
print(f"Output file: {results.data.htmlFileName}")




