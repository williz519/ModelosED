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

pandas = pd.read_csv('/Users/williz/Dropbox/Doctorado/Resultados Tesis/BasesDatos/DBModeloLogitVL1.csv', sep='\t')
database = db.Database("DBModeloLogitVL1.csv",pandas)

from headers import *

#exclude = (CHOICE == -1.0)
#database.remove(exclude)

# Definicion de Variables

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
CLIMASECO = DefineVariable('CSECO', CLIMA == 1, database)
NCONG_A = DefineVariable('NCONG_A', CONGESTION == 1, database)
NCONG_B = DefineVariable('NCONG_B', CONGESTION == 2, database)
NCONG_C = DefineVariable('NCONG_C', CONGESTION == 3, database)
NCONG_D = DefineVariable('NCONG_D', CONGESTION == 4, database)
NCONG_E = DefineVariable('NCONG_E', CONGESTION == 5, database)
NCONG_F = DefineVariable('NCONG_F', CONGESTION == 6, database)
INCID = DefineVariable('NINCID', INCIDENTE == 1, database)
AMPM = DefineVariable('AMPM', MERIDIANO == 1, database)
PICOVALLE = DefineVariable('PICOVALLE',HPICOHVALLE ==1, database)
TPROF = DefineVariable('TPROF', TIEMPO_PROFESION, database)
HORASTRAB = DefineVariable('HORASTRAB', HORASTRABAJO, database)
EDU_BASICA = DefineVariable('EDU_BASICA', EDUBASICA, database)
INFOTRAF = DefineVariable('INFOTRAF',INFOTRAFICO == 1, database)
MENOR60 = DefineVariable('MENOR60', MENOR_60, database)

# Coeficientes

ASC_ALT1 = Beta('ASC_ALT1',0,None,None,0)
ASC_ALT2 = Beta('ASC_ALT2',0,None,None,0)
ASC_ALT3 = Beta('ASC_ALT3',0,None,None,0)
ASC_EC = Beta('ASC_ALT4',0,None,None,1)
B_TIME = Beta('B_TIME',0,None,None,0)
B_DIST = Beta('B_DIST',0,None,None,0)
#B_VEL = Beta('B_VEL',0,None,None,0)
COEF_CLIMA = Beta('COEF_CLIMA',0,None,None,0)
COEF_CONGNA = Beta('COEF_CONGNA',0,None,None,0)
COEF_CONGNB = Beta('COEF_CONGNB',0,None,None,0)
COEF_CONGNC = Beta('COEF_CONGNC',0,None,None,0)
COEF_CONGND = Beta('COEF_CONGND',0,None,None,0)
COEF_CONGNE = Beta('COEF_CONGNE',0,None,None,0)
COEF_CONGNF = Beta('COEF_CONGNF',0,None,None,0)
COEF_INCID = Beta('COEF_INCID',0,None, None, 0)
COEF_AMPM = Beta('COEF_AMPM',0,None, None, 0)
COEF_PICOVALLE = Beta('COEF_PICOVALLE',0,None, None, 0)
COEF_TPROF = Beta('COEF_TPROF',0,None,None,0)
COEF_HORASTRAB = Beta('COEF_HORASTRAB',0,None,None,0)
COEF_EDUBASICA = Beta('COEF_EDUBASICA',0,None,None,0)
COEF_INFOTRAF = Beta('COEF_INFOTRAF',0,None,None,0)
COEF_MENOR60 = Beta('MENOR_60',0,None,None,0)


#Especificacion de las Funciones de Utilidad

V1 = ASC_ALT1+B_TIME*TIME1_SC+B_DIST*DIST1_SC+COEF_CLIMA*CLIMASECO+COEF_INCID*INCID+\
    COEF_AMPM*AMPM+COEF_PICOVALLE*PICOVALLE+COEF_TPROF*TPROF+COEF_HORASTRAB*HORASTRAB+COEF_INFOTRAF*INFOTRAF+\
    COEF_MENOR60*MENOR60 + COEF_EDUBASICA*EDU_BASICA

V2 = ASC_ALT2+B_TIME*TIME2_SC+B_DIST*DIST2_SC

V3 = ASC_ALT3+B_TIME*TIME3_SC+B_DIST*DIST3_SC

V4 = ASC_EC+B_TIME*TIME4_SC+B_DIST*DIST4_SC

# Asociacion de las funciones de utilidad con el numero de alternativas
V = {1:V1, 2:V2, 3:V3, 4:V4}

#ASOCIAR LAS CONDICIONES DE DISPONIBILIDAD CON LAS ALTERNATIVAS PARA QUE SIEMPRE ESTEN DISPONIBLES
AV = {1:1, 2:1, 3:1, 4:1}

#Contribucion a la función de verosimilitud como el logaritmo de un modelo logit
logprob = bioLogLogit(V,AV,CHOICE)

#BIOGEME
biogeme = bio.BIOGEME(database, logprob)

biogeme.modelName = "ChoiceModel"
results = biogeme.estimate()
# Get the results in a pandas table
pandasResults = results.getEstimatedParameters()
print(pandasResults)
print(f"Nbr of observations: {database.getNumberOfObservations()}")
print(f"LL(0) =    {results.data.initLogLike:.3f}")
print(f"LL(beta) = {results.data.logLike:.3f}")
print(f"rho bar square = {results.data.rhoBarSquare:.3g}")
print(f"Output file: {results.data.htmlFileName}")




