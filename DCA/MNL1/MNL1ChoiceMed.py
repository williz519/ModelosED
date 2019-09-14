#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat May 18 15:52:01 2019

@author: williz
"""
# Modelo de Elección MNL1
# Modelo básico con funciones de utilidad definidas solo en terminos del tiempo y distancias


# Cargar Paquetes

import pandas as pd
import numpy as np
import biogeme.database as db
import biogeme.biogeme as bio
from biogeme.models import piecewise
import biogeme.loglikelihood as ll


#Cargar DataBase

#Cargar DataBase

pandas = pd.read_csv('/Users/williz/Desktop/ModelosED/Database/DBModLogitMuestraVLCE.csv', sep='\t')

# normalizar variables continuas para evitar zero log-likelihood 
pandas["TIEMPO_PROFESION"]=(pandas["TIEMPO_PROFESION"]-pandas["TIEMPO_PROFESION"].min())/(pandas["TIEMPO_PROFESION"].max()-pandas["TIEMPO_PROFESION"].min())
pandas["HORASTRABAJO"]=(pandas["HORASTRABAJO"]-pandas["HORASTRABAJO"].min())/(pandas["HORASTRABAJO"].max()-pandas["HORASTRABAJO"].min())
pandas["DISTAlt1"]=(pandas["DISTAlt1"]-pandas["DISTAlt1"].min())/(pandas["DISTAlt1"].max()-pandas["DISTAlt1"].min())
pandas["DISTAlt2"]=(pandas["DISTAlt2"]-pandas["DISTAlt2"].min())/(pandas["DISTAlt2"].max()-pandas["DISTAlt2"].min())
pandas["DISTAlt3"]=(pandas["DISTAlt3"]-pandas["DISTAlt3"].min())/(pandas["DISTAlt3"].max()-pandas["DISTAlt3"].min())
pandas["DISTEC"]=(pandas["DISTEC"]-pandas["DISTEC"].min())/(pandas["DISTEC"].max()-pandas["DISTEC"].min())
pandas["TIEMPOAlt1"]=(pandas["TIEMPOAlt1"]-pandas["TIEMPOAlt1"].min())/(pandas["TIEMPOAlt1"].max()-pandas["TIEMPOAlt1"].min())
pandas["TIEMPOAlt2"]=(pandas["TIEMPOAlt2"]-pandas["TIEMPOAlt2"].min())/(pandas["TIEMPOAlt2"].max()-pandas["TIEMPOAlt2"].min())
pandas["TIEMPOAlt3"]=(pandas["TIEMPOAlt3"]-pandas["TIEMPOAlt3"].min())/(pandas["TIEMPOAlt3"].max()-pandas["TIEMPOAlt3"].min())
pandas["TIEMPOEC"]=(pandas["TIEMPOEC"]-pandas["TIEMPOEC"].min())/(pandas["TIEMPOEC"].max()-pandas["TIEMPOEC"].min())


database = db.Database("DBModLogitMuestraVLCE",pandas)

from headers import *

#exclude = (CHOICE == -1.0)
#database.remove(exclude)

# Definicion de Variables

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
USODISPMOB = DefineVariable('USODISPMOB',(DispMob==2)+(DispMob==3)+(DispMob==4)+(DispMob==5)+(DispMob==6),database)


# Coeficientes

ASC_ALT1 = Beta('ASC_ALT1',0,None,None,0)
ASC_ALT2 = Beta('ASC_ALT2',0,None,None,0)
ASC_ALT3 = Beta('ASC_ALT3',0,None,None,0)
ASC_EC = Beta('ASC_ALT4',0,None,None,1)
B_TIME = Beta('B_TIME',0,None,None,0)
B_DIST = Beta('B_DIST',0,None,None,0)
COEF_EDUC_BASICA = Beta('COEF_EDUC_BASICA',0,None,None,0)
COEF_JOVEN30 = Beta('COEF_JOVEN30',0,None,None,0)
COEF_ADULTO40 = Beta('COEF_ADULTO40',0	,None,None,0)
COEF_ADULTO60 = Beta('COEF_ADULTO60',0,None,None,0)
COEF_ADULTOMAYOR = Beta('COEF_ADULTOMAYOR',0,None,None,0)
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
COEF_USODISPMOB = Beta('COEF_USODISPMOB',0,None,None,0)


#Especificacion de las Funciones de Utilidad

V1 = ASC_ALT1+B_TIME*TIME1_SC+B_DIST*DIST1_SC

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

biogeme.modelName = "MNL1ChoiceMed"
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



