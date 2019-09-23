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
USODISPMOB = DefineVariable('USODISPMOB',(DispMob==2)+(DispMob==3)+(DispMob==4)+(DispMob==5)+(DispMob==6),database)
CONDAGREF1 = DefineVariable('CONDAGRESF1', CondAgres, database)
AMBLABORF2 = DefineVariable('AMBLABORF2', AmbienteLaboral, database)
HABPROSOCF3 = DefineVariable('HABPROSOCF3', HabProsoc, database)
STRESSF4 = DefineVariable('STRESSF4', Stress, database)


# Coeficientes
# Read the estimates from the structural equation estimation
structResults = res.bioResults(pickleFile='/Users/williz/Desktop/ModelosED/DCA/4VL/02LV4MedellinOrdered.pickle')
structBetas = structResults.getBetaValues()


# Coeficientes
COEF_INTER = Beta('COEF_INTER',structBetas['COEF_INTER'],None,None,1)
COEF_EDUC_BASICA = Beta('COEF_EDUC_BASICA',structBetas['COEF_EDUC_BASICA'],None,None,1)
COEF_JOVEN30 = Beta('COEF_JOVEN30',structBetas['COEF_JOVEN30'],None,None,1)
COEF_ADULTO40 = Beta('COEF_ADULTO40',structBetas['COEF_ADULTO40'],None,None,1)
COEF_ADULTO60 = Beta('COEF_ADULTO60',structBetas['COEF_ADULTO60'],None,None,1)
COEF_ADULTOMAYOR = Beta('COEF_ADULTOMAYOR',0,None,None,0)
COEF_EDUC_SUPERIOR = Beta('COEF_EDUC_SUPERIOR',0,None,None,1)
COEF_TPROF = Beta('COEF_TPROF',0,None,None,1)
COEF_HORASTRAB = Beta('COEF_HORASTRAB',structBetas['COEF_HORASTRAB'],None,None,1)
COEF_HORAPICO = Beta('COEF_HORAPICO',structBetas['COEF_HORAPICO'],None,None,1)
COEF_HVALLE = Beta('COEF_HVALLE',0,None,None,0)
COEF_CLIMASECO = Beta('COEF_CLIMASECO',structBetas['COEF_CLIMASECO'],None,None,1)
COEF_CLIMALLUVIA = Beta('COEF_CLIMALLUVIA',0,None,None,0)
COEF_CONGAB = Beta('COEF_CONGAB',0,None,None,0)
COEF_CONGCD = Beta('COEF_CONGCD',structBetas['COEF_CONGCD'],None,None,1)
COEF_CONGEF = Beta('COEF_CONGEF',structBetas['COEF_CONGEF'],None,None,1)
COEF_SININFO = Beta('COEF_SININFO',structBetas['COEF_SININFO'],None,None,1)
COEF_CONINFO = Beta('COEF_CONINFO',0,None,None,0)
COEF_USOCINTURON = Beta('COEF_USOCINTURON',structBetas['COEF_USOCINTURON'],None,None,1)
COEF_USODISPMOB = Beta('COEF_USODISPMOB',structBetas['COEF_USODISPMOB'],None,None,1)
COEF_CONDAGREF1 = Beta('COEF_CONDAGREF1',0,None,None,0)
COEF_AMBLABORF2 = Beta('COEF_AMBLABORF2',structBetas['COEF_AMBLABORF2'],None,None,1)
COEF_HABPROSOCF3 = Beta('COEF_HABPROSOCF3',structBetas['COEF_HABPROSOCF3'],None,None,1)
COEF_STRESSF4 = Beta('COEF_STRESSF4',structBetas['COEF_STRESSF4'],None,None,1)



# Ecuacion Estructural Variable Latente

omega_LV1 = bioDraws('omega_LV1','NORMAL')
Beta_sigma_LV1 = Beta('Beta_sigma_LV1',0,None,None,0)

omega_LV2 = bioDraws('omega_LV2','NORMAL')
Beta_sigma_LV2 = Beta('Beta_sigma_LV2',0,None,None,0)

omega_LV3 = bioDraws('omega_LV3','NORMAL')
Beta_sigma_LV3 = Beta('Beta_sigma_LV3',0,None,None,0)

omega_LV4 = bioDraws('omega_LV4','NORMAL')
Beta_sigma_LV4 = Beta('Beta_sigma_LV4',0,None,None,0)


ACTAGR = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA+COEF_JOVEN30*JOVEN30 +\
    COEF_ADULTO40*ADULTO40+COEF_HORASTRAB*HORASTRAB+COEF_CLIMASECO*CLIMASECO+\
    COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF+COEF_USOCINTURON*USOCINTURON +\
    COEF_STRESSF4*STRESSF4 + Beta_sigma_LV1 * omega_LV1

AMBLABOR = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA + COEF_ADULTO40*ADULTO40+\
    COEF_ADULTO60*ADULTO60+COEF_HORASTRAB*HORASTRAB+COEF_HORAPICO*HORAPICO+\
    COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF + COEF_USOCINTURON*USOCINTURON +\
    COEF_USODISPMOB*USODISPMOB + Beta_sigma_LV2 * omega_LV2

HABPROSOC = COEF_INTER + COEF_ADULTO40*ADULTO40 + COEF_ADULTO60*ADULTO60+\
    COEF_HORASTRAB*HORASTRAB + COEF_CLIMASECO*CLIMASECO + COEF_CONGEF*CONGEF+\
    COEF_USOCINTURON*USOCINTURON + COEF_AMBLABORF2*AMBLABORF2 + Beta_sigma_LV3 * omega_LV3

STRESS = COEF_INTER + COEF_JOVEN30*JOVEN30 + COEF_ADULTO40*ADULTO40 + COEF_ADULTO60*ADULTO60+\
    COEF_HORASTRAB*HORASTRAB + COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF + COEF_SININFO*SININFO+\
    COEF_USODISPMOB*USODISPMOB + COEF_HABPROSOCF3*HABPROSOCF3 + Beta_sigma_LV4 * omega_LV4

#Coeficientes Variables Latentes
B_ACTAGRE = Beta('B_ACTAGRE',0,None,None,0)
B_AMBLABOR = Beta('B_AMBLABOR',0,None,None,0)
B_HABPROSOC = Beta('B_HABPROSOC',0,None,None,0)
B_STRESS = Beta('B_STRESS',0,None,None,0)


# Valores de la estimacion MNL1
ASC_ALT1 = Beta('ASC_ALT1',-0.734,None,None,0)
ASC_ALT2 = Beta('ASC_ALT2',-1.68,None,None,0)
ASC_ALT3 = Beta('ASC_ALT3',-2.27,None,None,0)
ASC_EC = Beta('ASC_ALT4',1,None,None,1)
B_TIME = Beta('B_TIME',4.1,None,None,0)
B_DIST = Beta('B_DIST',-3.19,None,None,0)


#Especificacion de las Funciones de Utilidad

V1 = ASC_ALT1*1+\
    B_TIME*TIME1_SC+\
    B_DIST*DIST1_SC
    
V2 = ASC_ALT2+\
    B_TIME*TIME2_SC+\
    B_DIST*DIST2_SC

V3 = ASC_ALT3+\
    B_TIME * TIME3_SC +\
    B_DIST*DIST3_SC 


V4 = ASC_EC+B_TIME*TIME4_SC+\
    B_DIST*DIST4_SC+\
    B_ACTAGRE*ACTAGR+\
    B_AMBLABOR*AMBLABOR+\
    B_HABPROSOC*HABPROSOC+\
    B_STRESS*STRESS


# Asociacion de las funciones de utilidad con el numero de alternativas
V = {1:V1, 2:V2, 3:V3, 4:V4}

#ASOCIAR LAS CONDICIONES DE DISPONIBILIDAD CON LAS ALTERNATIVAS PARA QUE SIEMPRE ESTEN DISPONIBLES
AV = {1:1, 2:1, 3:1, 4:1}

# EL MODELO DE ELECCIÓN ES UN LOGIT, CONDICIONADO AL VALOR DE LA VARIABLE LATENTE 

condprob = models.logit(V,AV,CHOICE)

#prob = Integrate(condprob * density,'omega')

loglike = log(MonteCarlo(condprob))

biogeme  = bio.BIOGEME(database,loglike, numberOfDraws=100)
biogeme.DRAWS = {'omega_LV1': ('NORMAL'), 'omega_LV2': ('NORMAL'), 'omega_LV3': ('NORMAL'), 'omega_LV4': ('NORMAL')}

biogeme.modelName = "04LV4REGChoiceSeqMedV2"
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
