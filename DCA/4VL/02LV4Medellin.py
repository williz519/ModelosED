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
structResults = res.bioResults(pickleFile='/Users/williz/Desktop/ModelosED/DCA/3VL/02LV3MedellinOrdered.pickle')
structBetas = structResults.getBetaValues()


# Coeficientes
COEF_INTER = Beta('COEF_INTER',structBetas['COEF_INTER'],None,None,0)
COEF_EDUC_BASICA = Beta('COEF_EDUC_BASICA',structBetas['COEF_EDUC_BASICA'],None,None,0)
COEF_JOVEN30 = Beta('COEF_JOVEN30',structBetas['COEF_JOVEN30'],None,None,0)
COEF_ADULTO40 = Beta('COEF_ADULTO40',structBetas['COEF_ADULTO40'],None,None,0)
COEF_ADULTO60 = Beta('COEF_ADULTO60',structBetas['COEF_ADULTO60'],None,None,0)
COEF_ADULTOMAYOR = Beta('COEF_ADULTOMAYOR',0,None,None,0)
COEF_EDUC_SUPERIOR = Beta('COEF_EDUC_SUPERIOR',0,None,None,0)
COEF_TPROF = Beta('COEF_TPROF',structBetas['COEF_TPROF'],None,None,0)
COEF_HORASTRAB = Beta('COEF_HORASTRAB',structBetas['COEF_HORASTRAB'],None,None,0)
COEF_HORAPICO = Beta('COEF_HORAPICO',structBetas['COEF_HORAPICO'],None,None,0)
COEF_HVALLE = Beta('COEF_HVALLE',0,None,None,0)
COEF_CLIMASECO = Beta('COEF_CLIMASECO',structBetas['COEF_CLIMASECO'],None,None,0)
COEF_CLIMALLUVIA = Beta('COEF_CLIMALLUVIA',0,None,None,0)
COEF_CONGAB = Beta('COEF_CONGAB',0,None,None,0)
COEF_CONGCD = Beta('COEF_CONGCD',structBetas['COEF_CONGCD'],None,None,0)
COEF_CONGEF = Beta('COEF_CONGEF',structBetas['COEF_CONGEF'],None,None,0)
COEF_SININFO = Beta('COEF_SININFO',structBetas['COEF_SININFO'],None,None,0)
COEF_CONINFO = Beta('COEF_CONINFO',0,None,None,0)
COEF_USOCINTURON = Beta('COEF_USOCINTURON',structBetas['COEF_USOCINTURON'],None,None,0)
COEF_USODISPMOB = Beta('COEF_USODISPMOB',structBetas['COEF_USODISPMOB'],None,None,0)
COEF_CONDAGREF1 = Beta('COEF_CONDAGREF1',0,None,None,0)
COEF_AMBLABORF2 = Beta('COEF_AMBLABORF2',0,None,None,0)
COEF_HABPROSOCF3 = Beta('COEF_HABPROSOCF3',0,None,None,0)
COEF_STRESSF4 = Beta('COEF_STRESSF4',structBetas['COEF_STRESSF4'],None,None,0)

# Ecuacion Estructural Variable Latente

ACTAGR = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA+COEF_JOVEN30*JOVEN30 +\
    COEF_ADULTO40*ADULTO40+COEF_ADULTO60*ADULTO60+COEF_TPROF*TPROF+\
    COEF_HORASTRAB*HORASTRAB+COEF_HORAPICO*HORAPICO+COEF_CLIMASECO*CLIMASECO+\
    COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF+COEF_SININFO*SININFO+\
    COEF_USOCINTURON*USOCINTURON +COEF_USODISPMOB*USODISPMOB + COEF_STRESSF4*STRESSF4

AMBLABOR = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA+COEF_JOVEN30*JOVEN30 +\
    COEF_ADULTO40*ADULTO40+COEF_ADULTO60*ADULTO60+COEF_TPROF*TPROF+\
    COEF_HORASTRAB*HORASTRAB+COEF_HORAPICO*HORAPICO+COEF_CLIMASECO*CLIMASECO+\
    COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF+COEF_SININFO*SININFO+\
    COEF_USOCINTURON*USOCINTURON +COEF_USODISPMOB*USODISPMOB

HABPROSOC = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA+COEF_JOVEN30*JOVEN30 +\
    COEF_ADULTO40*ADULTO40+COEF_ADULTO60*ADULTO60+COEF_TPROF*TPROF+\
    COEF_HORASTRAB*HORASTRAB+COEF_HORAPICO*HORAPICO+COEF_CLIMASECO*CLIMASECO+\
    COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF+COEF_SININFO*SININFO+\
    COEF_USOCINTURON*USOCINTURON +COEF_USODISPMOB*USODISPMOB + COEF_AMBLABORF2*AMBLABORF2

STRESS = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA+COEF_JOVEN30*JOVEN30 +\
    COEF_ADULTO40*ADULTO40+COEF_ADULTO60*ADULTO60+COEF_TPROF*TPROF+\
    COEF_HORASTRAB*HORASTRAB+COEF_HORAPICO*HORAPICO+COEF_CLIMASECO*CLIMASECO+\
    COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF+COEF_SININFO*SININFO+\
    COEF_USOCINTURON*USOCINTURON +COEF_USODISPMOB*USODISPMOB + COEF_HABPROSOCF3*HABPROSOCF3


### ECUACIONES DE MEDIDA

INTER_FRbr= Beta('INTER_FRbr',0,None,None,1)
INTER_EnfCond = Beta('INTER_EnfCond',structBetas['INTER_EnfCond'],None,None,0)
INTER_AFrSem = Beta('INTER_AFrSem',structBetas['INTER_AFrSem'],None,None,0)
INTER_CulFr = Beta('INTER_CulFr',structBetas['INTER_CulFr'],None,None,0)
INTER_OmLmVel = Beta('INTER_OmLmVel',structBetas['INTER_OmLmVel'],None,None,0)
INTER_IgPare = Beta('INTER_IgPare',-structBetas['INTER_IgPare'],None,None,0)
INTER_UsoCel = Beta('INTER_UsoCel',structBetas['INTER_UsoCel'],None,None,0)

BETA_FRbrF1 = Beta('BETA_FRbrF1',0.574,None,None,1)
BETA_EnfCondF1 = Beta('BETA_EnfCondF1',0.492,None,None,1)
BETA_AFrSemF1 = Beta('BETA_AFrSemF1',0.589,None,None,1)
BETA_CulFrF1 = Beta('BETA_CulFrF1',0.397,None,None,1)
BETA_OmLmVelF1 = Beta('BETA_OmLmVelF1',0.472,None,None,1)
BETA_IgPareF1 = Beta('BETA_IgPareF1', 0.501,None,None,1)
BETA_UsoCelF1 = Beta('BETA_UsoCelF1',0.330,None,None,1)


MODEL_FRbr = INTER_FRbr + BETA_FRbrF1 * ACTAGR
MODEL_EnfCond = INTER_EnfCond + BETA_EnfCondF1 * ACTAGR
MODEL_AFrSem = INTER_AFrSem + BETA_AFrSemF1 * ACTAGR
MODEL_CulFr = INTER_CulFr + BETA_CulFrF1 * ACTAGR
MODEL_OmLmVel = INTER_OmLmVel + BETA_OmLmVelF1 * ACTAGR
MODEL_IgPare = INTER_IgPare + BETA_IgPareF1 * ACTAGR
MODEL_UsoCel = INTER_UsoCel + BETA_UsoCelF1 * ACTAGR


SIGMA_STAR_FRbr = Beta('SIGMA_STAR_FRbr', 10,None,None,1)
SIGMA_STAR_EnfCond = Beta('SIGMA_STAR_EnfCond', structBetas['SIGMA_STAR_EnfCond'],None,None,0)
SIGMA_STAR_AFrSem = Beta('SIGMA_STAR_AFrSem',structBetas['SIGMA_STAR_AFrSem'],None,None,0)
SIGMA_STAR_CulFr = Beta('SIGMA_STAR_CulFr', structBetas['SIGMA_STAR_CulFr'],None,None,0)
SIGMA_STAR_OmLmVel = Beta('SIGMA_STAR_OmLmVel', structBetas['SIGMA_STAR_OmLmVel'],None,None,0)
SIGMA_STAR_IgPare = Beta('SIGMA_STAR_IgPare', structBetas['SIGMA_STAR_IgPare'],None,None,0)
SIGMA_STAR_UsoCel = Beta('SIGMA_STAR_UsoCel', structBetas['SIGMA_STAR_UsoCel'],None,None,0)


### ECUACIONES DE MEDIDA FACTOR AMBIENTE LABORAL

INTER_PrPer= Beta('INTER_PrPer',0,None,None,1)
INTER_AmbTr= Beta('INTER_AmbTr',structBetas['INTER_AmbTr'],None,None,0)

BETA_PrPerF2 = Beta('BETA_PrPerF2',0.596,None,None,1)
BETA_AmbTrF2 = Beta('BETA_AmbTFr2',0.566,None,None,1)

MODEL_PrPer = INTER_PrPer + BETA_PrPerF2 * AMBLABOR
MODEL_AmbTr = INTER_AmbTr + BETA_AmbTrF2 * AMBLABOR

SIGMA_STAR_PrPer = Beta('SIGMA_STAR_PrPer',10,None,None,1)
SIGMA_STAR_AmbTr = Beta('SIGMA_STAR_AmbTr',structBetas['SIGMA_STAR_AmbTr'],None,None,0)


### ECUACIONES DE MEDIDA FACTOR HABILIDADES PROSOCIALES

INTER_ComVrb = Beta('INTER_ComVrb',0,None,None,1)
INTER_ComAfec = Beta('INTER_ComAfec',structBetas['INTER_ComAfec'],None,None,0)
INTER_ConCl = Beta('INTER_ConCl',structBetas['INTER_ConCl'],None,None,0)

BETA_ComVrbF3 = Beta('BETA_ComVrbF3',0.506,None,None,1)
BETA_ComAfecF3 = Beta('BETA_ComAfecF3',-0.591,None,None,1)
BETA_ConClF3 = Beta('BETA_ConClF3',-0.281,None,None,1)

MODEL_ComVrb = INTER_ComVrb + BETA_ComVrbF3 * HABPROSOC
MODEL_ComAfec = INTER_ComAfec + BETA_ComAfecF3 * HABPROSOC
MODEL_ConCl = INTER_ConCl + BETA_ConClF3 * HABPROSOC


SIGMA_STAR_ComVrb = Beta('SIGMA_STAR_ComVrb',10,None,None,1)
SIGMA_STAR_ComAfec = Beta('SIGMA_STAR_ComAfec',structBetas['SIGMA_STAR_ComAfec'],None,None,0)
SIGMA_STAR_ConCl = Beta('SIGMA_STAR_ConCl',structBetas['SIGMA_STAR_ConCl'],None,None,0)


### ECUACIONES DE MEDIDA FACTOR STRESS

INTER_Ans= Beta('INTER_Ans',0,None,None,1)
INTER_StrC= Beta('INTER_StrC',0,None,None,0)
INTER_UsoDirec= Beta('INTER_UsoDirec',0,None,None,0)

BETA_AnsF4 = Beta('BETA_AnsF4',0.308,None,None,1)
BETA_StrCF4 = Beta('BETA_StrCF4',0.266,None,None,1)
BETA_UsoDirecF4 = Beta('BETA_UsoDirecF4',-0.133,None,None,1)

MODEL_Ans = INTER_Ans + BETA_AnsF4 * STRESS
MODEL_StrC = INTER_StrC + BETA_StrCF4 * STRESS
MODEL_UsoDirec = INTER_UsoDirec + BETA_UsoDirecF4 * STRESS

SIGMA_STAR_Ans = Beta('SIGMA_STAR_Ans',10,None,None,1)
SIGMA_STAR_StrC = Beta('SIGMA_STAR_StrC',10,None,None,0)
SIGMA_STAR_UsoDirec = Beta('SIGMA_STAR_UsoDirec',10,None,None,0)


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


PrPer_tau_1 = (tau_1-MODEL_PrPer) / SIGMA_STAR_PrPer
PrPer_tau_2 = (tau_2-MODEL_PrPer) / SIGMA_STAR_PrPer
PrPer_tau_3 = (tau_3-MODEL_PrPer) / SIGMA_STAR_PrPer
PrPer_tau_4 = (tau_4-MODEL_PrPer) / SIGMA_STAR_PrPer
IndPrPer = {
    1: bioNormalCdf(PrPer_tau_1),
    2: bioNormalCdf(PrPer_tau_2)-bioNormalCdf(PrPer_tau_1),
    3: bioNormalCdf(PrPer_tau_3)-bioNormalCdf(PrPer_tau_2),
    4: bioNormalCdf(PrPer_tau_4)-bioNormalCdf(PrPer_tau_3),
    5: 1-bioNormalCdf(PrPer_tau_4)
}

P_PrPer = Elem(IndPrPer, PrPer)

AmbTr_tau_1 = (tau_1-MODEL_AmbTr) / SIGMA_STAR_AmbTr
AmbTr_tau_2 = (tau_2-MODEL_AmbTr) / SIGMA_STAR_AmbTr
AmbTr_tau_3 = (tau_3-MODEL_AmbTr) / SIGMA_STAR_AmbTr
AmbTr_tau_4 = (tau_4-MODEL_AmbTr) / SIGMA_STAR_AmbTr
IndAmbTr = {
    1: bioNormalCdf(AmbTr_tau_1),
    2: bioNormalCdf(AmbTr_tau_2)-bioNormalCdf(AmbTr_tau_1),
    3: bioNormalCdf(AmbTr_tau_3)-bioNormalCdf(AmbTr_tau_2),
    4: bioNormalCdf(AmbTr_tau_4)-bioNormalCdf(AmbTr_tau_3),
    5: 1-bioNormalCdf(AmbTr_tau_4)
}

P_AmbTr = Elem(IndAmbTr, AmbTr)

ComVrb_tau_1 = (tau_1-MODEL_ComVrb) / SIGMA_STAR_ComVrb
ComVrb_tau_2 = (tau_2-MODEL_ComVrb) / SIGMA_STAR_ComVrb
ComVrb_tau_3 = (tau_3-MODEL_ComVrb) / SIGMA_STAR_ComVrb
ComVrb_tau_4 = (tau_4-MODEL_ComVrb) / SIGMA_STAR_ComVrb
IndComVrb = {
    1: bioNormalCdf(ComVrb_tau_1),
    2: bioNormalCdf(ComVrb_tau_2)-bioNormalCdf(ComVrb_tau_1),
    3: bioNormalCdf(ComVrb_tau_3)-bioNormalCdf(ComVrb_tau_2),
    4: bioNormalCdf(ComVrb_tau_4)-bioNormalCdf(ComVrb_tau_3),
    5: 1-bioNormalCdf(ComVrb_tau_4)
}

P_ComVrb = Elem(IndComVrb, ComVrb)

ComAfec_tau_1 = (tau_1-MODEL_ComAfec) / SIGMA_STAR_ComAfec
ComAfec_tau_2 = (tau_2-MODEL_ComAfec) / SIGMA_STAR_ComAfec
ComAfec_tau_3 = (tau_3-MODEL_ComAfec) / SIGMA_STAR_ComAfec
ComAfec_tau_4 = (tau_4-MODEL_ComAfec) / SIGMA_STAR_ComAfec
IndComAfec = {
    1: bioNormalCdf(ComAfec_tau_1),
    2: bioNormalCdf(ComAfec_tau_2)-bioNormalCdf(ComAfec_tau_1),
    3: bioNormalCdf(ComAfec_tau_3)-bioNormalCdf(ComAfec_tau_2),
    4: bioNormalCdf(ComAfec_tau_4)-bioNormalCdf(ComAfec_tau_3),
    5: 1-bioNormalCdf(ComAfec_tau_4)
}

P_ComAfec = Elem(IndComAfec, ComAfec)

ConCl_tau_1 = (tau_1-MODEL_ConCl) / SIGMA_STAR_ConCl
ConCl_tau_2 = (tau_2-MODEL_ConCl) / SIGMA_STAR_ConCl
ConCl_tau_3 = (tau_3-MODEL_ConCl) / SIGMA_STAR_ConCl
ConCl_tau_4 = (tau_4-MODEL_ConCl) / SIGMA_STAR_ConCl
IndConCl = {
    1: bioNormalCdf(ConCl_tau_1),
    2: bioNormalCdf(ConCl_tau_2)-bioNormalCdf(ConCl_tau_1),
    3: bioNormalCdf(ConCl_tau_3)-bioNormalCdf(ConCl_tau_2),
    4: bioNormalCdf(ConCl_tau_4)-bioNormalCdf(ConCl_tau_3),
    5: 1-bioNormalCdf(ConCl_tau_4)
}

P_ConCl = Elem(IndConCl, ConCl)


Ans_tau_1 = (tau_1-MODEL_Ans) / SIGMA_STAR_Ans
Ans_tau_2 = (tau_2-MODEL_Ans) / SIGMA_STAR_Ans
Ans_tau_3 = (tau_3-MODEL_Ans) / SIGMA_STAR_Ans
Ans_tau_4 = (tau_4-MODEL_Ans) / SIGMA_STAR_Ans
IndAns = {
    1: bioNormalCdf(Ans_tau_1),
    2: bioNormalCdf(Ans_tau_2)-bioNormalCdf(Ans_tau_1),
    3: bioNormalCdf(Ans_tau_3)-bioNormalCdf(Ans_tau_2),
    4: bioNormalCdf(Ans_tau_4)-bioNormalCdf(Ans_tau_3),
    5: 1-bioNormalCdf(Ans_tau_4)
}

P_Ans = Elem(IndAns, Ans)


StrC_tau_1 = (tau_1-MODEL_StrC) / SIGMA_STAR_StrC
StrC_tau_2 = (tau_2-MODEL_StrC) / SIGMA_STAR_StrC
StrC_tau_3 = (tau_3-MODEL_StrC) / SIGMA_STAR_StrC
StrC_tau_4 = (tau_4-MODEL_StrC) / SIGMA_STAR_StrC
IndStrC = {
    1: bioNormalCdf(StrC_tau_1),
    2: bioNormalCdf(StrC_tau_2)-bioNormalCdf(StrC_tau_1),
    3: bioNormalCdf(StrC_tau_3)-bioNormalCdf(StrC_tau_2),
    4: bioNormalCdf(StrC_tau_4)-bioNormalCdf(StrC_tau_3),
    5: 1-bioNormalCdf(StrC_tau_4)
}

P_StrC = Elem(IndStrC, StrC)

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

# log-verosimilitud

loglike = log(P_FRbr*P_EnfCond*P_AFrSem*P_CulFr*P_OmLmVel*P_IgPare*P_UsoCel*P_PrPer*P_AmbTr*\
    P_ComVrb*P_ComAfec*P_ConCl*P_Ans*P_StrC*P_UsoDirec)
            

biogeme  = bio.BIOGEME(database,loglike)
biogeme.modelName = "02LV4MedellinOrdered"
results = biogeme.estimate()
print(f"Estimated betas: {len(results.data.betaValues)}")
print(f"final log likelihood: {results.data.logLike:.3f}")
print(f"Output file: {results.data.htmlFileName}")
results.writeLaTeX()
print(f"LaTeX file: {results.data.latexFileName}")