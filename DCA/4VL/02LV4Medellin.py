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
CONDAGREF1 = DefineVariable('CONDAGRESF1', CondAgres, database)
AMBLABORF2 = DefineVariable('AMBLABORF2', AmbienteLaboral, database)
HABPROSOCF3 = DefineVariable('HABPROSOCF3', HabProsoc, database)
STRESSF4 = DefineVariable('STRESSF4', Stress, database)



## Coeficientes
COEF_INTER = Beta('COEF_INTER',-6.41,None,None,0)
COEF_EDUC_BASICA = Beta('COEF_EDUC_BASICA',1.57,None,None,0)
COEF_JOVEN30 = Beta('COEF_JOVEN30',0.606,None,None,0)
COEF_ADULTO40 = Beta('COEF_ADULTO40',-0.654	,None,None,0)
COEF_ADULTO60 = Beta('COEF_ADULTO60',-0.836,None,None,0)
COEF_ADULTOMAYOR = Beta('COEF_ADULTOMAYOR',0,None,None,0)
COEF_EDUC_SUPERIOR = Beta('COEF_EDUC_SUPERIOR',0,None,None,0)
COEF_TPROF = Beta('COEF_TPROF',-0.0249,None,None,0)
COEF_HORASTRAB = Beta('COEF_HORASTRAB',-6.43,None,None,0)
COEF_HORAPICO = Beta('COEF_HORAPICO',1.2,None,None,0)
COEF_HVALLE = Beta('COEF_HVALLE',0,None,None,0)
COEF_CLIMASECO = Beta('COEF_CLIMASECO',0.831,None,None,0)
COEF_CLIMALLUVIA = Beta('COEF_CLIMALLUVIA',0,None,None,0)
COEF_CONGAB = Beta('COEF_CONGAB',0,None,None,0)
COEF_CONGCD = Beta('COEF_CONGCD',1.83,None,None,0)
COEF_CONGEF = Beta('COEF_CONGEF',2.61,None,None,0)
COEF_SININFO = Beta('COEF_SININFO',-0.632,None,None,0)
COEF_CONINFO = Beta('COEF_CONINFO',0,None,None,0)
COEF_USOCINTRURON = Beta('COEF_USOCINTURON',0,None,None,0)
COEF_CONDAGREF1 = Beta('COEF_CONDAGREF1',0,None,None,0)
COEF_AMBLABORF2 = Beta('COEF_AMBLABORF2',0,None,None,0)
COEF_HABPROSOCF3 = Beta('COEF_HABPROSOCF3',-17.9,None,None,0)
COEF_STRESSF4 = Beta('COEF_STRESSF4',0,None,None,0)

# Ecuacion Estructural Variable Latente

ACTAGR = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA+COEF_JOVEN30*JOVEN30 +\
    COEF_ADULTO40*ADULTO40+COEF_ADULTO60*ADULTO60+COEF_TPROF*TPROF+\
    COEF_HORASTRAB*HORASTRAB+COEF_HORAPICO*HORAPICO+COEF_CLIMASECO*CLIMASECO+\
    COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF+COEF_SININFO*SININFO+COEF_HABPROSOCF3*HABPROSOCF3

AMBLABOR = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA+COEF_JOVEN30*JOVEN30 +\
    COEF_ADULTO40*ADULTO40+COEF_ADULTO60*ADULTO60+COEF_TPROF*TPROF+\
    COEF_HORASTRAB*HORASTRAB+COEF_HORAPICO*HORAPICO+COEF_CLIMASECO*CLIMASECO+\
    COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF+COEF_SININFO*SININFO+COEF_HABPROSOCF3*HABPROSOCF3

HABPROSOC = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA+COEF_JOVEN30*JOVEN30 +\
    COEF_ADULTO40*ADULTO40+COEF_ADULTO60*ADULTO60+COEF_TPROF*TPROF+\
    COEF_HORASTRAB*HORASTRAB+COEF_HORAPICO*HORAPICO+COEF_CLIMASECO*CLIMASECO+\
    COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF+COEF_SININFO*SININFO

STRESS = COEF_INTER + COEF_EDUC_BASICA*EDUC_BASICA+COEF_JOVEN30*JOVEN30 +\
    COEF_ADULTO40*ADULTO40+COEF_ADULTO60*ADULTO60+COEF_TPROF*TPROF+\
    COEF_HORASTRAB*HORASTRAB+COEF_HORAPICO*HORAPICO+COEF_CLIMASECO*CLIMASECO+\
    COEF_CONGCD*CONGCD + COEF_CONGEF*CONGEF+COEF_SININFO*SININFO+COEF_HABPROSOCF3*HABPROSOCF3


### ECUACIONES DE MEDIDA

INTER_FRbr= Beta('INTER_FRbr',0,None,None,1)
INTER_EnfCond = Beta('INTER_EnfCond',-3.12,None,None,1)
INTER_AFrSem = Beta('INTER_AFrSem',-0.414,None,None,1)
INTER_CulFr = Beta('INTER_CulFr',3.53,None,None,1)
INTER_OmLmVel = Beta('INTER_OmLmVel',-3.09,None,None,1)
INTER_IgPare = Beta('INTER_IgPare',-3.63,None,None,1)
INTER_UsoCel = Beta('INTER_UsoCel',-6.36,None,None,1)


BETA_FRbrF1 = Beta('BETA_FRbrF1',0.573,None,None,1)
BETA_EnfCondF1 = Beta('BETA_EnfCondF1',0.493,None,None,1)
BETA_AFrSemF1 = Beta('BETA_AFrSemF1',0.589,None,None,1)
BETA_CulFrF1 = Beta('BETA_CulFrF1',0.406,None,None,1)
BETA_OmLmVelF1 = Beta('BETA_OmLmVelF1',0.474,None,None,1)
BETA_IgPareF1 = Beta('BETA_IgPareF1', 0.501,None,None,1)
BETA_UsoCelF1 = Beta('BETA_UsoCelF1',0.333,None,None,1)


MODEL_FRbr = INTER_FRbr + BETA_FRbrF1 * ACTAGR
MODEL_EnfCond = INTER_EnfCond + BETA_EnfCondF1 * ACTAGR
MODEL_AFrSem = INTER_AFrSem + BETA_AFrSemF1 * ACTAGR
MODEL_CulFr = INTER_CulFr + BETA_CulFrF1 * ACTAGR
MODEL_OmLmVel = INTER_OmLmVel + BETA_OmLmVelF1 * ACTAGR
MODEL_IgPare = INTER_IgPare + BETA_IgPareF1 * ACTAGR
MODEL_UsoCel = INTER_UsoCel + BETA_UsoCelF1 * ACTAGR


SIGMA_STAR_FRbr = Beta('SIGMA_STAR_FRbr', 10,None,None,1)
SIGMA_STAR_EnfCond = Beta('SIGMA_STAR_EnfCond', 7.2,None,None,1)
SIGMA_STAR_AFrSem = Beta('SIGMA_STAR_AFrSem',8.91,None,None,1)
SIGMA_STAR_CulFr = Beta('SIGMA_STAR_CulFr', 10.1,None,None,1)
SIGMA_STAR_OmLmVel = Beta('SIGMA_STAR_OmLmVel', 8.18,None,None,1)
SIGMA_STAR_IgPare = Beta('SIGMA_STAR_IgPare', 7.05,None,None,1)
SIGMA_STAR_UsoCel = Beta('SIGMA_STAR_UsoCel', 9.59,None,None,1)


### ECUACIONES DE MEDIDA FACTOR AMBIENTE LABORAL

INTER_PrPer= Beta('INTER_PrPer',0,None,None,1)
INTER_AmbTr= Beta('INTER_AmbTr',-9.8,None,None,1)

BETA_PrPerF2 = Beta('BETA_PrPerF2',0.558,None,None,1)
BETA_AmbTrF2 = Beta('BETA_AmbTFr2',0.485,None,None,1)

MODEL_PrPer = INTER_PrPer + BETA_PrPerF2 * AMBLABOR
MODEL_AmbTr = INTER_AmbTr + BETA_AmbTrF2 * AMBLABOR

SIGMA_STAR_PrPer = Beta('SIGMA_STAR_PrPer',10,None,None,1)
SIGMA_STAR_AmbTr = Beta('SIGMA_STAR_AmbTr',12.2,None,None,1)


### ECUACIONES DE MEDIDA FACTOR HABILIDADES PROSOCIALES

INTER_ComVrb = Beta('INTER_ComVrb',0,None,None,1)
INTER_ComAfec = Beta('INTER_ComAfec',-25,None,None,1)
INTER_ConCl = Beta('INTER_ConCl',-25.7,None,None,1)
INTER_AnsF3 = Beta('INTER_AnsF3',-17.8,None,None,1)

BETA_ComVrbF3 = Beta('BETA_ComVrbF3',0.596,None,None,1)
BETA_ComAfecF3 = Beta('BETA_ComAfecF3',-0.673,None,None,1)
BETA_ConClF3 = Beta('BETA_ConClF3',-0.328,None,None,1)
BETA_AnsF3 = Beta('BETA_AnsF3',-0.362,None,None,1)

MODEL_ComVrb = INTER_ComVrb + BETA_ComVrbF3 * HABPROSOC
MODEL_ComAfec = INTER_ComAfec + BETA_ComAfecF3 * HABPROSOC
MODEL_ConCl = INTER_ConCl + BETA_ConClF3 * HABPROSOC
MODEL_AnsF3 = INTER_AnsF3 + BETA_AnsF3 * HABPROSOC

SIGMA_STAR_ComVrb = Beta('SIGMA_STAR_ComVrb',10,None,None,1)
SIGMA_STAR_ComAfec = Beta('SIGMA_STAR_ComAfec',13.4,None,None,1)
SIGMA_STAR_ConCl = Beta('SIGMA_STAR_ConCl',10.6,None,None,1)
SIGMA_STAR_AnsF3 = Beta('SIGMA_STAR_AnsF3',9.64,None,None,1)


### ECUACIONES DE MEDIDA FACTOR STRESS

INTER_Ans= Beta('INTER_Ans',1,None,None,1)
INTER_StrC= Beta('INTER_StrC',0,None,None,0)
INTER_UsoDirec= Beta('INTER_UsoDirec',0,None,None,0)

BETA_AnsF4 = Beta('BETA_AnsF4',0.204,None,None,1)
BETA_StrCF4 = Beta('BETA_StrCF4',0.570,None,None,1)
BETA_UsoDirecF4 = Beta('BETA_UsoDirecF4',-0.291,None,None,1)

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

UsoCel_tau_1 = (tau_1-MODEL_UsoCel) / SIGMA_STAR_UsoCel
UsoCel_tau_2 = (tau_2-MODEL_UsoCel) / SIGMA_STAR_UsoCel
UsoCel_tau_3 = (tau_3-MODEL_UsoCel) / SIGMA_STAR_UsoCel
UsoCel_tau_4 = (tau_4-MODEL_UsoCel) / SIGMA_STAR_UsoCel
IndUsoCel = {
    1: bioNormalCdf(UsoCel_tau_1),
    2: bioNormalCdf(UsoCel_tau_2)-bioNormalCdf(UsoCel_tau_1),
    3: bioNormalCdf(UsoCel_tau_3)-bioNormalCdf(UsoCel_tau_2),
    4: bioNormalCdf(UsoCel_tau_4)-bioNormalCdf(UsoCel_tau_3),
    5: 1-bioNormalCdf(UsoCel_tau_4),
    6: 1.0,
    -1: 1.0,
    -2: 1.0
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