/*----------------------------------------------------------------------------
 *	
 *	Copyright (C) 2021 Isabelle Boulangeat, Damien Georges, Maya Guéguen,
 *	Wilfried Thuiller
 *	
 *	This file is part of FATE.
 *	
 *	FATE is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *	
 *	FATE is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *	GNU General Public License for more details.
 *	
 *	You should have received a copy of the GNU General Public License
 *	along with FATE. If not, see <https://www.gnu.org/licenses/>.
 *	
 --------------------------------------------------------------------------*/

/*================================================*/
/*            Plants Functionals Groups           */
/*                 Definition Class               */
/*================================================*/


#include <cmath>
#include <iostream>
#include <cstring>
#include <fstream>
#include <cstdio>
#include "FG.h"
#include "FGUtils.h"
#include "Params.h"

using namespace std;

/* Note : New version of FG constructor using Matt T. parameters handler utilities (Params.h) */


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FG::FG() : m_Name(""), m_M(0), m_L(1), m_MaxAbund(100), m_ImmSize(100), m_MaxStratum(0), m_Strata(0, 1000), /* Life history*/
m_PoolL(PTcount,0), m_InnateDorm(false), m_PotentialFecundity(100), /* Propagule biology */
m_LightShadeFactor(1.0), m_LightActiveGerm(Rcount, 100), m_LightTolerance(LScount, vector<int>(Rcount, 100)), /* Light response */
m_IsSeeded(false), m_disp50(0.0), m_disp99(0.0), m_dispLD(0.0), /* Dispersal module */
m_SoilContrib(0.0), m_SoilLow(0.0), m_SoilHigh(0.0), /* Soil response */
m_SoilActiveGerm(Rcount, 100), m_SoilTolerance(LScount, vector<int>(Rcount, 100)), /* Soil response */
m_DistResponse(FGresponse()), /* Disturbance response */
m_FireResponse(FGresponse()), m_Flamm(0.0), /* Fire response */
m_DroughtResponse(FGresponse()), m_DroughtThreshMod(0.0), m_DroughtThreshSev(0.0),
m_CountRecovery(0), m_CountSens(0), m_CountCum(0), /* Drought response */
m_IsAlien(false) /* Alien module */
{
  /* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FG::getSuccParams(const GSP& glob_params, const string& PFG_LifeHistoryFile)
{
  /* 1. check parameter file existence */
  testFileExist("--PFG_PARAMS_LIFE_HISTORY--", PFG_LifeHistoryFile, false);
  
  /* 2. read succession parameters */
  par::Params SuccParams(PFG_LifeHistoryFile.c_str(), " = \"", "#"); /* opening PFG life history traits parameters file */
  
  logg.info("\n*********************************************",
            "\n** PFG : ", SuccParams.get_val<string>("NAME")[0],
            "\n*********************************************\n",
             "\n> Succession files opened");
  
  /* 3. fill FG object according to given parameters */
  
  /* PFG Life History parameters filling =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
  m_Name = SuccParams.get_val<string>("NAME")[0];
  m_M = SuccParams.get_val<int>("MATURITY")[0];
  m_L = SuccParams.get_val<int>("LONGEVITY")[0];
  if (m_M >= m_L)
  {
    logg.error("!!! MATURITY is superior or equal to LONGEVITY. Please check!");
  }
  
  m_MaxAbund = SuccParams.get_val<int>("MAX_ABUNDANCE")[0];
  m_ImmSize = SuccParams.get_val<int>("IMM_SIZE")[0];
  if (m_ImmSize < 0 || m_ImmSize > 100)
  {
    logg.error("!!! IMM_SIZE must be superior or equal to 0, and inferior or equal to 100. Please check!");
  }
  vector<int> v_int = SuccParams.get_val<int>("MAX_STRATUM", true);
  if (v_int.size()) m_MaxStratum = v_int[0]; else m_MaxStratum = glob_params.getNoStrata();
  m_Strata = SuccParams.get_val<int>("CHANG_STR_AGES");
  m_Strata.push_back(10000); /* High value of to avoid PFGs to exit the upper stata */
  if (static_cast<int>(m_Strata.size()) != glob_params.getNoStrata() + 1)
  {
    logg.error("!!! Wrong number of parameters provided for CHANG_STR_AGES (",
               m_Strata.size() - 1," instead of ", glob_params.getNoStrata(),
               "). Please check!");
  }
  bool is_sup = false;
  int prev_age = m_Strata[0];
  for(unsigned i=1; i<m_Strata.size(); i++)
  {
    if (m_Strata[i] < prev_age)
    {
      is_sup = true;
    }
    prev_age = m_Strata[i];
    if (is_sup)
    {
      logg.error("!!! CHANG_STR_AGES must be given in ascending order. Please check!");
    }
  }
  
  v_int = SuccParams.get_val<int>("IS_ALIEN", true);
  if (v_int.size()) m_IsAlien = v_int[0]; else m_IsAlien = false;
  
  /* Propagule biology parameters filling =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
  m_PoolL = SuccParams.get_val<int>("SEED_POOL_LIFE");
  if (m_PoolL.size() != PTcount)
  {
    logg.error("!!! Wrong number of parameters provided for SEED_POOL_LIFE (",
               m_PoolL.size(), " instead of ", PTcount, "). Please check!");
  }
  m_InnateDorm = bool(SuccParams.get_val<int>("SEED_DORMANCY")[0]);
  
  /* Potential fecundity parameter filling  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
  v_int = SuccParams.get_val<int>("POTENTIAL_FECUNDITY", true);
  if (v_int.size()) m_PotentialFecundity = v_int[0]; else m_PotentialFecundity = glob_params.getPotentialFecundity();
  
  logg.info("> Life History parameters provided");
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FG::getLightParams(const string& PFG_LightFile)
{
  /* 1. check parameter file existence */
  testFileExist("--PFG_PARAMS_LIGHT--", PFG_LightFile, false);
  
  /* 2. read light parameters */
  par::Params LightParams(PFG_LightFile.c_str(), " = \"", "#");
  
  m_LightShadeFactor = LightParams.get_val<double>("SHADE_FACTOR")[0];
  if (m_LightShadeFactor < 0)
  {
    logg.error("!!! SHADE_FACTOR must be superior or equal to 0. Please check!");
  }
  
  m_LightActiveGerm = LightParams.get_val<int>("ACTIVE_GERM");
  if (m_LightActiveGerm.size() != Rcount)
  {
    logg.error("!!! Wrong number of parameters provided for ACTIVE_GERM (LIGHT) (",
               m_LightActiveGerm.size(), " instead of ", Rcount, "). Please check!");
  }
  for(unsigned i=1; i<m_LightActiveGerm.size(); i++)
  {
    if (m_LightActiveGerm[i] < 0 || m_LightActiveGerm[i] > 100)
    {
      logg.error("!!! ACTIVE_GERM (LIGHT) values must be superior or equal to 0, and inferior or equal to 100. Please check!");
    }
  }
  
  /* get light tolerance as vector and reshape it into matrix format */
  vector<int> v_int = LightParams.get_val<int>("LIGHT_TOL");
  if (v_int.size() < (LScount-1) * Rcount)
  {
    logg.error("!!! Wrong number of parameters provided for LIGHT_TOL (",
               v_int.size(), " instead of ", (LScount-1) * Rcount,
               "). Please check!");
  }
  int counter = 0;
  m_LightTolerance.resize(LScount);
  for (int ls=1; ls<LScount; ls++)
  {
    m_LightTolerance[ls].resize(Rcount);
    for (int r=0; r<Rcount; r++)
    {
      if (v_int[counter] < 0 || v_int[counter] > 100)
      {
        logg.error("!!! LIGHT_TOL values must be superior or equal to 0, and inferior or equal to 100. Please check!");
      }
      m_LightTolerance[ls][r] = v_int[counter];
      counter ++;
    }
  }
  
  /* Propagule Light tolerance is assumed to be the same as germinants */
  m_LightTolerance[0].resize(Rcount);
  for (int r=0; r<Rcount; r++)
  {
    m_LightTolerance[0][r] = m_LightTolerance[1][r];
  }
  logg.info("> PFG light parameters provided");
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FG::getDispParams(const string& PFG_DispersalFile)
{
  /* 1. check parameter file existence */
  testFileExist("--PFG_PARAMS_DISPERSAL--", PFG_DispersalFile, false);
  
  /* 2. read dispersal parameters */
  par::Params DispParams(PFG_DispersalFile.c_str(), " = \"", "#");
  
  m_IsSeeded = false;
  
  vector<double> v_double = DispParams.get_val<double>("DISPERS_DIST");
  if (v_double.size() == 3)
  {
    m_disp50 = v_double[0];
    m_disp99 = v_double[1];
    m_dispLD = v_double[2];
  } else
  {
    logg.error("!!! Wrong number of parameters provided for DISPERS_DIST (",
               v_double.size(), " instead of ", 3, "). Please check!");
  }
  if (m_disp99 < m_disp50 || m_dispLD < m_disp50 || m_dispLD < m_disp99)
  {
    logg.error("!!! DISPERS_DIST must be given in ascending order (disp50 <= disp99 <= dispLD). Please check!");
  }
  
  logg.info("> PFG dispersal parameters provided");
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FG::getDistParams(const GSP& glob_params, const string& PFG_DisturbancesFile)
{
  m_DistResponse = FGresponse(PFG_DisturbancesFile, glob_params.getNoDist(), glob_params.getNoDistSub());
  logg.info("> PFG disturbances parameters provided");
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FG::getSoilParams(const string& PFG_SoilFile)
{
  /* 1. check parameter file existence */
  testFileExist("--PFG_PARAMS_SOIL--", PFG_SoilFile, false);
  
  /* 2. read dispersal parameters */
  par::Params SoilParams(PFG_SoilFile.c_str(), " = \"", "#");
  
  m_SoilContrib = SoilParams.get_val<double>("SOIL_CONTRIB")[0];
  m_SoilLow = SoilParams.get_val<double>("SOIL_LOW")[0];
  m_SoilHigh = SoilParams.get_val<double>("SOIL_HIGH")[0];
  if (m_SoilHigh < m_SoilContrib || m_SoilContrib < m_SoilLow || m_SoilHigh < m_SoilLow)
  {
    logg.error("!!! Soil values must be given in ascending order (SOIL_LOW <= SOIL_CONTRIB <= SOIL_HIGH). Please check!");
  }
  
  m_SoilActiveGerm = SoilParams.get_val<int>("ACTIVE_GERM");
  if (m_SoilActiveGerm.size() != Rcount)
  {
    logg.error("!!! Wrong number of parameters provided for ACTIVE_GERM (SOIL) (",
               m_SoilActiveGerm.size(), " instead of ", Rcount, "). Please check!");
  }
  for(unsigned i=1; i<m_SoilActiveGerm.size(); i++)
  {
    if (m_SoilActiveGerm[i] < 0 || m_SoilActiveGerm[i] > 100)
    {
      logg.error("!!! ACTIVE_GERM (SOIL) values must be superior or equal to 0, and inferior or equal to 100. Please check!");
    }
  }
  
  /* get soil tolerance as vector and reshape it into matrix format */
  vector<int> v_int = SoilParams.get_val<int>("SOIL_TOL", true);
  if (v_int.size() != Rcount * (LScount - 1))
  {
    logg.error("!!! Wrong number of parameters provided for SOIL_TOL (",
               v_int.size(), " instead of ", Rcount * (LScount - 1),
               "). Please check!");
  }
  int counter = 0;
  m_SoilTolerance.resize(LScount);
  for (unsigned i=1; i<m_SoilTolerance.size(); i++)
  { // fill automatically germinant LS case ==> not use at time
    m_SoilTolerance[i].resize(Rcount);
    for (unsigned j=0; j<m_SoilTolerance[i].size(); j++)
    {
      if (v_int[counter] < 0 || v_int[counter] > 100)
      {
        logg.error("!!! SOIL_TOL values must be superior or equal to 0, and inferior or equal to 100. Please check!");
      }
      m_SoilTolerance[i][j] = v_int[counter];
      counter ++;
    }
  }
  
  /* Propagule Soil tolerance is assumed to be the same as germinants */
  m_SoilTolerance[0].resize(Rcount);
  for (unsigned c=0; c<m_SoilTolerance[0].size(); c++)
  {
    m_SoilTolerance[0][c] = m_SoilTolerance[1][c];
  }
  
  logg.info("> PFG soil parameters provided");
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FG::getFireParams(const GSP& glob_params, const string& PFG_FireFile)
{
  /* 1. check parameter file existence */
  testFileExist("--PFG_PARAMS_FIRE--", PFG_FireFile, false);
  
  /* 2. read fire disturbance parameters */
  par::Params FireParams(PFG_FireFile.c_str(), " = \"", "#");
  m_FireResponse = FGresponse(PFG_FireFile, glob_params.getNoFireDist(), glob_params.getNoFireDistSub());
  m_Flamm = FireParams.get_val<double>("FLAMMABILITY")[0];
  logg.info("> PFG fire parameters provided");
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FG::getDrouParams(const GSP& glob_params, const string& PFG_DroughtFile)
{
  /* 1. check parameter file existence */
  testFileExist("--PFG_PARAMS_DROUGHT--", PFG_DroughtFile, false);
  
  /* 2. read drought disturbance parameters */
  par::Params DroughtParams(PFG_DroughtFile.c_str(), " = \"", "#");
  m_DroughtResponse = FGresponse(PFG_DroughtFile, 2, glob_params.getNoDroughtSub());
  m_DroughtThreshMod = DroughtParams.get_val<double>("DROUGHT_THRESH_MOD")[0];
  m_DroughtThreshSev = DroughtParams.get_val<double>("DROUGHT_THRESH_SEV")[0];
  m_CountRecovery = DroughtParams.get_val<int>("COUNTER_RECOVERY")[0];
  m_CountSens = DroughtParams.get_val<int>("COUNTER_SENS")[0];
  m_CountCum = DroughtParams.get_val<int>("COUNTER_CUM")[0];
  logg.info("> PFG drought parameters provided");
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FG::FG(const GSP& glob_params, const FOPL& file_of_params, const unsigned& fg_id)
{
  bool wrong_identifier = false;
  
  bool doLight = glob_params.getDoLightInteraction();
  bool doDisp = glob_params.getDoDispersal();
  bool doDist = glob_params.getDoDisturbances();
  bool doSoil = glob_params.getDoSoilInteraction();
  bool doFire = glob_params.getDoFireDisturbances();
  bool doDrought = glob_params.getDoDroughtDisturbances();
  
  if (fg_id < file_of_params.getFGLifeHistory().size())
  {
    getSuccParams(glob_params,file_of_params.getFGLifeHistory()[fg_id]);
    if (doLight)
    {
      if (fg_id < file_of_params.getFGLight().size())
      {
        getLightParams(file_of_params.getFGLight()[fg_id]);
      } else
      {
        wrong_identifier = true;
      }
    } else
    {
      m_LightShadeFactor = 1.0;
      m_LightActiveGerm.resize(Rcount, 100);
      m_LightTolerance.resize(LScount, vector<int>(Rcount, 100));
    }
    if (doDisp)
    {
      if (fg_id < file_of_params.getFGDispersal().size())
      {
        getDispParams(file_of_params.getFGDispersal()[fg_id]);
      } else
      {
        wrong_identifier = true;
      }
    } else
    {
      m_IsSeeded = false;
      m_disp50 = 0.0;
      m_disp99 = 0.0;
      m_dispLD = 0.0;
    }
    if (doSoil)
    {
      if (fg_id < file_of_params.getFGSoil().size())
      {
        getSoilParams(file_of_params.getFGSoil()[fg_id]);
      } else
      {
        wrong_identifier = true;
      }
    } else
    {
      m_SoilContrib = 0.0;
      m_SoilLow = 0.0;
      m_SoilHigh = 0.0;
      m_SoilActiveGerm.resize(Rcount, 100);
      m_SoilTolerance.resize(LScount, vector<int>(Rcount, 100));
    }
    if (doDist)
    {
      if (fg_id < file_of_params.getFGDisturbance().size())
      {
        getDistParams(glob_params, file_of_params.getFGDisturbance()[fg_id]);
      } else
      {
        wrong_identifier = true;
      }
    } else
    {
      m_DistResponse = FGresponse();
    }
    if (doFire)
    {
      if (fg_id < file_of_params.getFGFire().size())
      {
        getFireParams(glob_params, file_of_params.getFGFire()[fg_id]);
      } else
      {
        wrong_identifier = true;
      }
    } else
    {
      m_FireResponse = FGresponse();
      m_Flamm = 0.0;
    }
    if (doDrought)
    {
      if (fg_id < file_of_params.getFGDrought().size())
      {
        getDrouParams(glob_params, file_of_params.getFGDrought()[fg_id]);
      } else
      {
        wrong_identifier = true;
      }
    } else
    {
      m_DroughtResponse = FGresponse();
      m_DroughtThreshMod = 0.0;
      m_DroughtThreshSev = 0.0;
      m_CountRecovery = 0;
      m_CountSens = 0;
      m_CountCum = 0;
    }
    
  } else
  {
    wrong_identifier = true;
  }
  
  if (wrong_identifier)
  {
    logg.error("!!! Wrong identifier of FG given. Please check!");
  } else
  {
    this->show();
  }
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FG::~FG()
{
  /* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

const string& FG::getName() const {return m_Name;}
int FG::getMatTime() const {return m_M;}
int FG::getLifeSpan() const {return m_L;}
int FG::getMaxAbund() const {return m_MaxAbund;}
int FG::getImmSize() const {return m_ImmSize;}
int FG::getMaxStratum() const {return m_MaxStratum;}
const vector<int> FG::getStrata() const {return m_Strata;}
int FG::getStrata(const int& i) const {return m_Strata[i];}
const vector<int> FG::getPoolLife() const {return m_PoolL;}
int FG::getPoolLife(const PoolType& pt ) const {return m_PoolL[pt];}
bool FG::getInnateDormancy() const {return m_InnateDorm;}
int FG::getPotentialFecund() const {return m_PotentialFecundity;}
double FG::getLightShadeFactor() const {return m_LightShadeFactor;}
const vector<int> FG::getLightActiveGerm() const {return m_LightActiveGerm;}
const int& FG::getLightActiveGerm(const Resource& r) const {return m_LightActiveGerm[r];}
const vector< vector<int> >& FG::getLightTolerance() const {return m_LightTolerance;}
int FG::getLightTolerance(LifeStage ls, Resource r) const {return m_LightTolerance[ls][r];}
bool FG::getIsSeeded() const {return m_IsSeeded;}
double FG::getDisp50() const {return m_disp50;}
double FG::getDisp99() const {return m_disp99;}
double FG::getDispLD() const {return m_dispLD;}
double FG::getSoilContrib() const {return m_SoilContrib;}
double FG::getSoilLow() const {return m_SoilLow;}
double FG::getSoilHigh() const {return m_SoilHigh;}
const vector<int> FG::getSoilActiveGerm() const {return m_SoilActiveGerm;}
const int& FG::getSoilActiveGerm(const Resource& r) const {return m_SoilActiveGerm[r];}
const vector< vector<int> >& FG::getSoilTolerance() const {return m_SoilTolerance;}
int FG::getSoilTolerance(LifeStage ls, Resource r) const { return m_SoilTolerance[ls][r];}
FGresponse FG::getDistResponse() {return m_DistResponse;}
const FGresponse& FG::getFireResponse() const {return m_FireResponse;}
double FG::getFlamm() const {return m_Flamm;}
const FGresponse& FG::getDroughtResponse() const {return m_DroughtResponse;}
double FG::getDroughtThreshMod() const {return m_DroughtThreshMod;}
double FG::getDroughtThreshSev() const {return m_DroughtThreshSev;}
int FG::getCountRecovery() const {return m_CountRecovery;}
int FG::getCountSens() const {return m_CountSens;}
int FG::getCountCum() const {return m_CountCum;}
bool FG::getIsAlien() const {return m_IsAlien;}


void FG::setName(const string& name){m_Name = name;}
void FG::setMatTime(const int& matTime){m_M = matTime;}
void FG::setLifeSpan(const int& lifeSpan){m_L = lifeSpan;}
void FG::setMaxAbund(const int& maxAbund){m_MaxAbund = maxAbund;}
void FG::setImmSize(const int& immSize){m_ImmSize = immSize;}
void FG::setMaxStratum(const int& maxStratum){m_MaxStratum = maxStratum;}
void FG::setStrata(const vector<int>& strata){m_Strata = strata;}
void FG::setStrata(const int& strata, const int& i){m_Strata[i] = strata;}
void FG::setPoolLife(const int (&poolLife)[ PTcount ]){for(int i=0; i<PTcount; i++){m_PoolL[i] = poolLife[i];}}
void FG::setPoolLife(const int& poolLife, const PoolType& pt ){m_PoolL[pt] = poolLife;}
void FG::setInnateDormancy(const bool& innateDormancy){m_InnateDorm = innateDormancy;}
void FG::setPotentialFecund(const int& potentialFecund){m_PotentialFecundity = potentialFecund;}
void FG::setLightShadeFactor(const double& lightShadeFactor){m_LightShadeFactor = lightShadeFactor;}
void FG::setLightActiveGerm(const int (&activeGerm) [ Rcount ] ){ for(int i=0; i<Rcount; i++){m_LightActiveGerm[i] = activeGerm[i];}}
void FG::setLightActiveGerm(const int& activeGerm, const Resource& r ){ m_LightActiveGerm[r] = activeGerm;}
void FG::setTolerance(const int (&tolerance)[ LScount ][ Rcount ]){
  for(int i=0; i<LScount; i++){
    for(int j=0; j<Rcount; j++){
      m_LightTolerance[i][j] = tolerance[i][j];}}}
void FG::setTolerance(const int& tolerance, const LifeStage& ls, const Resource& r){m_LightTolerance[ls][r] = tolerance;}
void FG::setIsSeeded(const bool& isSeeded){m_IsSeeded = isSeeded;}
void FG::setDisp50(const double& disp50){ m_disp50 = disp50;}
void FG::setDisp99(const double& disp99){ m_disp99 = disp99;}
void FG::setDispLD(const double& dispLD){ m_dispLD = dispLD;}
void FG::setSoilContrib(const double& soilContrib) {m_SoilContrib = soilContrib;}
void FG::setSoilLow(const double& soilLow) {m_SoilLow = soilLow;}
void FG::setSoilHigh(const double& soilHigh) {m_SoilHigh = soilHigh;}
void FG::setSoilActiveGerm(const int (&activeGerm) [ Rcount ] ){ for(int i=0; i<Rcount; i++){m_SoilActiveGerm[i] = activeGerm[i];}}
void FG::setSoilActiveGerm(const int& activeGerm, const Resource& r ){ m_SoilActiveGerm[r] = activeGerm;}
void FG::setSoilTolerance(const vector< vector<int> >& tolerance) { m_SoilTolerance = tolerance; }
void FG::setSoilTolerance(const int& tolerance, const LifeStage& ls, const Resource& r) { m_SoilTolerance[ls][r] = tolerance; }
void FG::setDistResponse(const FGresponse& distResponse){m_DistResponse = distResponse;}
void FG::setFireResponse(const FGresponse& fireResponse){m_FireResponse = fireResponse;}
void FG::setFlamm(const double& flamm){m_Flamm = flamm;}
void FG::setDroughtResponse(const FGresponse& droughtResponse){m_DroughtResponse = droughtResponse;}
void FG::setDroughtThreshMod(const double& droughtThreshMod){m_DroughtThreshMod = droughtThreshMod;}
void FG::setDroughtThreshSev(const double& droughtThreshSev){m_DroughtThreshSev = droughtThreshSev;}
void FG::setCountRecovery(const int& countRecovery){m_CountRecovery = countRecovery;}
void FG::setCountSens(const int& countSens){m_CountSens = countSens;}
void FG::setCountCum(const int& countCum){m_CountCum = countCum;}
void FG::setIsAlien(const bool& isAlien){m_IsAlien = isAlien;}



/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FG::show()
{
  logg.debug("\n*********************************************",
             "\n** Functional Group Parameters:",
             "\n*********************************************\n",
             "\nm_Name = ", m_Name,
             "\nm_M = ", m_M,
             "\nm_L = ", m_L,
             "\nm_MaxAbund = ", m_MaxAbund,
             "\nm_ImmSize = ", m_ImmSize,
             "\nm_MaxStratum = ", m_MaxStratum,
             "\nm_Strata = ", m_Strata,
             "\nm_PoolL = ", m_PoolL,
             "\nm_InnateDorm = ", m_InnateDorm,
             "\nm_PotentialFecundity = ", m_PotentialFecundity,
             "\nm_LightShadeFactor = ", m_LightShadeFactor,
             "\nm_LightActiveGerm = (column: resource) ", m_LightActiveGerm,
             "\nm_LightTolerance = (line: life stage, column: resource)", m_LightTolerance,
             "\nm_IsSeeded = ", m_IsSeeded,
             "\nm_disp50 = ", m_disp50,
             "\nm_disp99 = ", m_disp99,
             "\nm_dispLD = ", m_dispLD,
             "\n** m_DistResponse =");
  m_DistResponse.show();
  logg.debug("\nm_SoilContrib = ", m_SoilContrib,
             "\nm_SoilLow = ", m_SoilLow,
             "\nm_SoilHigh = ", m_SoilHigh,
             "\nm_SoilActiveGerm = (column: resource) ", m_SoilActiveGerm,
             "\nm_SoilTolerance = (line: life stage, column: resource)", m_SoilTolerance,
             "\n** m_FireResponse =");
  m_FireResponse.show();
  logg.debug("\nm_Flamm = ", m_Flamm,
             "\n** m_DroughtResponse =");
  m_DroughtResponse.show();
  logg.debug("\nm_DroughtThreshMod = ", m_DroughtThreshMod,
             "\nm_DroughtThreshSev = ", m_DroughtThreshSev,
             "\nm_CountRecovery = ", m_CountRecovery,
             "\nm_CountSens = ", m_CountSens,
             "\nm_CountCum = ", m_CountCum,
             "\nm_IsAlien = ", m_IsAlien);
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/