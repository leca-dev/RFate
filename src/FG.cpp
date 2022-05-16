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

FG::FG() : m_Name(""), m_M(0), m_L(1), m_MaxA(ANone), m_ImmSize(0.0), m_MaxStratum(0), m_Strata(0,1000), /* Life history*/
m_PoolL(PTcount,0), m_InnateDorm(false), m_PotentialFecundity(100), /* Propagule biology */
m_LightActiveGerm(Rcount, PC100), m_LightTolerance(LScount, vector<bool>(Rcount, true)), /* Light response */
m_Dispersed(false), m_disp50(0.0), m_disp99(0.0), m_dispLD(0.0), /* Dispersal module */
m_SoilContrib(0.0), m_SoilLow(0.0), m_SoilHigh(0.0), /* Soil response */
m_SoilActiveGerm(Rcount, PC100), m_SoilTolerance(LScount, vector<Fract>(Rcount, PC100)), /* Soil response */
m_DistResponse(FGresponse()), /* Disturbance response */
m_FireResponse(FGresponse()), m_Flamm(0.0), /* Fire response */
m_DroughtResponse(FGresponse()), m_DroughtSD(2,0.0), m_CountModToSev(0), m_CountSevMort(0), m_DroughtRecovery(0), /* Drought response */
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

	m_MaxA = Abund(SuccParams.get_val<int>("MAX_ABUNDANCE")[0]);
	m_ImmSize = FractToDouble(Fract(SuccParams.get_val<int>("IMM_SIZE")[0]));
	//m_MaxStratum = SuccParams.get_val<int>("MAX_STRATUM")[0];
	vector<int> v_int = SuccParams.get_val<int>("MAX_STRATUM", true);
	if (v_int.size()) m_MaxStratum = v_int[0]; else m_MaxStratum = glob_params.getNoStrata();
	m_Strata = SuccParams.get_val<int>("CHANG_STR_AGES");
	m_Strata.push_back(10000); /* High value of to avoid PFGs to exit the upper stata */
	if (m_Strata.size() != glob_params.getNoStrata() + 1)
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

void FG::getLightParams(const GSP& glob_params, const string& PFG_LightFile)
{
	/* 1. check parameter file existence */
	testFileExist("--PFG_PARAMS_LIGHT--", PFG_LightFile, false);

	/* 2. read light parameters */
	par::Params LightParams(PFG_LightFile.c_str(), " = \"", "#");

	vector<int> v_int = LightParams.get_val<int>("ACTIVE_GERM");
	m_LightActiveGerm = convert_int_to_enum<Fract>("ACTIVE_GERM", v_int, "Fract", Fcount);
	if (m_LightActiveGerm.size() != Rcount)
	{
		logg.error("!!! Wrong number of parameters provided for ACTIVE_GERM (LIGHT) (",
               m_LightActiveGerm.size(), " instead of ", Rcount, "). Please check!");
	}

	/* get light tolerance as vector and reshape it into matrix format */
	v_int = LightParams.get_val<int>("LIGHT_TOL");
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

void FG::getDispParams(const GSP& glob_params, const string& PFG_DispersalFile)
{
	/* 1. check parameter file existence */
	testFileExist("--PFG_PARAMS_DISPERSAL--", PFG_DispersalFile, false);

	/* 2. read dispersal parameters */
	par::Params DispParams(PFG_DispersalFile.c_str(), " = \"", "#");

	m_Dispersed = false;

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

void FG::getSoilParams(const GSP& glob_params, const string& PFG_SoilFile)
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

	vector<int> v_int = SoilParams.get_val<int>("ACTIVE_GERM");
	m_SoilActiveGerm = convert_int_to_enum<Fract>("ACTIVE_GERM", v_int, "Fract", Fcount);
	if (m_SoilActiveGerm.size() != Rcount)
	{
		logg.error("!!! Wrong number of parameters provided for ACTIVE_GERM (SOIL) (",
               m_SoilActiveGerm.size(), " instead of ", Rcount, "). Please check!");
	}

	/* get soil tolerance as vector and reshape it into matrix format */
	v_int = SoilParams.get_val<int>("SOIL_TOL", true);
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
			m_SoilTolerance[i][j] = convert_int_to_enum<Fract>("SOIL_TOL", v_int[counter], "Fract", Fcount);
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
	m_DroughtSD = DroughtParams.get_val<double>("DROUGHT_SD");
	m_CountModToSev = DroughtParams.get_val<unsigned>("COUNT_MOD_TO_SEV")[0];
	m_CountSevMort = DroughtParams.get_val<unsigned>("COUNT_SEV_MORT")[0];
	m_DroughtRecovery = DroughtParams.get_val<unsigned>("DROUGHT_RECOVERY")[0];
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
				getLightParams(glob_params,file_of_params.getFGLight()[fg_id]);
			} else
			{
				wrong_identifier = true;
			}
		} else
		{
			m_LightActiveGerm.resize(Rcount, PC100);
			m_LightTolerance.resize(LScount, vector<bool>(Rcount, true));
		}
		if (doDisp)
		{
			if (fg_id < file_of_params.getFGDispersal().size())
			{
				getDispParams(glob_params,file_of_params.getFGDispersal()[fg_id]);
			} else
			{
				wrong_identifier = true;
			}
		} else
		{
			m_Dispersed = false;
			m_disp50 = 0.0;
			m_disp99 = 0.0;
			m_dispLD = 0.0;
		}
		if (doSoil)
		{
			if (fg_id < file_of_params.getFGSoil().size())
			{
				getSoilParams(glob_params,file_of_params.getFGSoil()[fg_id]);
			} else
			{
				wrong_identifier = true;
			}
		} else
		{
			m_SoilContrib = 0.0;
			m_SoilLow = 0.0;
			m_SoilHigh = 0.0;
			m_SoilActiveGerm.resize(Rcount, PC100);
			m_SoilTolerance.resize(LScount, vector<Fract>(Rcount, PC100));
			//m_SoilTolerance.resize(LScount, vector<bool>(1, true));
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
			m_DroughtSD.resize(2,0.0);
			m_CountModToSev = 0;
			m_CountSevMort = 0;
			m_DroughtRecovery = 0;
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
const Abund& FG::getMaxAbund() const {return m_MaxA;}
//int FG::getMaxAbund() {return(AbundToInt(m_MaxA));}
double FG::getImmSize() const {return m_ImmSize;}
int FG::getMaxStratum() const {return m_MaxStratum;}
const vector<int> FG::getStrata() const {return m_Strata;}
int FG::getStrata(const int& i) const {return m_Strata[i];}
const vector<int> FG::getPoolLife() const {return m_PoolL;}
int FG::getPoolLife(const PoolType& pt ) const {return m_PoolL[pt];}
bool FG::getInnateDormancy() const {return m_InnateDorm;}
int FG::getPotentialFecund() const {return m_PotentialFecundity;}
const vector<Fract> FG::getMaxRecruitLight() const {return m_LightActiveGerm;}
const Fract& FG::getMaxRecruitLight(const Resource& r) const {return m_LightActiveGerm[r];}
const vector< vector<bool> >& FG::getLightTolerance() const {return m_LightTolerance;}
bool FG::getLightTolerance(LifeStage ls, Resource r) const {return m_LightTolerance[ls][r];}
bool FG::getDispersed() const {return m_Dispersed;}
double FG::getDisp50() const {return m_disp50;}
double FG::getDisp99() const {return m_disp99;}
double FG::getDispLD() const {return m_dispLD;}
double FG::getSoilContrib() const {return m_SoilContrib;}
double FG::getSoilLow() const {return m_SoilLow;}
double FG::getSoilHigh() const {return m_SoilHigh;}
const vector<Fract> FG::getMaxRecruitSoil() const {return m_SoilActiveGerm;}
const Fract& FG::getMaxRecruitSoil(const Resource& r) const {return m_SoilActiveGerm[r];}
const vector< vector<Fract> >& FG::getSoilTolerance() const {return m_SoilTolerance;}
const Fract FG::getSoilTolerance(LifeStage ls, Resource r) const { return m_SoilTolerance[ls][r];}
FGresponse FG::getDistResponse() {return m_DistResponse;}
const FGresponse& FG::getFireResponse() const {return m_FireResponse;}
double FG::getFlamm() const {return m_Flamm;}
const FGresponse& FG::getDroughtResponse() const {return m_DroughtResponse;}
const vector<double>& FG::getDroughtSD() const {return m_DroughtSD;}
unsigned FG::getCountModToSev() const {return m_CountModToSev;}
unsigned FG::getCountSevMort() const {return m_CountSevMort;}
unsigned FG::getDroughtRecovery() const {return m_DroughtRecovery;}
bool FG::getIsAlien() const {return m_IsAlien;}


void FG::setName(const string& name){m_Name = name;}
void FG::setMatTime(const int& matTime){m_M = matTime;}
void FG::setLifeSpan(const int& lifeSpan){m_L = lifeSpan;}
void FG::setMaxAbund(const Abund& maxAbund){m_MaxA = maxAbund;}
void FG::setImmSize(const double& immSize){m_ImmSize = immSize;}
void FG::setMaxStratum(const int& maxStratum){m_MaxStratum = maxStratum;}
void FG::setStrata(const vector<int>& strata){m_Strata = strata;}
void FG::setStrata(const int& strata, const int& i){m_Strata[i] = strata;}
void FG::setPoolLife(const int (&poolLife)[ PTcount ]){for(int i=0; i<PTcount; i++){m_PoolL[i] = poolLife[i];}}
void FG::setPoolLife(const int& poolLife, const PoolType& pt ){m_PoolL[pt] = poolLife;}
void FG::setInnateDormancy(const bool& innateDormancy){m_InnateDorm = innateDormancy;}
void FG::setPotentialFecund(const int& potentialFecund){m_PotentialFecundity = potentialFecund;}
void FG::setMaxRecruitLight(const Fract (&maxRecruit) [ Rcount ] ){ for(int i=0; i<Rcount; i++){m_LightActiveGerm[i] = maxRecruit[i];}}
void FG::setMaxRecruitLight(const Fract& maxRecruit, const Resource& r ){ m_LightActiveGerm[r] = maxRecruit;}
void FG::setTolerance(const bool (&tolerance)[ LScount ][ Rcount ]){
  for(int i=0; i<LScount; i++){
    for(int j=0; j<Rcount; j++){
      m_LightTolerance[i][j] = tolerance[i][j];}}}
void FG::setTolerance(const bool& tolerance, const LifeStage& ls, const Resource& r){m_LightTolerance[ls][r] = tolerance;}
void FG::setDispersed(const bool& dispersed){m_Dispersed = dispersed;}
void FG::setDisp50(const double& disp50){ m_disp50 = disp50;}
void FG::setDisp99(const double& disp99){ m_disp99 = disp99;}
void FG::setDispLD(const double& dispLD){ m_dispLD = dispLD;}
void FG::setSoilContrib(const double& soilContrib) {m_SoilContrib = soilContrib;}
void FG::setSoilLow(const double& soilLow) {m_SoilLow = soilLow;}
void FG::setSoilHigh(const double& soilHigh) {m_SoilHigh = soilHigh;}
void FG::setMaxRecruitSoil(const Fract (&maxRecruit) [ Rcount ] ){ for(int i=0; i<Rcount; i++){m_SoilActiveGerm[i] = maxRecruit[i];}}
void FG::setMaxRecruitSoil(const Fract& maxRecruit, const Resource& r ){ m_SoilActiveGerm[r] = maxRecruit;}
void FG::setSoilTolerance(const vector< vector<Fract> >& tolerance) { m_SoilTolerance = tolerance; }
void FG::setSoilTolerance(const Fract& tolerance, const LifeStage& ls, const Resource& r) { m_SoilTolerance[ls][r] = tolerance; }
void FG::setDistResponse(const FGresponse& distResponse){m_DistResponse = distResponse;}
void FG::setFireResponse(const FGresponse& fireResponse){m_FireResponse = fireResponse;}
void FG::setFlamm(const double& flamm){m_Flamm = flamm;}
void FG::setDroughtResponse(const FGresponse& droughtResponse){m_DroughtResponse = droughtResponse;}
void FG::setDroughtSD(const vector<double>& droughtSD){m_DroughtSD = droughtSD;}
void FG::setCountModToSev(const unsigned& countModToSev){m_CountModToSev = countModToSev;}
void FG::setCountSevMort(const unsigned& countSevMort){m_CountSevMort = countSevMort;}
void FG::setDroughtRecovery(const unsigned& droughtRecovery){m_DroughtRecovery = droughtRecovery;}
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
             "\nm_MaxA = ", m_MaxA,
             "\nm_ImmSize = ", m_ImmSize,
             "\nm_MaxStratum = ", m_MaxStratum,
             "\nm_Strata = ", m_Strata,
             "\nm_PoolL = ", m_PoolL,
             "\nm_InnateDorm = ", m_InnateDorm,
             "\nm_PotentialFecundity = ", m_PotentialFecundity,
             "\nm_LightActiveGerm = (column: resource) ", m_LightActiveGerm,
             "\nm_LightTolerance = (line: life stage, column: resource)", m_LightTolerance,
             "\nm_Dispersed = ", m_Dispersed,
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
	logg.debug("\nm_DroughtSD = ", m_DroughtSD,
             "\nm_CountModToSev = ", m_CountModToSev,
             "\nm_CountSevMort = ", m_CountSevMort,
             "\nm_DroughtRecovery = ", m_DroughtRecovery,
             "\nm_IsAlien = ", m_IsAlien);
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
