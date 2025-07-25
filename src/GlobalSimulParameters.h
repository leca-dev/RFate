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

/*============================================================================*/
/*                     Global Simulation Parameters Class                     */
/*============================================================================*/

/*!
 * \file GSP.h
 * \brief Global Simulation Parameters Class
 * \author Damien Georges, Maya Gueguen
 * \version 1.0
 * \date 2020
 */

#ifndef GSP_H
#define GSP_H

#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/version.hpp>
#include "Params.h"
#include "FGUtils.h"
#include "Logger.h"

using namespace std;


/*!
 * \class GSP
 * \brief Global Simulation Parameters Class
 *
 * This class contains a set of parameters related to the simulation process.
 * Basic parameters are mandatory : they concern simulation timings (simulation
 * duration, seeding duration and timestep, ...) and PFG demography (number of
 * height strata, maximum abundances, ...).
 * If some modules are activated (dispersal, habitat suitability, light, ...),
 * some specific parameters might also be required.
 */

class GSP
{
	private:

	unsigned m_Seed; /*!< Seed for stochastic steps (dispersal, disturbance frequency, envsuitRef) */
	int m_NoCPU; /*!< Number of CPU for parallel computing */
	int m_NoFG; /*!< Number of PFG studied */
	int m_NoStrata; /*!< Number of height strata */
	int m_SimulDuration; /*!< Simulation duration */
	int m_SeedingDuration; /*!< Seeding duration */
	int m_SeedingTimeStep; /*!< Seeding time step */
	int m_SeedingInput; /*!< Number of seeds introduced during seeding */
  int m_PotentialFecundity; /*!< Potential Fecundity of mature plants (maximum value of seeds produced in optimal conditions) */

  /* Saving parameters */
  bool m_DoSavingPFGStratum; /*!< Unable or not the saving of abundance maps per PFG per stratum */
  bool m_DoSavingPFG; /*!< Unable or not the saving of abundance maps per PFG (sum over strata) */
  bool m_DoSavingStratum; /*!< Unable or not the saving of abundance maps per stratum (sum over PFG) */

	/* Light interaction module */
	bool m_DoLightInteraction; /*!< Unable or not Light interaction module */
	int m_LightThreshLow; /*!< Threshold to transform PFG abundances into Low light resources */
	int m_LightThreshMedium; /*!< Threshold to transform PFG abundances into Medium light resources */
	bool m_LightRecruitment; /*!< Unable or not the dependency of recruitment based on strata 0 light resources */
	bool m_LightSaving; /*!< Unable or not the saving of light resources maps */
	
	/* Habitat suitability module */
	bool m_DoHabSuitability; /*!< Unable or not habitat suitability module */
	int m_HabSuitMode; /*!< Option to draw the habitat suitability ref */

	/* Dispersal module */
	bool m_DoDispersal; /*!< Unable or not dispersal module */
	int m_DispersalMode; /*!< Option to disperse the seeds */
	bool m_DispersalSaving; /*!< Unable or not the saving of dispersal maps after dispersal */
	
	/* Disturbances module */
	bool m_DoDisturbances; /*!< Unable or not disturbance module */
	int m_NoDist; /*!< Number of disturbances */
	int m_NoDistSub; /*!< Number of disturbances subdivision (no of way to react to dist) */
	vector<int> m_FreqDist; /*!< Frequency of each disturbance in years */
	vector<double> m_ProbDist; /*!< Probability of pixels to be impacted by each disturbance */
	vector<int> m_PairDist; /*!< Disturbance paired identification */
	
	/* Soil interaction module */
	bool m_DoSoilInteraction; /*!< Unable or not Soil interaction module */
	bool m_SoilFillMap; /*! Fill or not initialization soil map with initialization soil value */
	double m_SoilInit; /*!< Initialization soil value */
	double m_SoilRetention; /*!< Percentage of soil from previous year to keep */
	bool m_SoilRecruitment; /*!< Unable or not the dependency of recruitment based on soil medium resources */
	bool m_SoilSaving; /*!< Unable or not the saving of soil resources maps */
	
	/* Fire disturbance module */
	bool m_DoFireDisturbances; /*!< Unable or not fire disturbance module */
	int m_NoFireDist; /*!< Number of fire disturbances */
	int m_NoFireDistSub; /*!< Number of fire disturbances subdivision (no of way to react to dist) */
	vector<int> m_FreqFireDist; /*!< Frequency of each fire disturbance in years */
	int m_FireIgnitMode; /*!< ignition option (fire) */
	int m_FireNeighMode; /*!< neighbour option (fire) */
	int m_FirePropMode; /*!< propagation option (fire) */
	int m_FireQuotaMode; /*!< quota option (fire) */
	vector<int> m_FireIgnitNo; /*!< Number of starting fires fore each fire intensity */
	vector<int> m_FireIgnitNoHist; /*!< Previous data of number of starting fires */
	int m_FireIgnitFlammMax; /*!< Maximum flammability of PFG */
	vector<double> m_FireIgnitLogis; /*!< Logistic parameters for the baseline probability of fire ignition */
	vector<int> m_FireNeighCC; /*!< Fire size extent in each direction (north, east, south, west) */
	vector<double> m_FirePropIntensity; /*!< Probabilities of propagation, depending on fire intensity */
	vector<double> m_FirePropLogis; /*!< Logistic parameters for the baseline probability of fire spread */
	int m_FireQuotaMax; /*!< Maximum step / amount / cells before end of fire spreading */

	/* Drought disturbance module */
	bool m_DoDroughtDisturbances; /*!< Unable or not drought disturbance module */
	int m_NoDroughtSub; /*!< Number of drought disturbances subdivision (no of way to react to dist) */
	string m_ChronoPost; /*!< When are applied post drought effects ("prev" or "post" succession) */
	string m_ChronoCurr; /*!< When are applied current drought effects ("prev" or "post" succession) */

	/* Aliens introduction module */
	bool m_DoAliensIntroduction; /*!< Unable or not aliens introduction module */
	vector<int> m_FreqAliens; /*!< Frequency of each aliens introduction in years */

	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		ar & m_Seed;
		ar & m_NoCPU;
		ar & m_NoFG;
		ar & m_NoStrata;
		ar & m_SimulDuration;
		ar & m_SeedingDuration;
		ar & m_SeedingTimeStep;
		ar & m_SeedingInput;
		ar & m_PotentialFecundity;
		ar & m_DoSavingPFGStratum;
		ar & m_DoSavingPFG;
		ar & m_DoSavingStratum;
		ar & m_DoLightInteraction;
		ar & m_LightThreshLow;
		ar & m_LightThreshMedium;
		ar & m_LightRecruitment;
		ar & m_LightSaving;
		ar & m_DoHabSuitability;
		ar & m_HabSuitMode;
		ar & m_DoDispersal;
		ar & m_DispersalMode;
		ar & m_DispersalSaving;
		ar & m_DoDisturbances;
		ar & m_NoDist;
		ar & m_NoDistSub;
		ar & m_FreqDist;
		ar & m_ProbDist;
		ar & m_PairDist;
		ar & m_DoSoilInteraction;
		ar & m_SoilFillMap;
		ar & m_SoilInit;
		ar & m_SoilRetention;
		ar & m_SoilRecruitment;
		ar & m_SoilSaving;
		ar & m_DoFireDisturbances;
		ar & m_NoFireDist;
		ar & m_NoFireDistSub;
		ar & m_FreqFireDist;
		ar & m_FireIgnitMode;
		ar & m_FireNeighMode;
		ar & m_FirePropMode;
		ar & m_FireQuotaMode;
		ar & m_FireIgnitNo;
		ar & m_FireIgnitNoHist;
		ar & m_FireIgnitFlammMax;
		ar & m_FireIgnitLogis;
		ar & m_FireNeighCC;
		ar & m_FirePropIntensity;
		ar & m_FirePropLogis;
		ar & m_FireQuotaMax;
		ar & m_DoDroughtDisturbances;
		ar & m_NoDroughtSub;
		ar & m_ChronoPost;
		ar & m_ChronoCurr;
		ar & m_DoAliensIntroduction;
		ar & m_FreqAliens;
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	GSP default constructor => All parameters are set to 0, False or None
	 */
	GSP();

	/*!
	 *	\brief Full constructor
	 *
	 *	GSP full constructor
	 *
	 *	\param globalParamsFile : path to text file containing well-formatted
	 * global simulation related parameters
	 */
	GSP(const string globalParamsFile);

	/*!
	 *	\brief Full constructor
	 *
	 *	GSP full constructor
	 *
	 *	\param seed : seed for stochastic steps (dispersal, disturbance frequency, envsuitRef)
	 *	\param noCPU : number of CPUs required for simulation
	 *	\param noFG : number of Functional Groups involved
	 *	\param noStrata : number of height strata
	 *	\param simulDuration : simulation duration (in years)
	 *	\param seedingDuration : seeding duration (in years)
	 *	\param seedingTimeStep : seeding time step (in years)
	 *	\param seedingInput : number of seeds dispersed each year during seeding
	 *	\param potentialFecundity : potential fecundity of mature plants 
	 * (maximum value of seeds produced in optimal conditions)
	 *  \param doSavingPFGStratum : unable or not the saving of abundance maps 
	 * per PFG per stratum
	 *  \param doSavingPFG : unable or not the saving of abundance maps 
	 * per PFG (sum over strata)
	 *  \param doSavingStratum : unable or not the saving of abundance maps 
	 * per stratum (sum over PFG)
	 *	\param doLightInteraction : unable or not Light interaction module
	 *	\param lightThreshLow : threshold to transform PFG abundances into Low
	 * light resources
	 *	\param lightThreshMedium : threshold to transform PFG abundances into
	 * Medium light resources
	 *  \param lightRecruitment : unable or not the dependency of recruitment 
	 * based on strata 0 light resources
	 *  \param lightSaving : unable or not the saving of light resources maps
	 *	\param doHabSuitability : unable or not Habitat suitability module
	 *	\param habSuitMode : option to draw the habitat suitability ref
	 *	\param doDispersal : unable or not Dispersal module
	 *  \param dispersalMode : option to disperse the seeds
	 *  \param dispersalSaving : unable or not the saving of dispersal maps 
	 *  after dispersal
	 *	\param doDisturbances : unable or not Disturbances module
	 *	\param noDist : number of disturbances involved
	 *	\param noDistSub : number of way a FG can react to a disturbance
	 *  \param freqDist : the frequency of each disturbance
	 *  \param probDist : probability of pixels to be impacted by each disturbance
	 *  \param pairDist : disturbance paired identification
	 *	\param doSoilInteraction : unable or not Soil interaction module
	 *	\param soilFillMap : fill or not initialization soil map with initialization 
	 * soil value
	 *	\param soilInit : initialization soil value
	 *	\param soilRetention : percentage of soil from previous year to keep
	 *  \param soilRecruitment : unable or not the dependency of recruitment 
	 * based on soil medium resources
	 *  \param soilSaving : unable or not the saving of soil resources maps
	 *	\param doFireDisturbances : unable or not Fire disturbances module
	 *	\param noFireDist : number of fire disturbances involved
	 *	\param noFireDistSub : number of way a FG can react to a fire disturbance
	 *	\param freqFireDist : the frequency of each fire disturbance
	 *  \param fireIgnitMode : ignition option (fire)
	 *  \param fireNeighMode : neighbour option (fire)
	 *  \param firePropMode : propagation option (fire)
	 *  \param fireQuotaMode : quota option (fire)
	 *	\param fireIgnitNo : number of starting fires fore each fire intensity
	 *	\param fireIgnitNoHist : previous data of number of starting fires
	 *	\param fireIgnitFlammMax : maximum flammability of PFG
	 *	\param fireIgnitLogis : logistic parameters for the baseline probability of
	 * fire ignition
	 *	\param fireNeighCC : fire size extent in each direction (north, east, south,
	 * west)
	 *	\param firePropIntensity : probabilities of propagation, depending on fire
	 * intensity
	 *	\param firePropLogis : logistic parameters for the baseline probability of
	 * fire spread
	 *  \param fireQuotaMax : maximum step / amount / cells before end of fire spreading
	 *	\param doDroughtDisturbances : unable or not Drought disturbances module
	 *	\param noDroughtSub : number of way a FG can react to a drought
	 * disturbance
	 *	\param chronoPost : when are applied post drought effects ("prev" or
	 * "post" succession)
	 *	\param chronoCurr : when are applied current drought effects ("prev" or
	 * "post" succession)
	 *	\param doAliensIntroduction : unable or not Aliens introduction module
	 *	\param freqAliens : the frequency of each aliens introduction
	 */
	GSP(const unsigned& seed,
  const int& noCPU,
	const int& noFG,
	const int& noStrata,
	const int& simulDuration,
	const int& seedingDuration,
	const int& seedingTimeStep,
	const int& seedingInput,
	const int& potentialFecundity,
	const bool& doSavingPFGStratum,
	const bool& doSavingPFG,
	const bool& doSavingStratum,
	const bool& doLightInteraction,
	const int& lightThreshLow,
	const int& lightThreshMedium,
	const bool& lightRecruitment,
	const bool& lightSaving,
	const bool& doHabSuitability,
	const int& habSuitMode,
	const bool& doDispersal,
	const int& dispersalMode,
	const bool& dispersalSaving,
	const bool& doDisturbances,
	const int& noDist,
	const int& noDistSub,
	const vector<int>& freqDist,
	const vector<double>& probDist,
	const vector<int>& pairDist,
	const bool& doSoilInteraction,
	const bool& soilFillMap,
	const double& soilInit,
	const double& soilRetention,
	const bool& soilRecruitment,
	const bool& soilSaving,
	const bool& doFireDisturbances,
	const int& noFireDist,
	const int& noFireDistSub,
	const vector<int>& freqFireDist,
	const int& fireIgnitMode,
	const int& fireNeighMode,
	const int& firePropMode,
	const int& fireQuotaMode,
	const vector<int>& fireIgnitNo,
	const vector<int>& fireIgnitNoHist,
	const int& fireIgnitFlammMax,
	const vector<double>& fireIgnitLogis,
	const vector<int>& fireNeighCC,
	const vector<double>& firePropIntensity,
	const vector<double>& firePropLogis,
	const int& fireQuotaMax,
	const bool& doDroughtDisturbances,
	const int& noDroughtSub,
	const string& chronoPost,
	const string& chronoCurr,
	const bool& doAliensIntroduction,
	const vector<int>& freqAliens);

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	GSP destructor
	 */
	~GSP();

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const GSP& o) const
	{
		return ( m_Seed == o.m_Seed &&
    m_NoCPU == o.m_NoCPU &&
		m_NoFG == o.m_NoFG &&
		m_NoStrata == o.m_NoStrata &&
		m_SimulDuration == o.m_SimulDuration &&
		m_SeedingDuration == o.m_SeedingDuration &&
		m_SeedingTimeStep == o.m_SeedingTimeStep &&
		m_SeedingInput == o.m_SeedingInput &&
		m_PotentialFecundity == o.m_PotentialFecundity &&
		m_DoSavingPFGStratum == o.m_DoSavingPFGStratum &&
		m_DoSavingPFG == o.m_DoSavingPFG &&
		m_DoSavingStratum == o.m_DoSavingStratum &&
		m_DoLightInteraction == o.m_DoLightInteraction &&
		m_LightThreshLow == o.m_LightThreshLow &&
		m_LightThreshMedium == o.m_LightThreshMedium &&
		m_LightRecruitment == o.m_LightRecruitment &&
		m_LightSaving == o.m_LightSaving &&
		m_DoHabSuitability == o.m_DoHabSuitability &&
		m_HabSuitMode == o.m_HabSuitMode &&
		m_DoDispersal == o.m_DoDispersal &&
		m_DispersalMode == o.m_DispersalMode &&
		m_DispersalSaving == o.m_DispersalSaving &&
		m_DoDisturbances == o.m_DoDisturbances &&
		m_NoDist == o.m_NoDist &&
		m_NoDistSub == o.m_NoDistSub &&
		m_FreqDist == o.m_FreqDist &&
		m_ProbDist == o.m_ProbDist &&
		m_PairDist == o.m_PairDist &&
		m_DoSoilInteraction == o.m_DoSoilInteraction &&
		m_SoilFillMap == o.m_SoilFillMap &&
		m_SoilInit == o.m_SoilInit &&
		m_SoilRetention == o.m_SoilRetention &&
		m_SoilRecruitment == o.m_SoilRecruitment &&
		m_SoilSaving == o.m_SoilSaving &&
		m_DoFireDisturbances == o.m_DoFireDisturbances &&
		m_NoFireDist == o.m_NoFireDist &&
		m_NoFireDistSub == o.m_NoFireDistSub &&
		m_FreqFireDist == o.m_FreqFireDist &&
		m_FireIgnitMode == o.m_FireIgnitMode &&
		m_FireNeighMode == o.m_FireNeighMode &&
		m_FirePropMode == o.m_FirePropMode &&
		m_FireQuotaMode == o.m_FireQuotaMode &&
		m_FireIgnitNo == o.m_FireIgnitNo &&
		m_FireIgnitNoHist == o.m_FireIgnitNoHist &&
		m_FireIgnitFlammMax == o.m_FireIgnitFlammMax &&
		m_FireIgnitLogis == o.m_FireIgnitLogis &&
		m_FireNeighCC == o.m_FireNeighCC &&
		m_FirePropIntensity == o.m_FirePropIntensity &&
		m_FirePropLogis == o.m_FirePropLogis &&
		m_FireQuotaMax == o.m_FireQuotaMax &&
		m_DoDroughtDisturbances == o.m_DoDroughtDisturbances &&
		m_NoDroughtSub == o.m_NoDroughtSub &&
		m_ChronoPost == o.m_ChronoPost &&
		m_ChronoCurr == o.m_ChronoCurr &&
		m_DoAliensIntroduction == o.m_DoAliensIntroduction &&
		m_FreqAliens == o.m_FreqAliens);
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	unsigned getSeed() const;
	int getNoCPU() const;
	int getNoFG() const;
	int getNoStrata() const;
	int getSimulDuration() const;
	int getSeedingDuration() const;
	int getSeedingTimeStep() const;
	int getSeedingInput() const;
	int getPotentialFecundity() const;
	bool getDoSavingPFGStratum() const;
	bool getDoSavingPFG() const;
	bool getDoSavingStratum() const;
	bool getDoLightInteraction() const;
	int getLightThreshLow() const;
	int getLightThreshMedium() const;
	bool getLightRecruitment() const;
	bool getLightSaving() const;
	bool getDoHabSuitability() const;
	int getHabSuitMode() const;
	bool getDoDispersal() const;
	int getDispersalMode() const;
	bool getDispersalSaving() const;
	bool getDoDisturbances() const;
	int getNoDist() const;
	int getNoDistSub() const;
	const vector<int>& getFreqDist() const;
	const vector<double>& getProbDist() const;
	const vector<int>& getPairDist() const;
	bool getDoSoilInteraction() const;
	bool getSoilFillMap() const;
	double getSoilInit() const;
	double getSoilRetention() const;
	bool getSoilRecruitment() const;
	bool getSoilSaving() const;
	bool getDoFireDisturbances() const;
	int getNoFireDist() const;
	int getNoFireDistSub() const;
	const vector<int>& getFreqFireDist() const;
	int getFireIgnitMode() const;
	int getFireNeighMode() const;
	int getFirePropMode() const;
	int getFireQuotaMode() const;
	const vector<int>& getFireIgnitNo() const;
	const vector<int>& getFireIgnitNoHist() const;
	const vector<int>& getFireNeighCC() const;
	const vector<double>& getFirePropIntensity() const;
	int getFireIgnitFlammMax() const;
	const vector<double>& getFireIgnitLogis() const;
	const vector<double>& getFirePropLogis() const;
	int getFireQuotaMax() const;
	bool getDoDroughtDisturbances() const;
	int getNoDroughtSub() const;
	const string& getChronoPost() const;
	const string& getChronoCurr() const;
	bool getDoAliensIntroduction() const;
	const vector<int>& getFreqAliens() const;

	void setSeed(const unsigned& seed);
	void setNoCPU(const int& noCPU);
	void setNoFG(const int& noFG);
	void setNoStrata(const int& noStrata);
	void setSimulDuration(const int& simulDuration);
	void setSeedingDuration(const int& seedingDuration);
	void setSeedingTimeStep(const int& seedingTimeStep);
	void setSeedingInput(const int& seedingInput);
	void setPotentialFecundity(const int& potentialFecundity);
	void setDoSavingPFGStratum(const bool& doSavingPFGStratum);
	void setDoSavingPFG(const bool& doSavingPFG);
	void setDoSavingStratum(const bool& doSavingStratum);
	void setDoLightInteraction(const bool& doLightInteraction);
	void setLightThreshLow(const int& lightThreshLow);
	void setLightThreshMedium(const int& lightThreshMedium);
	void setLightRecruitment(const bool& lightRecruitment);
	void setLightSaving(const bool& lightSaving);
	void setDoHabSuitability(const bool& doHabSuitability);
	void setHabSuitMode(const int& habSuitMode);
	void setDoDispersal(const bool& doDispersal);
	void setDispersalMode(const int& dispersalMode);
	void setDispersalSaving(const bool& dispersalSaving);
	void setDoDisturbances(const bool& doDisturbances);
	void setNoDist(const int& noDist);
	void setNoDistSub(const int& noDistSub);
	void setFreqDist(const vector<int>& freqDist);
	void setProbDist(const vector<double>& probDist);
	void setPairDist(const vector<int>& pairDist);
	void setDoSoilInteraction(const bool& doSoilInteraction);
	void setSoilFillMap(const bool& soilFillMap);
	void setSoilInit(const double& soilInit);
	void setSoilRetention(const double& soilRetention);
	void setSoilRecruitment(const bool& soilRecruitment);
	void setSoilSaving(const bool& soilSaving);
	void setDoFireDisturbances(const bool& doFireDisturbances);
	void setNoFireDist(const int& noFireDist);
	void setNoFireDistSub(const int& noFireDistSub);
	void setFreqFireDist(const vector<int>& freqFireDist);
	void setFireIgnitMode(const int& fireIgnitMode);
	void setFireNeighMode(const int& fireNeighMode);
	void setFirePropMode(const int& firePropMode);
	void setFireQuotaMode(const int& fireQuotaMode);
	void setFireIgnitNo(const vector<int>& fireIgnitNo);
	void setFireIgnitNoHist(const vector<int>& fireIgnitNoHist);
	void setFireIgnitFlammMax(const int& fireIgnitFlammMax);
	void setFireIgnitLogis(const vector<double>& fireIgnitLogis);
	void setFireNeighCC(const vector<int>& fireNeighCC);
	void setFirePropIntensity(const vector<double>& firePropIntensity);
	void setFirePropLogis(const vector<double>& firePropLogis);
	void setFireQuotaMax(const int& fireQuotaMax);
	void setDoDroughtDisturbances(const bool& doDroughtDisturbances);
	void setNoDroughtSub(const int& noDroughtSub);
	void setChronoPost(const string& chronoPost);
	void setChronoCurr(const string& chronoCurr);
	void setDoAliensIntroduction(const bool& doAliensIntroduction);
	void setFreqAliens(const vector<int>& freqAliens);

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	void show();

};

BOOST_CLASS_VERSION(GSP, 0)
#endif //GSP_H
