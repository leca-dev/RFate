/*============================================================================*/
/*                     Global Simulation Parameters Class                     */
/*============================================================================*/

/*!
 * \file GSP.h
 * \brief Global Simulation Parameters Class
 * \author Damien Georges
 * \version 1.0
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

	int m_NoCPU; /*!< Number of CPU for parallel computing */
	int m_NoFG; /*!< Number of PFG studied */
	int m_NoStrata; /*!< Number of height strata */
	int m_SimulDuration; /*!< Simulation duration */
	int m_SeedingDuration; /*!< Seeding duration */
	int m_SeedingTimeStep; /*!< Seeding time step */
	int m_SeedingInput; /*!< Number of seeds introduced during seeding */
	int m_MaxAbundLow; /*!< Maximum abundance or space a PFG can occupy : low value */
	int m_MaxAbundMedium; /*!< Maximum abundance or space a PFG can occupy : medium value */
	int m_MaxAbundHigh; /*!< Maximum abundance or space a PFG can occupy : high value */

	/* Light competition module */
	bool m_DoLightCompetition; /*!< Unable or not Light competition module */
	int m_LightThreshLow; /*!< Threshold to transform PFG abundances into Low light resources */
	int m_LightThreshMedium; /*!< Threshold to transform PFG abundances into Medium light resources */

	/* Habitat suitability module */
	bool m_DoHabSuitability; /*!< Unable or not habitat suitability module */
	int m_HabSuitMode; /*!< Option to draw the habitat suitability ref */

	/* Dispersal module */
	bool m_DoDispersal; /*!< Unable or not dispersal module */
	int m_DispersalMode; /*!< Option to disperse the seeds */

	/* Disturbances module */
	bool m_DoDisturbances; /*!< Unable or not disturbance module */
	int m_NoDist; /*!< Number of disturbances */
	int m_NoDistSub; /*!< Number of disturbances subdivision (no of way to react to dist) */
	vector<int> m_FreqDist; /*!< Frequency of each disturbance in years */

	/* Soil competition module */
	bool m_DoSoilCompetition; /*!< Unable or not Soil competition module */
	double m_SoilInit; /*!< Initialization soil value */
	double m_SoilRetention; /*!< Percentage of soil from previous year to keep */

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
		ar & m_NoCPU;
		ar & m_NoFG;
		ar & m_NoStrata;
		ar & m_SimulDuration;
		ar & m_SeedingDuration;
		ar & m_SeedingTimeStep;
		ar & m_SeedingInput;
		ar & m_MaxAbundLow;
		ar & m_MaxAbundMedium;
		ar & m_MaxAbundHigh;
		ar & m_DoLightCompetition;
		ar & m_LightThreshLow;
		ar & m_LightThreshMedium;
		ar & m_DoHabSuitability;
		ar & m_HabSuitMode;
		ar & m_DoDispersal;
		ar & m_DispersalMode;
		ar & m_DoDisturbances;
		ar & m_NoDist;
		ar & m_NoDistSub;
		ar & m_FreqDist;
		ar & m_DoSoilCompetition;
		ar & m_SoilInit;
		ar & m_SoilRetention;
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
	 *	\param noCPU : number of CPUs required for simulation
	 *	\param noFG : number of Functional Groups involved
	 *	\param noStrata : number of height strata
	 *	\param simulDuration : simulation duration (in years)
	 *	\param seedingDuration : seeding duration (in years)
	 *	\param seedingTimeStep : seeding time step (in years)
	 *	\param seedingInput : number of seeds dispersed each year during seeding
	 *	\param maxAbundLow : maximum abundance or space a PFG can occupy : low
	 * value
	 *	\param maxAbundMedium : maximum abundance or space a PFG can occupy :
	 * medium value
	 *	\param maxAbundHigh : maximum abundance or space a PFG can occupy :
	 * high value
	 *	\param doLightCompetition : unable or not Light competition module
	 *	\param lightThreshLow : threshold to transform PFG abundances into Low
	 * light resources
	 *	\param lightThreshMedium : threshold to transform PFG abundances into
	 * Medium light resources
	 *	\param doHabSuitability : unable or not Habitat suitability module
	 *	\param habSuitMode : option to draw the habitat suitability ref
	 *	\param doDispersal : unable or not Dispersal module
	 *  \param dispersalMode : option to disperse the seeds
	 *	\param doDisturbances : unable or not Disturbances module
	 *	\param noDist : number of disturbances involved
	 *	\param noDistSub : number of way a FG can react to a disturbance
	 *  \param freqDist : the frequency of each disturbance
	 *	\param doSoilCompetition : unable or not Soil competition module
	 *	\param soilInit : initialization soil value
	 *	\param soilRetention : percentage of soil from previous year to keep
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
	GSP(const int& noCPU,
	const int& noFG,
	const int& noStrata,
	const int& simulDuration,
	const int& seedingDuration,
	const int& seedingTimeStep,
	const int& seedingInput,
	const int& maxAbundLow,
	const int& maxAbundMedium,
	const int& maxAbundHigh,
	const bool& doLightCompetition,
	const int& lightThreshLow,
	const int& lightThreshMedium,
	const bool& doHabSuitability,
	const int& habSuitMode,
	const bool& doDispersal,
	const int& dispersalMode,
	const bool& doDisturbances,
	const int& noDist,
	const int& noDistSub,
	const vector<int>& freqDist,
	const bool& doSoilCompetition,
	const double& soilInit,
	const double& soilRetention,
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
		return ( m_NoCPU == o.m_NoCPU &&
		m_NoFG == o.m_NoFG &&
		m_NoStrata == o.m_NoStrata &&
		m_SimulDuration == o.m_SimulDuration &&
		m_SeedingDuration == o.m_SeedingDuration &&
		m_SeedingTimeStep == o.m_SeedingTimeStep &&
		m_SeedingInput == o.m_SeedingInput &&
		m_MaxAbundLow == o.m_MaxAbundLow &&
		m_MaxAbundMedium == o.m_MaxAbundMedium &&
		m_MaxAbundHigh == o.m_MaxAbundHigh &&
		m_DoLightCompetition == o.m_DoLightCompetition &&
		m_LightThreshLow == o.m_LightThreshLow &&
		m_LightThreshMedium == o.m_LightThreshMedium &&
		m_DoHabSuitability == o.m_DoHabSuitability &&
		m_HabSuitMode == o.m_HabSuitMode &&
		m_DoDispersal == o.m_DoDispersal &&
		m_DispersalMode == o.m_DispersalMode &&
		m_DoDisturbances == o.m_DoDisturbances &&
		m_NoDist == o.m_NoDist &&
		m_NoDistSub == o.m_NoDistSub &&
		m_FreqDist == o.m_FreqDist &&
		m_DoSoilCompetition == o.m_DoSoilCompetition &&
		m_SoilInit == o.m_SoilInit &&
		m_SoilRetention == o.m_SoilRetention &&
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

	const int& getNoCPU() const;
	const int& getNoFG() const;
	const int& getNoStrata() const;
	const int& getSimulDuration() const;
	const int& getSeedingDuration() const;
	const int& getSeedingTimeStep() const;
	const int& getSeedingInput() const;
	const int& getMaxAbundLow() const;
	const int& getMaxAbundMedium() const;
	const int& getMaxAbundHigh() const;
	const bool& getDoLightCompetition() const;
	const int& getLightThreshLow() const;
	const int& getLightThreshMedium() const;
	const bool& getDoHabSuitability() const;
	const int& getHabSuitMode() const;
	const bool& getDoDispersal() const;
	const int& getDispersalMode() const;
	const bool& getDoDisturbances() const;
	const int& getNoDist() const;
	const int& getNoDistSub() const;
	const vector<int>& getFreqDist() const;
	const bool& getDoSoilCompetition() const;
	const double& getSoilInit() const;
	const double& getSoilRetention() const;
	const bool& getDoFireDisturbances() const;
	const int& getNoFireDist() const;
	const int& getNoFireDistSub() const;
	const vector<int>& getFreqFireDist() const;
	const int& getFireIgnitMode() const;
	const int& getFireNeighMode() const;
	const int& getFirePropMode() const;
	const int& getFireQuotaMode() const;
	const vector<int>& getFireIgnitNo() const;
	const vector<int>& getFireIgnitNoHist() const;
	const vector<int>& getFireNeighCC() const;
	const vector<double>& getFirePropIntensity() const;
	const int& getFireIgnitFlammMax() const;
	const vector<double>& getFireIgnitLogis() const;
	const vector<double>& getFirePropLogis() const;
	const int& getFireQuotaMax() const;
	const bool& getDoDroughtDisturbances() const;
	const int& getNoDroughtSub() const;
	const string& getChronoPost() const;
	const string& getChronoCurr() const;
	const bool& getDoAliensIntroduction() const;
	const vector<int>& getFreqAliens() const;

	void setNoCPU(const int& noCPU);
	void setNoFG(const int& noFG);
	void setNoStrata(const int& noStrata);
	void setSimulDuration(const int& simulDuration);
	void setSeedingDuration(const int& seedingDuration);
	void setSeedingTimeStep(const int& seedingTimeStep);
	void setSeedingInput(const int& seedingInput);
	void setMaxAbundLow(const int& maxAbundLow);
	void setMaxAbundMedium(const int& maxAbundMedium);
	void setMaxAbundHigh(const int& maxAbundHigh);
	void setDoLightCompetition(const bool& doLightCompetition);
	void setLightThreshLow(const int& lightThreshLow);
	void setLightThreshMedium(const int& lightThreshMedium);
	void setDoHabSuitability(const bool& doHabSuitability);
	void setHabSuitMode(const int& habSuitMode);
	void setDoDispersal(const bool& doDispersal);
	void setDispersalMode(const int& dispersalMode);
	void setDoDisturbances(const bool& doDisturbances);
	void setNoDist(const int& noDist);
	void setNoDistSub(const int& noDistSub);
	void setFreqDist(const vector<int>& freqDist);
	void setDoSoilCompetition(const bool& doSoilCompetition);
	void setSoilInit(const double& soilInit);
	void setSoilRetention(const double& soilRetention);
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

	/*!
	 *	\brief Convert Abundance classes to integer
	 *
	 *	This functions converts a value from enum Abund to an integer :
	 *   - ANone -> 0
	 *   - ALow -> m_MaxAbundLow
	 *   - AMedium -> m_MaxAbundMedium
	 *   - AHigh -> m_MaxAbundHigh
	 *
	 * \param abund : a value from enum Abund (ANone, ALow, AMedium, AHigh)
	 *	\return : an integer corresponding to a value from enum Abund
	 */
	int AbundToInt(Abund abund);

};

BOOST_CLASS_VERSION(GSP, 0)
#endif //GSP_H
