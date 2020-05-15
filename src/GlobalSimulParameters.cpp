#include <cmath>
#include <iostream>
#include <cmath>
#include <cstring>
#include <fstream>
#include <sstream>
#include <cstdio>
#include <vector>
#include <numeric>
#include <sys/stat.h>
#include <sys/types.h>
#include <cstdlib>
#include <ctime>
#include <omp.h>

#include "GlobalSimulParameters.h"

using namespace std;

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

GSP::GSP() : m_NoCPU(1), m_NoFG(0), m_NoStrata(0), m_SimulDuration(0),
m_SeedingDuration(0), m_SeedingTimeStep(0), m_SeedingInput(0),
m_MaxAbundLow(0), m_MaxAbundMedium(0), m_MaxAbundHigh(0),
m_DoLightCompetition(false), m_LightThreshLow(0), m_LightThreshMedium(0),
m_DoHabSuitability(false), m_HabSuitMode(1),
m_DoDispersal(false), m_DispersalMode(1),
m_DoDisturbances(false), m_NoDist(0), m_NoDistSub(0), m_FreqDist(0,0),
m_DoSoilCompetition(false), m_SoilInit(0.0), m_SoilRetention(0.0),
m_DoFireDisturbances(false), m_NoFireDist(0), m_NoFireDistSub(0), m_FreqFireDist(0,0),
m_FireIgnitMode(1), m_FireNeighMode(1), m_FirePropMode(1), m_FireQuotaMode(1),
m_FireIgnitNo(0,0), m_FireIgnitNoHist(0), m_FireIgnitFlammMax(0), m_FireIgnitLogis(0,0),
m_FireNeighCC(0,0), m_FirePropIntensity(0,0), m_FirePropLogis(0,0), m_FireQuotaMax(0),
m_DoDroughtDisturbances(false), m_NoDroughtSub(0), m_ChronoPost("prev"), m_ChronoCurr("post"),
m_DoAliensIntroduction(false), m_FreqAliens(0,0)
{
	/* Nothing to do */
}

GSP::GSP(const int& noCPU, const int& noFG, const int& noStrata, const int& simulDuration,
const int& seedingDuration, const int& seedingTimeStep, const int& seedingInput,
const int& maxAbundLow, const int& maxAbundMedium, const int& maxAbundHigh,
const bool& doLightCompetition, const int& lightThreshLow, const int& lightThreshMedium,
const bool& doHabSuitability, const int& habSuitMode,
const bool& doDispersal, const int& dispersalMode,
const bool& doDisturbances, const int& noDist, const int& noDistSub, const vector<int>& freqDist,
const bool& doSoilCompetition, const double& soilInit, const double& soilRetention,
const bool& doFireDisturbances, const int& noFireDist, const int& noFireDistSub, const vector<int>& freqFireDist,
const int& fireIgnitMode, const int& fireNeighMode, const int& firePropMode, const int& fireQuotaMode,
const vector<int>& fireIgnitNo, const vector<int>& fireIgnitNoHist,
const int& fireIgnitFlammMax, const vector<double>& fireIgnitLogis, 
const vector<int>& fireNeighCC, const vector<double>& firePropIntensity,
const vector<double>& firePropLogis, const int& fireQuotaMax,
const bool& doDroughtDisturbances, const int& noDroughtSub, const string& chronoPost, const string& chronoCurr,
const bool& doAliensIntroduction, const vector<int>& freqAliens) : m_NoFG(noFG), m_NoStrata(noStrata),
m_SimulDuration(simulDuration),
m_SeedingDuration(seedingDuration), m_SeedingTimeStep(seedingTimeStep),
m_MaxAbundLow(maxAbundLow), m_MaxAbundMedium(maxAbundMedium), m_MaxAbundHigh(maxAbundHigh),
m_DoLightCompetition(doLightCompetition), m_LightThreshLow(lightThreshLow), m_LightThreshMedium(lightThreshMedium),
m_DoHabSuitability(doHabSuitability), m_HabSuitMode(habSuitMode),
m_DoDispersal(doDispersal), m_DispersalMode(dispersalMode),
m_DoDisturbances(doDisturbances), m_NoDist(noDist), m_NoDistSub(noDistSub), m_FreqDist(freqDist),
m_DoSoilCompetition(doSoilCompetition), m_SoilInit(soilInit), m_SoilRetention(soilRetention),
m_DoFireDisturbances(doFireDisturbances), m_NoFireDist(noFireDist), m_NoFireDistSub(noFireDistSub), m_FreqFireDist(freqFireDist),
m_FireIgnitMode(fireIgnitMode), m_FireNeighMode(fireNeighMode), m_FirePropMode(firePropMode), m_FireQuotaMode(fireQuotaMode),
m_FireIgnitNo(fireIgnitNo), m_FireIgnitNoHist(fireIgnitNoHist), m_FireIgnitFlammMax(fireIgnitFlammMax), m_FireIgnitLogis(fireIgnitLogis), 
m_FireNeighCC(fireNeighCC), m_FirePropIntensity(firePropIntensity), m_FirePropLogis(firePropLogis), m_FireQuotaMax(fireQuotaMax),
m_DoDroughtDisturbances(doDroughtDisturbances), m_NoDroughtSub(noDroughtSub), m_ChronoPost(chronoPost), m_ChronoCurr(chronoCurr),
m_DoAliensIntroduction(doAliensIntroduction), m_FreqAliens(freqAliens)
{
	/* check if the number of cores required is compatible with computer arch */
	if (noCPU > 1)
	{
		/* get OMP_NUM_THREADS environment variable :
		   maximum number of threads that can be used to form a new team
		   if a parallel region without a num_threads clause is encountered */
		int threads_number = omp_get_max_threads();
		if (noCPU <= threads_number)
		{
			m_NoCPU = noCPU;
		} else
		{
			m_NoCPU = threads_number;
			cout << "\nInitially required number of cores is too high. It was automatically set-up to " << m_NoCPU << endl;
		}
	} else
	{
		m_NoCPU = 1;
	}
}

GSP::GSP(const string globalParamsFile)
{
	/* 1. check parameter file existence */
	testFileExist("--GLOBAL_PARAMS--", globalParamsFile, false);
	
	/* 2. read global parameter file */
	par::Params GlobParms(globalParamsFile.c_str(), " = \"", "#");
	
	/* 3. fill global simulation attributes given parameters */
	
	/* GET OPTIONAL number of cores */
	vector<int> v_int = GlobParms.get_val<int>("NO_CPU", true);
	if (v_int.size()) m_NoCPU = v_int[0]; else m_NoCPU = 1;
	
	/* check if the number of cores required is compatible with computer arch */
	if (m_NoCPU > 1)
	{
		/* get OMP_NUM_THREADS environment variable :
		   maximum number of threads that can be used to form a new team
		   if a parallel region without a num_threads clause is encountered */
		int threads_number = omp_get_max_threads();
		if (m_NoCPU > threads_number)
		{
			m_NoCPU = threads_number;
			cout << "\nInitially required number of cores is too high. It was automatically set-up to " << m_NoCPU << endl;
		}
	} else
	{
		m_NoCPU = 1;
	}
	
	/* GET BASIC REQUIRED parameters*/
	m_NoFG = GlobParms.get_val<int>("NO_PFG", false, "!!! Parameter NO_PFG : must be equal to the number of Plant Functional Groups!")[0];
	if (m_NoFG <= 0)
	{
		cerr << "!!! Parameter NO_PFG : must be superior to 0!" << endl;
		terminate();
	}
	m_NoStrata = GlobParms.get_val<int>("NO_STRATA", false, "!!! Parameter NO_STRATA : must be equal to the number of height strata!")[0];
	if (m_NoStrata <= 0)
	{
		cerr << "!!! Parameter NO_STRATA : must be superior to 0!" << endl;
		terminate();
	}
	m_SimulDuration = GlobParms.get_val<int>("SIMULATION_DURATION", false, "!!! Parameter SIMULATION_DURATION : must be superior to 0!")[0];
	if (m_SimulDuration <= 0)
	{
		cerr << "!!! Parameter SIMULATION_DURATION : must be superior to 0!" << endl;
		terminate();
	}
	m_MaxAbundLow = GlobParms.get_val<int>("MAX_ABUND_LOW")[0];
	m_MaxAbundMedium = GlobParms.get_val<int>("MAX_ABUND_MEDIUM")[0];
	m_MaxAbundHigh = GlobParms.get_val<int>("MAX_ABUND_HIGH")[0];
	if (m_MaxAbundLow > m_MaxAbundMedium)
	{
		cerr << "!!! Parameter MAX_ABUND_LOW : must be inferior to MAX_ABUND_MEDIUM!" << endl;
		terminate();
	}
	if (m_MaxAbundMedium > m_MaxAbundHigh)
	{
		cerr << "!!! Parameter MAX_ABUND_MEDIUM : must be inferior to MAX_ABUND_HIGH!" << endl;
		terminate();
	}
	
	/* GET OPTIONAL parameters : seeding */
	v_int = GlobParms.get_val<int>("SEEDING_DURATION", true);
	if (v_int.size()) m_SeedingDuration = v_int[0]; else m_SeedingDuration = 0;
	v_int = GlobParms.get_val<int>("SEEDING_TIMESTEP", true);
	if (v_int.size()) m_SeedingTimeStep = v_int[0]; else m_SeedingTimeStep = 0;
	if (m_SeedingDuration > 0 && m_SeedingTimeStep <= 0)
	{
		cerr << "!!! Parameter SEEDING_TIMESTEP : must be superior to 0!" << endl;
		terminate();
	}
	v_int = GlobParms.get_val<int>("SEEDING_INPUT", true);
	if (v_int.size()) m_SeedingInput = v_int[0]; else m_SeedingInput = 100;
	if (m_SeedingDuration > 0 && m_SeedingInput <= 0)
	{
		cerr << "!!! Parameter SEEDING_INPUT : must be superior to 0!" << endl;
		terminate();
	}

	/* GET OPTIONAL parameters : light competition */
	v_int = GlobParms.get_val<int>("DO_LIGHT_COMPETITION", true);
	if (v_int.size()) m_DoLightCompetition = bool(v_int[0]); else m_DoLightCompetition = false;
	if (m_DoLightCompetition)
	{
		m_LightThreshLow = GlobParms.get_val<int>("LIGHT_THRESH_LOW")[0];
		m_LightThreshMedium = GlobParms.get_val<int>("LIGHT_THRESH_MEDIUM")[0];
		if (m_LightThreshLow < m_LightThreshMedium)
		{
			cerr << "!!! Parameter LIGHT_THRESH_LOW : must be superior to LIGHT_THRESH_MEDIUM!" << endl;
			terminate();
		}
	} else
	{
		m_LightThreshLow = 0;
		m_LightThreshMedium = 0;
	}
	
	/* GET OPTIONAL parameters : habitat suitability */
	v_int = GlobParms.get_val<int>("DO_HAB_SUITABILITY", true);
	if (v_int.size()) m_DoHabSuitability = bool(v_int[0]); else m_DoHabSuitability= false;
	if (m_DoHabSuitability)
	{
		m_HabSuitMode = GlobParms.get_val<int>("HABSUIT_MODE")[0];
		if (m_HabSuitMode != 1 && m_HabSuitMode != 2)
		{
			cerr << "!!! Parameter HABSUIT_MODE : must be either equal to 1 (one random number per pixel) or 2 (one distribution per PFG)!" << endl;
			terminate();
		}
	} else
	{
		m_HabSuitMode = 1;
	}
	
	/* GET OPTIONAL parameters : dispersal */
	v_int = GlobParms.get_val<int>("DO_DISPERSAL", true);
	if (v_int.size()) m_DoDispersal = bool(v_int[0]); else m_DoDispersal= false;
	v_int = GlobParms.get_val<int>("DISPERSAL_MODE", true);
	if (v_int.size()) m_DispersalMode = v_int[0]; else m_DispersalMode = 1;
	if (m_DispersalMode != 1 && m_DispersalMode != 2 && m_DispersalMode != 3)
	{
		cerr << "!!! DISPERSAL MODE should be either 1 (uniform), 2 (exponential kernel) or 3 (exponential kernel + probability). Please check!" << endl;
		terminate();
	}
	
	/* GET OPTIONAL parameters : disturbances */
	v_int = GlobParms.get_val<int>("DO_DISTURBANCES", true);
	if (v_int.size()) m_DoDisturbances = bool(v_int[0]); else m_DoDisturbances= false;
	if (m_DoDisturbances)
	{
		m_NoDist = GlobParms.get_val<int>("DIST_NO")[0];
		m_NoDistSub = GlobParms.get_val<int>("DIST_NOSUB")[0];
		if (m_NoDist <= 0 || m_NoDistSub <= 0)
		{
			cerr << "!!! Parameter DIST_NO and DIST_NOSUB : must be superior to 0!" << endl;
			terminate();
		}
		
		m_FreqDist = GlobParms.get_val<int>("DIST_FREQ");
		if (m_NoDist != m_FreqDist.size())
		{
			cerr << "!!! Parameter DIST_FREQ : number of frequencies must be equal to the number of disturbances (DIST_NO)!" << endl;
			terminate();
		}
	} else
	{
		m_NoDist = 0;
		m_NoDistSub = 0;
		m_FreqDist = vector<int>(1,0);
	}
	
	/* GET OPTIONAL parameters : soil competition */
	v_int = GlobParms.get_val<int>("DO_SOIL_COMPETITION", true);
	if (v_int.size()) m_DoSoilCompetition = bool(v_int[0]); else m_DoSoilCompetition = false;
	if (m_DoSoilCompetition)
	{
		m_SoilInit = GlobParms.get_val<double>("SOIL_INIT")[0];
		m_SoilRetention = GlobParms.get_val<double>("SOIL_RETENTION")[0];
		if (m_SoilRetention < 0.0)
		{
			cerr << "!!! Parameter SOIL_RETENTION : must be superior or equal to 0!" << endl;
			terminate();
		}
		if (m_SoilRetention > 1.0)
		{
			cerr << "!!! Parameter SOIL_RETENTION : must be inferior or equal to 1!" << endl;
			terminate();
		}
	} else
	{
		m_SoilInit = 0.0;
		m_SoilRetention = 0.0;
	}
	
	/* GET OPTIONAL parameters : fire disturbances */
	v_int = GlobParms.get_val<int>("DO_FIRE_DISTURBANCE", true);
	if (v_int.size()) m_DoFireDisturbances = bool(v_int[0]); else m_DoFireDisturbances= false;
	if (m_DoFireDisturbances)
	{
		v_int = GlobParms.get_val<int>("FIRE_NO",true);
		if (v_int.size()) m_NoFireDist = v_int[0]; else m_NoFireDist = 0;
		v_int = GlobParms.get_val<int>("FIRE_NOSUB",true);
		if (v_int.size()) m_NoFireDistSub = v_int[0]; else m_NoFireDistSub = 0;
		v_int = GlobParms.get_val<int>("FIRE_FREQ",true);
		if (v_int.size()) m_FreqFireDist = v_int; else m_FreqFireDist = vector<int>(1,0);
		if (m_NoFireDist != m_FreqFireDist.size())
		{
			cerr << "!!! Parameter FIRE_FREQ : number of frequencies must be equal to the number of fire disturbances (FIRE_NO)!" << endl;
			terminate();
		}
		v_int = GlobParms.get_val<int>("FIRE_IGNIT_MODE", true);
		if (v_int.size()) m_FireIgnitMode = v_int[0]; else m_FireIgnitMode = 0;
		v_int = GlobParms.get_val<int>("FIRE_NEIGH_MODE", true);
		if (v_int.size()) m_FireNeighMode = v_int[0]; else m_FireNeighMode = 0;
		v_int = GlobParms.get_val<int>("FIRE_PROP_MODE", true);
		if (v_int.size()) m_FirePropMode = v_int[0]; else m_FirePropMode = 0;
		v_int = GlobParms.get_val<int>("FIRE_QUOTA_MODE", true);
		if (v_int.size()) m_FireQuotaMode = v_int[0]; else m_FireQuotaMode = 0;
		if ((m_FireNeighMode == 2 || m_FireNeighMode == 3) && (m_FireIgnitMode == 4 || m_FireIgnitMode == 5))
		{
			cerr << "!!! FIRE options : Cookie-cutter module : ignition option is wrong!" << endl;
			terminate();
		}
		if (m_FireIgnitMode == 5 && m_FirePropMode != 4)
		{
			cerr << "!!! FIRE options : Chao Li module : propagation option is wrong!" << endl;
			terminate();
		}
		if (m_FireQuotaMode == 2 && m_FirePropMode != 2)
		{
			cerr << "!!! FIRE options : probability module (based on current cell) : propagation option is wrong!" << endl;
			terminate();
		}
		v_int = GlobParms.get_val<int>("FIRE_IGNIT_NO",true);
		if (v_int.size()) m_FireIgnitNo = v_int; else m_FireIgnitNo = vector<int>(1,0);
		v_int = GlobParms.get_val<int>("FIRE_IGNIT_NOHIST",true);
		if (v_int.size()) m_FireIgnitNoHist = v_int; else m_FireIgnitNoHist = vector<int>(1,0);
		v_int = GlobParms.get_val<int>("FIRE_IGNIT_FLAMMMAX", true);
		if (v_int.size()) m_FireIgnitFlammMax = v_int[0]; else m_FireIgnitFlammMax = 0;
		vector<double> v_double = GlobParms.get_val<double>("FIRE_IGNIT_LOGIS",true);
		if (v_int.size()) m_FireIgnitLogis = v_double; else m_FireIgnitLogis = vector<double>(1.0,0);
		v_int = GlobParms.get_val<int>("FIRE_NEIGH_CC",true);
		if (v_int.size()) m_FireNeighCC = v_int; else m_FireNeighCC = vector<int>(1,0);
		if ((m_FireNeighMode == 2 || m_FireNeighMode == 3) && m_FireNeighCC.size() == 0)
		{
			cerr << "!!! FIRE options : Cookie-cutter module : must give a Cookie-Cutter extent!" << endl;
			terminate();
		}
		v_double = GlobParms.get_val<double>("FIRE_PROP_INTENSITY",true);
		if (v_double.size()) m_FirePropIntensity = v_double; else m_FirePropIntensity = vector<double>(1.0,0);
		v_double = GlobParms.get_val<double>("FIRE_PROP_LOGIS",true);
		if (v_int.size()) m_FirePropLogis = v_double; else m_FirePropLogis = vector<double>(1.0,0);
		v_int = GlobParms.get_val<int>("FIRE_QUOTA_MAX", true);
		if (v_int.size()) m_FireQuotaMax = v_int[0]; else m_FireQuotaMax = 0;
	} else
	{
		m_NoFireDist = 0;
		m_NoFireDistSub = 0;
		m_FreqFireDist = vector<int>(1,0);
		m_FireIgnitMode = 0;
		m_FireNeighMode = 0;
		m_FirePropMode = 0;
		m_FireQuotaMode = 0;
		m_FireIgnitNo = vector<int>(1,0);
		m_FireIgnitNoHist = vector<int>(1,0);
		m_FireIgnitFlammMax = 0;
		m_FireIgnitLogis = vector<double>(1.0,0);
		m_FireNeighCC = vector<int>(1,0);
		m_FirePropIntensity = vector<double>(1.0,0);
		m_FirePropLogis = vector<double>(1.0,0);
		m_FireQuotaMax = 0;
	}
	
	/* GET OPTIONAL parameters : drought disturbances */
	v_int = GlobParms.get_val<int>("DO_DROUGHT_DISTURBANCES", true);
	if (v_int.size()) m_DoDroughtDisturbances = bool(v_int[0]); else m_DoDroughtDisturbances= false;
	if (m_DoDroughtDisturbances)
	{
		m_NoDroughtSub = GlobParms.get_val<int>("DROUGHT_NOSUB")[0];
		v_int = GlobParms.get_val<int>("CHRONO_POST_DROUGHT",true);
		if (v_int.size())
		{
			if(v_int[0]==0) m_ChronoPost = "prev"; else m_ChronoPost = "post";
		} else
		{
			m_ChronoPost = "prev";
		}
		v_int = GlobParms.get_val<int>("CHRONO_CURR_DROUGHT",true);
		if (v_int.size())
		{
			if (v_int[0]==0) m_ChronoCurr = "prev"; else m_ChronoCurr = "post";
		} else
		{
			m_ChronoCurr = "post";
		}
	} else
	{
		m_NoDroughtSub = 0;
		m_ChronoPost = "prev";
		m_ChronoCurr = "post";
	}
	
	/* GET OPTIONAL parameters : aliens introduction */
	v_int = GlobParms.get_val<int>("DO_ALIENS_INTRODUCTION", true);
	if (v_int.size()) m_DoAliensIntroduction = bool(v_int[0]); else m_DoAliensIntroduction= false;
	if (m_DoAliensIntroduction)
	{
		m_FreqAliens = GlobParms.get_val<int>("ALIENS_FREQ");
		if (m_NoFG != m_FreqAliens.size())
		{
			cerr << "!!! Parameter ALIENS_FREQ : number of frequencies must be equal to the number of PFG (NO_PFG)!" << endl;
			terminate();
		}
	} else
	{
		m_FreqAliens = vector<int>(1,0);
	}
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

GSP::~GSP()
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

const int& GSP::getNoCPU() const{ return m_NoCPU; }
const int& GSP::getNoFG() const{ return m_NoFG; }
const int& GSP::getNoStrata() const{ return m_NoStrata; }
const int& GSP::getSimulDuration() const{ return m_SimulDuration; }
const int& GSP::getSeedingDuration() const{ return m_SeedingDuration; }
const int& GSP::getSeedingTimeStep() const{ return m_SeedingTimeStep; }
const int& GSP::getSeedingInput() const{ return m_SeedingInput; }
const int& GSP::getMaxAbundLow() const{ return m_MaxAbundLow; }
const int& GSP::getMaxAbundMedium() const{ return m_MaxAbundMedium; }
const int& GSP::getMaxAbundHigh() const{ return m_MaxAbundHigh; }
const bool& GSP::getDoLightCompetition() const{ return m_DoLightCompetition; }
const int& GSP::getLightThreshLow() const{ return m_LightThreshLow; }
const int& GSP::getLightThreshMedium() const{ return m_LightThreshMedium; }
const bool& GSP::getDoHabSuitability() const{ return m_DoHabSuitability; }
const int& GSP::getHabSuitMode() const{ return m_HabSuitMode; }
const bool& GSP::getDoDispersal() const{ return m_DoDispersal; }
const int& GSP::getDispersalMode() const{ return m_DispersalMode; }
const bool& GSP::getDoDisturbances() const{ return m_DoDisturbances; }
const int& GSP::getNoDist() const{ return m_NoDist; }
const int& GSP::getNoDistSub() const{ return m_NoDistSub; }
const vector<int>& GSP::getFreqDist() const{ return m_FreqDist; }
const bool& GSP::getDoSoilCompetition() const{ return m_DoSoilCompetition; }
const double& GSP::getSoilInit() const{ return m_SoilInit; }
const double& GSP::getSoilRetention() const{ return m_SoilRetention; }
const bool& GSP::getDoFireDisturbances() const{ return m_DoFireDisturbances; }
const int& GSP::getNoFireDist() const{ return m_NoFireDist; }
const int& GSP::getNoFireDistSub() const{ return m_NoFireDistSub; }
const vector<int>& GSP::getFreqFireDist() const{ return m_FreqFireDist; }
const int& GSP::getFireIgnitMode() const{ return m_FireIgnitMode; }
const int& GSP::getFireNeighMode() const{ return m_FireNeighMode; }
const int& GSP::getFirePropMode() const{ return m_FirePropMode; }
const int& GSP::getFireQuotaMode() const{ return m_FireQuotaMode; }
const vector<int>& GSP::getFireIgnitNo() const{ return m_FireIgnitNo; }
const vector<int>& GSP::getFireIgnitNoHist() const{ return m_FireIgnitNoHist; }
const int& GSP::getFireIgnitFlammMax() const{ return m_FireIgnitFlammMax; }
const vector<double>& GSP::getFireIgnitLogis() const{ return m_FireIgnitLogis; }
const vector<int>& GSP::getFireNeighCC() const{ return m_FireNeighCC; }
const vector<double>& GSP::getFirePropIntensity() const{ return m_FirePropIntensity; }
const vector<double>& GSP::getFirePropLogis() const{ return m_FirePropLogis; }
const int& GSP::getFireQuotaMax() const{ return m_FireQuotaMax; }
const bool& GSP::getDoDroughtDisturbances() const{ return m_DoDroughtDisturbances; }
const int& GSP::getNoDroughtSub() const{ return m_NoDroughtSub; }
const string& GSP::getChronoPost() const{ return m_ChronoPost; }
const string& GSP::getChronoCurr() const{ return m_ChronoCurr; }
const bool& GSP::getDoAliensIntroduction() const{ return m_DoAliensIntroduction; }
const vector<int>& GSP::getFreqAliens() const{ return m_FreqAliens; }

void GSP::setNoCPU(const int& noCPU){ m_NoCPU = noCPU; }
void GSP::setNoFG(const int& noFG){ m_NoFG = noFG; }
void GSP::setNoStrata(const int& noStrata){ m_NoStrata = noStrata; }
void GSP::setSimulDuration(const int& simulDuration){ m_SimulDuration = simulDuration; }
void GSP::setSeedingDuration(const int& seedingDuration){ m_SeedingDuration = seedingDuration; }
void GSP::setSeedingTimeStep(const int& seedingTimeStep){ m_SeedingTimeStep = seedingTimeStep; }
void GSP::setSeedingInput(const int& seedingInput){ m_SeedingInput = seedingInput; }
void GSP::setMaxAbundLow(const int& maxAbundLow){ m_MaxAbundLow = maxAbundLow; }
void GSP::setMaxAbundMedium(const int& maxAbundMedium){ m_MaxAbundMedium = maxAbundMedium; }
void GSP::setMaxAbundHigh(const int& maxAbundHigh){ m_MaxAbundHigh = maxAbundHigh; }
void GSP::setDoLightCompetition(const bool& doLightCompetition){ m_DoLightCompetition = doLightCompetition; }
void GSP::setLightThreshLow(const int& lightThreshLow){ m_LightThreshLow = lightThreshLow; }
void GSP::setLightThreshMedium(const int& lightThreshMedium){ m_LightThreshMedium = lightThreshMedium; }
void GSP::setDoHabSuitability(const bool& doHabSuitability){ m_DoHabSuitability = doHabSuitability; }
void GSP::setHabSuitMode(const int& habSuitMode){ m_HabSuitMode = habSuitMode; }
void GSP::setDoDispersal(const bool& doDispersal){ m_DoDispersal = doDispersal; }
void GSP::setDispersalMode(const int& dispersalMode){ m_DispersalMode = dispersalMode; }
void GSP::setDoDisturbances(const bool& doDisturbances){ m_DoDisturbances = doDisturbances; }
void GSP::setNoDist(const int& noDist){ m_NoDist = noDist; }
void GSP::setNoDistSub(const int& noDistSub){ m_NoDistSub = noDistSub; }
void GSP::setFreqDist(const vector<int>& freqDist){ m_FreqDist = freqDist; }
void GSP::setDoSoilCompetition(const bool& doSoilCompetition){ m_DoSoilCompetition = doSoilCompetition; }
void GSP::setSoilInit(const double& soilInit){ m_SoilInit = soilInit; }
void GSP::setSoilRetention(const double& soilRetention){ m_SoilRetention = soilRetention; }
void GSP::setDoFireDisturbances(const bool& doFireDisturbances){ m_DoFireDisturbances = doFireDisturbances; }
void GSP::setNoFireDist(const int& noFireDist){ m_NoFireDist = noFireDist; }
void GSP::setNoFireDistSub(const int& noFireDistSub){ m_NoFireDistSub = noFireDistSub; }
void GSP::setFreqFireDist(const vector<int>& freqFireDist){ m_FreqFireDist = freqFireDist; }
void GSP::setFireIgnitMode(const int& fireIgnitMode){ m_FireIgnitMode = fireIgnitMode; }
void GSP::setFireNeighMode(const int& fireNeighMode){ m_FireNeighMode = fireNeighMode; }
void GSP::setFirePropMode(const int& firePropMode){ m_FirePropMode = firePropMode; }
void GSP::setFireQuotaMode(const int& fireQuotaMode){ m_FireQuotaMode = fireQuotaMode; }
void GSP::setFireIgnitNo(const vector<int>& fireIgnitNo){ m_FireIgnitNo = fireIgnitNo; }
void GSP::setFireIgnitNoHist(const vector<int>& fireIgnitNoHist){ m_FireIgnitNoHist = fireIgnitNoHist; }
void GSP::setFireIgnitFlammMax(const int& fireIgnitFlammMax){ m_FireIgnitFlammMax = fireIgnitFlammMax; }
void GSP::setFireIgnitLogis(const vector<double>& fireIgnitLogis){ m_FireIgnitLogis = fireIgnitLogis; }
void GSP::setFireNeighCC(const vector<int>& fireNeighCC){ m_FireNeighCC = fireNeighCC; }
void GSP::setFirePropIntensity(const vector<double>& firePropIntensity){ m_FirePropIntensity = firePropIntensity; }
void GSP::setFirePropLogis(const vector<double>& firePropLogis){ m_FirePropLogis = firePropLogis; }
void GSP::setFireQuotaMax(const int& fireQuotaMax){ m_FireQuotaMax = fireQuotaMax; }
void GSP::setDoDroughtDisturbances(const bool& doDroughtDisturbances){ m_DoDroughtDisturbances = doDroughtDisturbances; }
void GSP::setNoDroughtSub(const int& noDroughtSub){ m_NoDroughtSub = noDroughtSub; }
void GSP::setChronoPost(const string& chronoPost){ m_ChronoPost = chronoPost; }
void GSP::setChronoCurr(const string& chronoCurr){ m_ChronoCurr = chronoCurr; }
void GSP::setDoAliensIntroduction(const bool& doAliensIntroduction){ m_DoAliensIntroduction = doAliensIntroduction; }
void GSP::setFreqAliens(const vector<int>& freqAliens){ m_FreqAliens = freqAliens; }

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void GSP::show()
{
	cout << endl;
	cout << "Global Simulation Parameters:" << endl;
	cout << endl;
	cout << "m_NoCPU = " << m_NoCPU << endl;
	cout << "m_NoFG = " << m_NoFG << endl;
	cout << "m_NoStrata = " << m_NoStrata << endl;
	cout << "m_SimulDuration = " << m_SimulDuration << endl;
	cout << "m_SeedingDuration = " << m_SeedingDuration << endl;
	cout << "m_SeedingTimeStep = " << m_SeedingTimeStep << endl;
	cout << "m_SeedingInput = " << m_SeedingInput << endl;
	cout << "m_MaxAbundLow = " << m_MaxAbundLow << endl;
	cout << "m_MaxAbundMedium = " << m_MaxAbundMedium << endl;
	cout << "m_MaxAbundHigh = " << m_MaxAbundHigh << endl;
	cout << "m_DoLightCompetition = " << m_DoLightCompetition << endl;
	cout << "m_LightThreshLow = " << m_LightThreshLow << endl;
	cout << "m_LightThreshMedium = " << m_LightThreshMedium << endl;
	cout << "m_DoHabSuitability = " << m_DoHabSuitability << endl;
	cout << "m_HabSuitMode = " << m_HabSuitMode << endl;
	cout << "m_DoDispersal = " << m_DoDispersal << endl;
	cout << "m_DispersalMode = " << m_DispersalMode << endl;
	cout << "m_DoDisturbances = " << m_DoDisturbances << endl;
	cout << "m_NoDist = " << m_NoDist << endl;
	cout << "m_NoDistSub = " << m_NoDistSub << endl;
	cout << "m_FreqDist = ";
	copy(m_FreqDist.begin(), m_FreqDist.end(), ostream_iterator<int>(cout, " "));
	cout << endl;
	cout << "m_DoSoilCompetition = " << m_DoSoilCompetition << endl;
	cout << "m_SoilInit = " << m_SoilInit << endl;
	cout << "m_SoilRetention = " << m_SoilRetention << endl;
	cout << "m_DoFireDisturbances = " << m_DoFireDisturbances << endl;
	cout << "m_NoFireDist = " << m_NoFireDist << endl;
	cout << "m_NoFireDistSub = " << m_NoFireDistSub << endl;
	cout << "m_FreqFireDist = ";
	copy(m_FreqFireDist.begin(), m_FreqFireDist.end(), ostream_iterator<int>(cout, " "));
	cout << endl;
	cout << "m_FireIgnitMode = " << m_FireIgnitMode << endl;
	cout << "m_FireNeighMode = " << m_FireNeighMode << endl;
	cout << "m_FirePropMode = " << m_FirePropMode << endl;
	cout << "m_FireQuotaMode = " << m_FireQuotaMode << endl;
	cout << "m_FireIgnitNo = ";
	copy(m_FireIgnitNo.begin(), m_FireIgnitNo.end(), ostream_iterator<int>(cout, " "));
	cout << endl;
	cout << "m_FireIgnitNoHist = ";
	copy(m_FireIgnitNoHist.begin(), m_FireIgnitNoHist.end(), ostream_iterator<int>(cout, " "));
	cout << endl;
	cout << "m_FireIgnitFlammMax = " << m_FireIgnitFlammMax << endl;
	cout << "m_FireIgnitLogis = ";
	copy(m_FireIgnitLogis.begin(), m_FireIgnitLogis.end(), ostream_iterator<double>(cout, " "));
	cout << endl;
	cout << "m_FireNeighCC = ";
	copy(m_FireNeighCC.begin(), m_FireNeighCC.end(), ostream_iterator<int>(cout, " "));
	cout << endl;
	cout << "m_FirePropIntensity = ";
	copy(m_FirePropIntensity.begin(), m_FirePropIntensity.end(), ostream_iterator<double>(cout, " "));
	cout << endl;
	cout << "m_FirePropLogis = ";
	copy(m_FirePropLogis.begin(), m_FirePropLogis.end(), ostream_iterator<double>(cout, " "));
	cout << endl;
	cout << "m_FireQuotaMax = " << m_FireQuotaMax << endl;
	cout << "m_DoDroughtDisturbances = " << m_DoDroughtDisturbances << endl;
	cout << "m_NoDroughtSub = " << m_NoDroughtSub << endl;
	cout << "m_ChronoPost = " << m_ChronoPost << endl;
	cout << "m_ChronoCurr = " << m_ChronoCurr << endl;
	cout << "m_DoAliensIntroduction = " << m_DoAliensIntroduction << endl;
	cout << "m_FreqAliens = ";
	copy(m_FreqAliens.begin(), m_FreqAliens.end(), ostream_iterator<int>(cout, " "));
	cout << endl;
	cout << endl;
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

int GSP::AbundToInt(Abund abund)
{
	/* Convert Abundance classes to integer (defined by user) */
	int res = 0;
	switch (abund)
	{
		case ANone: case Acount:
			res =  0;
			break;
		case ALow:
			res = m_MaxAbundLow;
			break;
		case AMedium:
			res = m_MaxAbundMedium;
			break;
		case AHigh:
			res = m_MaxAbundHigh;
			break;
	}
	return res;
}
