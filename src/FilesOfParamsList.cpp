#include "FilesOfParamsList.h"

using namespace std;


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FOPL::FOPL() :
m_GlobSimulParams(""), /* Global simulation parameters */
m_SavedState(""), m_SavingDir(""), m_SavingTimesMaps(""), m_SavingTimesObjects(""), /* Saving parameters */
m_Mask(""), m_MaskDist(1,""), m_MaskFire(1,""), m_MaskDrought(""), m_MaskElevation(""), m_MaskSlope(""), /* Spatial parameters */
m_MaskChangemaskFiles(1,""), m_MaskChangemaskYears(""), m_HabSuitChangemaskFiles(1,""), m_HabSuitChangemaskYears(""), m_DistChangemaskFiles(1,""), m_DistChangemaskYears(""), /* Simulation Changes parameters */
m_FireChangemaskFiles(1,""), m_FireChangemaskYears(""), m_FireChangefreqFiles(1,""), m_FireChangefreqYears(""),
m_DroughtChangemaskFiles(1,""), m_DroughtChangemaskYears(""),
m_AliensChangemaskFiles(1,""), m_AliensChangemaskYears(""), m_AliensChangefreqFiles(1,""), m_AliensChangefreqYears(""), /* Aliens introduction parameters */
m_FGLifeHistory(1,""), m_FGLight(1,""), m_FGMapsHabSuit(1,""), m_FGDispersal(1,""), /* FG specific parameters */
m_FGDisturbance(1,""), m_FGSoil(1,""), m_FGFire(1,""), m_FGDrought(1,""), m_FGMapsAliens(1,"") /* FG specific parameters */
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FOPL::FOPL(string paramSimulFile)
{
	testFileExist("paramSimulFile", paramSimulFile, false);

	/* Global simulation parameters */
	m_GlobSimulParams = ReadParamsWithinFile(paramSimulFile, "GLOBAL_PARAMS")[0];

	/* Saving parameters */
	m_SavedState = ReadParamsWithinFile(paramSimulFile, "SAVED_STATE")[0];
	m_SavingDir = ReadParamsWithinFile(paramSimulFile, "SAVING_DIR")[0];
	m_SavingTimesMaps = ReadParamsWithinFile(paramSimulFile, "SAVING_YEARS_MAPS")[0];
	m_SavingTimesObjects = ReadParamsWithinFile(paramSimulFile, "SAVING_YEARS_OBJECTS")[0];

	/* Spatial parameters */
	m_Mask = ReadParamsWithinFile(paramSimulFile, "MASK")[0];
	m_MaskDist = ReadParamsWithinFile(paramSimulFile, "DIST_MASK");
	m_MaskFire = ReadParamsWithinFile(paramSimulFile, "FIRE_MASK");
	m_MaskDrought = ReadParamsWithinFile(paramSimulFile, "DROUGHT_MASK")[0];
	m_MaskElevation = ReadParamsWithinFile(paramSimulFile, "ELEVATION_MASK")[0];
	m_MaskSlope = ReadParamsWithinFile(paramSimulFile, "SLOPE_MASK")[0];

	/* Simulation Timing parameters */
	m_MaskChangemaskFiles = ReadParamsWithinFile(paramSimulFile, "MASK_CHANGEMASK_FILES");
	m_MaskChangemaskYears = ReadParamsWithinFile(paramSimulFile, "MASK_CHANGEMASK_YEARS")[0];
	m_HabSuitChangemaskFiles = ReadParamsWithinFile(paramSimulFile, "HABSUIT_CHANGEMASK_FILES");
	m_HabSuitChangemaskYears = ReadParamsWithinFile(paramSimulFile, "HABSUIT_CHANGEMASK_YEARS")[0];
	m_DistChangemaskFiles = ReadParamsWithinFile(paramSimulFile, "DIST_CHANGEMASK_FILES");
	m_DistChangemaskYears = ReadParamsWithinFile(paramSimulFile, "DIST_CHANGEMASK_YEARS")[0];
	m_FireChangemaskFiles = ReadParamsWithinFile(paramSimulFile, "FIRE_CHANGEMASK_FILES");
	m_FireChangemaskYears = ReadParamsWithinFile(paramSimulFile, "FIRE_CHANGEMASK_YEARS")[0];
	m_FireChangefreqFiles = ReadParamsWithinFile(paramSimulFile, "FIRE_CHANGEFREQ_FILES");
	m_FireChangefreqYears = ReadParamsWithinFile(paramSimulFile, "FIRE_CHANGEFREQ_YEARS")[0];
	m_DroughtChangemaskFiles = ReadParamsWithinFile(paramSimulFile, "DROUGHT_CHANGEMASK_FILES");
	m_DroughtChangemaskYears = ReadParamsWithinFile(paramSimulFile, "DROUGHT_CHANGEMASK_YEARS")[0];
	m_AliensChangemaskFiles = ReadParamsWithinFile(paramSimulFile, "ALIENS_CHANGEMASK_FILES");
	m_AliensChangemaskYears = ReadParamsWithinFile(paramSimulFile, "ALIENS_CHANGEMASK_YEARS")[0];
	m_AliensChangefreqFiles = ReadParamsWithinFile(paramSimulFile, "ALIENS_CHANGEFREQ_FILES");
	m_AliensChangefreqYears = ReadParamsWithinFile(paramSimulFile, "ALIENS_CHANGEFREQ_YEARS")[0];

	/* FG specific parameters */
	m_FGLifeHistory = ReadParamsWithinFile(paramSimulFile, "PFG_PARAMS_LIFE_HISTORY");
	m_FGLight = ReadParamsWithinFile(paramSimulFile, "PFG_PARAMS_LIGHT");
	m_FGMapsHabSuit = ReadParamsWithinFile(paramSimulFile, "PFG_MASK_HABSUIT");
	m_FGDispersal = ReadParamsWithinFile(paramSimulFile, "PFG_PARAMS_DISPERSAL");
	m_FGDisturbance = ReadParamsWithinFile(paramSimulFile, "PFG_PARAMS_DISTURBANCES");
	m_FGSoil = ReadParamsWithinFile(paramSimulFile, "PFG_PARAMS_SOIL");
	m_FGFire = ReadParamsWithinFile(paramSimulFile, "PFG_PARAMS_FIRE");
	m_FGDrought = ReadParamsWithinFile(paramSimulFile, "PFG_PARAMS_DROUGHT");
	m_FGMapsAliens = ReadParamsWithinFile(paramSimulFile, "PFG_MASK_ALIENS");
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FOPL::~FOPL()
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

const string& FOPL::getGlobSimulParams() const{ return  m_GlobSimulParams; }
const string& FOPL::getSavedState() const{ return  m_SavedState; }
const string& FOPL::getSavingDir() const{ return  m_SavingDir; }
const string& FOPL::getSavingTimesMaps() const{ return  m_SavingTimesMaps; }
const string& FOPL::getSavingTimesObjects() const{ return  m_SavingTimesObjects; }
const string& FOPL::getMask() const{ return  m_Mask; }
const vector<string>& FOPL::getMaskDist() const{ return  m_MaskDist; }
const vector<string>& FOPL::getMaskFire() const{ return  m_MaskFire; }
const string& FOPL::getMaskDrought() const{ return  m_MaskDrought; }
const string& FOPL::getMaskElevation() const{ return  m_MaskElevation; }
const string& FOPL::getMaskSlope() const{ return  m_MaskSlope; }
const vector<string>& FOPL::getMaskChangemaskFiles() const{ return  m_MaskChangemaskFiles; }
const string& FOPL::getMaskChangemaskYears() const{ return  m_MaskChangemaskYears; }
const vector<string>& FOPL::getHabSuitChangemaskFiles() const{ return  m_HabSuitChangemaskFiles; }
const string& FOPL::getHabSuitChangemaskYears() const{ return  m_HabSuitChangemaskYears; }
const vector<string>& FOPL::getDistChangemaskFiles() const{ return  m_DistChangemaskFiles; }
const string& FOPL::getDistChangemaskYears() const{ return  m_DistChangemaskYears; }
const vector<string>& FOPL::getFireChangemaskFiles() const{ return  m_FireChangemaskFiles; }
const string& FOPL::getFireChangemaskYears() const{ return  m_FireChangemaskYears; }
const vector<string>& FOPL::getFireChangefreqFiles() const{ return  m_FireChangefreqFiles; }
const string& FOPL::getFireChangefreqYears() const{ return  m_FireChangefreqYears; }
const vector<string>& FOPL::getDroughtChangemaskFiles() const{ return  m_DroughtChangemaskFiles; }
const string& FOPL::getDroughtChangemaskYears() const{ return  m_DroughtChangemaskYears; }
const vector<string>& FOPL::getFGLifeHistory() const{ return  m_FGLifeHistory; }
const vector<string>& FOPL::getFGLight() const{ return  m_FGLight; }
const vector<string>& FOPL::getFGMapsHabSuit() const{ return  m_FGMapsHabSuit; }
const vector<string>& FOPL::getFGDispersal() const{ return  m_FGDispersal; }
const vector<string>& FOPL::getFGDisturbance() const{ return  m_FGDisturbance; }
const vector<string>& FOPL::getFGSoil() const{ return  m_FGSoil; }
const vector<string>& FOPL::getFGFire() const{ return  m_FGFire; }
const vector<string>& FOPL::getFGDrought() const{ return  m_FGDrought; }
const vector<string>& FOPL::getFGMapsAliens() const{ return  m_FGMapsAliens; }
const vector<string>& FOPL::getAliensChangemaskFiles() const{ return  m_AliensChangemaskFiles; }
const string& FOPL::getAliensChangemaskYears() const{ return  m_AliensChangemaskYears; }
const vector<string>& FOPL::getAliensChangefreqFiles() const{ return  m_AliensChangefreqFiles; }
const string& FOPL::getAliensChangefreqYears() const{ return  m_AliensChangefreqYears; }

void FOPL::setGlobSimulParams(const string& globSimulParams){ m_GlobSimulParams = globSimulParams; }
void FOPL::setSavedState(const string& savedState){ m_SavedState = savedState; }
void FOPL::setSavingDir(const string& savingDir){ m_SavingDir = savingDir; }
void FOPL::setSavingTimesMaps(const string& savingTimesMaps){ m_SavingTimesMaps = savingTimesMaps; }
void FOPL::setSavingTimesObjects(const string& savingTimesObjects){ m_SavingTimesObjects = savingTimesObjects; }
void FOPL::setMask(const string& mask){ m_Mask = mask; }
void FOPL::setMaskDist(const vector<string>& maskDist){ m_MaskDist = maskDist; }
void FOPL::setMaskFire(const vector<string>& maskFire){ m_MaskFire = maskFire; }
void FOPL::setMaskDrought(const string& maskDrought){ m_MaskDrought = maskDrought; }
void FOPL::setMaskElevation(const string& maskElevation){ m_MaskElevation = maskElevation; }
void FOPL::setMaskSlope(const string& maskSlope){ m_MaskSlope = maskSlope; }
void FOPL::setMaskChangemaskFiles(const vector<string>& maskChangemaskFiles){ m_MaskChangemaskFiles = maskChangemaskFiles; }
void FOPL::setMaskChangemaskYears(const string& maskChangemaskYears){ m_MaskChangemaskYears = maskChangemaskYears; }
void FOPL::setHabSuitChangemaskFiles(const vector<string>& habSuitChangemaskFiles){ m_HabSuitChangemaskFiles = habSuitChangemaskFiles; }
void FOPL::setHabSuitChangemaskYears(const string& habSuitChangemaskYears){ m_HabSuitChangemaskYears = habSuitChangemaskYears; }
void FOPL::setDistChangemaskFiles(const vector<string>& distChangemaskFiles){ m_DistChangemaskFiles = distChangemaskFiles; }
void FOPL::setDistChangemaskYears(const string& distChangemaskYears){ m_DistChangemaskYears = distChangemaskYears; }
void FOPL::setFireChangemaskFiles(const vector<string>& fireChangemaskFiles){ m_FireChangemaskFiles = fireChangemaskFiles; }
void FOPL::setFireChangemaskYears(const string& fireChangemaskYears){ m_FireChangemaskYears = fireChangemaskYears; }
void FOPL::setFireChangefreqFiles(const vector<string>& fireChangefreqFiles){ m_FireChangefreqFiles = fireChangefreqFiles; }
void FOPL::setFireChangefreqYears(const string& fireChangefreqYears){ m_FireChangefreqYears = fireChangefreqYears; }
void FOPL::setDroughtChangemaskFiles(const vector<string>& droughtChangemaskFiles){ m_DroughtChangemaskFiles = droughtChangemaskFiles; }
void FOPL::setDroughtChangemaskYears(const string& droughtChangemaskYears){ m_DroughtChangemaskYears = droughtChangemaskYears; }
void FOPL::setFGLifeHistory(const vector<string>& fgLifeHistory){ m_FGLifeHistory = fgLifeHistory; }
void FOPL::setFGLight(const vector<string>& fgLight){ m_FGLight = fgLight; }
void FOPL::setFGMapsHabSuit(const vector<string>& fgMapsHabSuit){ m_FGMapsHabSuit = fgMapsHabSuit; }
void FOPL::setFGDispersal(const vector<string>& fgDispersal){ m_FGDispersal = fgDispersal; }
void FOPL::setFGDisturbance(const vector<string>& fgDisturbance){ m_FGDisturbance = fgDisturbance; }
void FOPL::setFGSoil(const vector<string>& fgSoil){ m_FGSoil = fgSoil; }
void FOPL::setFGFire(const vector<string>& fgFire){ m_FGFire = fgFire; }
void FOPL::setFGDrought(const vector<string>& fgDrought){ m_FGDrought = fgDrought; }
void FOPL::setFGMapsAliens(const vector<string>& fgMapsAliens){ m_FGMapsAliens = fgMapsAliens; }
void FOPL::setAliensChangemaskFiles(const vector<string>& aliensChangemaskFiles){ m_AliensChangemaskFiles = aliensChangemaskFiles; }
void FOPL::setAliensChangemaskYears(const string& aliensChangemaskYears){ m_AliensChangemaskYears = aliensChangemaskYears; }
void FOPL::setAliensChangefreqFiles(const vector<string>& aliensChangefreqFiles){ m_AliensChangefreqFiles = aliensChangefreqFiles; }
void FOPL::setAliensChangefreqYears(const string& aliensChangefreqYears){ m_AliensChangefreqYears = aliensChangefreqYears; }

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions		                                                                           */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
void FOPL::show()
{
	logg.debug("\nList of parameters files paths:\n",
						 // Globale simulations parameters
						 "\nm_GlobSimulParams = ", m_GlobSimulParams,
						 // Saving parameters
						 "\nm_SavingDir = ", m_SavingDir,
						 "\nm_SavingTimesMaps = ", m_SavingTimesMaps,
						 "\nm_SavingTimesObjects = ", m_SavingTimesObjects,
						 // Spatial parameters
						 "\nm_Mask = ", m_Mask,
						 "\nm_MaskDist = ", m_MaskDist,
						 "\nm_MaskFire = ", m_MaskFire,
						 "\nm_MaskDrought = ", m_MaskDrought,
					 	 "\nm_MaskElevation = ", m_MaskElevation,
					 	 "\nm_MaskSlope = ", m_MaskSlope,
						 // Simulation timing parameters
						 "\nm_MaskChangemaskFiles = ", m_MaskChangemaskFiles,
						 "\nm_MaskChangemaskYears = ", m_MaskChangemaskYears,
						 "\nm_HabSuitChangemaskFiles = ", m_HabSuitChangemaskFiles,
						 "\nm_HabSuitChangemaskYears = ", m_HabSuitChangemaskYears,
						 "\nm_DistChangemaskFiles = ", m_DistChangemaskFiles,
						 "\nm_DistChangemaskYears = ", m_DistChangemaskYears,
						 "\nm_FireChangemaskFiles = ", m_FireChangemaskFiles,
						 "\nm_FireChangemaskYears = ", m_FireChangemaskYears,
						 "\nm_FireChangefreqFiles = ", m_FireChangefreqFiles,
						 "\nm_FireChangefreqYears = ", m_FireChangefreqYears,
						 "\nm_DroughtChangemaskFiles = ", m_DroughtChangemaskFiles,
						 "\nm_DroughtChangemaskYears = ", m_DroughtChangemaskYears,
						 // FG specific parameters
						 "\nm_FGLifeHistory = ", m_FGLifeHistory,
						 "\nm_FGLight = ", m_FGLight,
						 "\nm_FGMapsHabSuit = ", m_FGMapsHabSuit,
						 "\nm_FGDispersal = ", m_FGDispersal,
						 "\nm_FGDisturbance = ", m_FGDisturbance,
						 "\nm_FGSoil = ", m_FGSoil,
						 "\nm_FGFire = ", m_FGFire,
						 "\nm_FGDrought = ", m_FGDrought,
						 "\nm_FGMapsAliens = ", m_FGMapsAliens,
						 // Aliens introduction parameters
						 "\nm_AliensChangemaskFiles = ", m_AliensChangemaskFiles,
						 "\nm_AliensChangemaskYears = ", m_AliensChangemaskYears,
						 "\nm_AliensChangefreqFiles = ", m_AliensChangefreqFiles,
						 "\nm_AliensChangefreqYears = ", m_AliensChangefreqYears,
						 "\n");
} // end of show()


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FOPL::checkCorrectParams_light()
{
	testDirExist("--SAVING_DIR--",m_SavingDir+"/LIGHT/", false);
	testFileExist("--PFG_PARAMS_LIGHT--",m_FGLight, false);
}

void FOPL::checkCorrectParams_habSuit()
{
	testFileExist("--PFG_MASK_HABSUIT--",m_FGMapsHabSuit, false);
	testFileExist("--HABSUIT_CHANGEMASK_YEARS--",m_HabSuitChangemaskYears, (strcmp(m_HabSuitChangemaskYears.c_str(),"0")==0));
	testFileExist("--HABSUIT_CHANGEMASK_FILES--",m_HabSuitChangemaskFiles, (strcmp(m_HabSuitChangemaskYears.c_str(),"0")==0));
	testFileExist_changeFile("--HABSUIT_CHANGEMASK_FILES--",m_HabSuitChangemaskFiles, (strcmp(m_HabSuitChangemaskYears.c_str(),"0")==0));
}

void FOPL::checkCorrectParams_disp()
{
	testDirExist("--SAVING_DIR--",m_SavingDir+"/DISPERSAL/", false);
	testFileExist("--PFG_PARAMS_DISPERSAL--",m_FGDispersal, false);
}

void FOPL::checkCorrectParams_dist()
{
	testFileExist("--PFG_PARAMS_DISTURBANCES--",m_FGDisturbance, false);
	testFileExist("--DIST_MASK--",m_MaskDist, false);
	testFileExist("--DIST_CHANGEMASK_YEARS--",m_DistChangemaskYears, (strcmp(m_DistChangemaskYears.c_str(),"0")==0));
	testFileExist("--DIST_CHANGEMASK_FILES--",m_DistChangemaskFiles, (strcmp(m_DistChangemaskYears.c_str(),"0")==0));
	testFileExist_changeFile("--DIST_CHANGEMASK_FILES--",m_DistChangemaskFiles, (strcmp(m_DistChangemaskYears.c_str(),"0")==0));
}

void FOPL::checkCorrectParams_soil()
{
	testDirExist("--SAVING_DIR--",m_SavingDir+"/SOIL/", false);
	testFileExist("--PFG_PARAMS_SOIL--",m_FGSoil, false);
}

void FOPL::checkCorrectParams_fire()
{
	testFileExist("--PFG_PARAMS_FIRE--",m_FGFire, false);
	testFileExist("--FIRE_MASK--",m_MaskFire, true);
	testFileExist("--FIRE_CHANGEMASK_YEARS--",m_FireChangemaskYears, (strcmp(m_FireChangemaskYears.c_str(),"0")==0));
	testFileExist("--FIRE_CHANGEMASK_FILES--",m_FireChangemaskFiles, (strcmp(m_FireChangemaskYears.c_str(),"0")==0));
	testFileExist_changeFile("--FIRE_CHANGEMASK_FILES--",m_FireChangemaskFiles, (strcmp(m_FireChangemaskYears.c_str(),"0")==0));
	testFileExist("--FIRE_CHANGEFREQ_FILES--",m_FireChangefreqFiles, true);
	testFileExist("--FIRE_CHANGEFREQ_YEARS--",m_FireChangefreqYears, true);
	testFileExist("--DROUGHT_MASK--",m_MaskDrought, true);
	testFileExist("--ELEVATION_MASK--",m_MaskElevation, true);
	testFileExist("--SLOPE_MASK--",m_MaskSlope, true);
}

void FOPL::checkCorrectParams_drought()
{
	testFileExist("--PFG_PARAMS_DROUGHT--",m_FGDrought, false);
	testFileExist("--DROUGHT_MASK--",m_MaskDrought, false);
	testFileExist("--DROUGHT_CHANGEMASK_YEARS--",m_DroughtChangemaskYears, (strcmp(m_DroughtChangemaskYears.c_str(),"0")==0));
	testFileExist("--DROUGHT_CHANGEMASK_FILES--",m_DroughtChangemaskFiles, (strcmp(m_DroughtChangemaskYears.c_str(),"0")==0));
	testFileExist_changeFile("--DROUGHT_CHANGEMASK_FILES--",m_DroughtChangemaskFiles, (strcmp(m_DroughtChangemaskYears.c_str(),"0")==0));
}

void FOPL::checkCorrectParams_aliens()
{
	testFileExist("--ALIENS_MASK--",m_FGMapsAliens, false);
	testFileExist("--ALIENS_CHANGEMASK_YEARS--",m_AliensChangemaskYears, (strcmp(m_AliensChangemaskYears.c_str(),"0")==0));
	testFileExist("--ALIENS_CHANGEMASK_FILES--",m_AliensChangemaskFiles, (strcmp(m_AliensChangemaskYears.c_str(),"0")==0));
	testFileExist_changeFile("--ALIENS_CHANGEMASK_FILES--",m_AliensChangemaskFiles, (strcmp(m_AliensChangemaskYears.c_str(),"0")==0));
	testFileExist("--ALIENS_CHANGEFREQ_FILES--",m_AliensChangefreqFiles, true);
	testFileExist("--ALIENS_CHANGEFREQ_YEARS--",m_AliensChangefreqYears, true);
}

void FOPL::checkCorrectParams(const bool& doLight, const bool& doHabSuit, const bool& doDisp, const bool& doDist,
const bool& doSoil, const bool& doFire, const bool& doDrought, const bool& doAliens)
{
	logg.info("\n===========> RUNNING Check of PARAM SIMUL FILES :");

	testFileExist("--GLOBAL_PARAMS--",m_GlobSimulParams, false);
	testFileExist("--SAVED_STATE--",m_SavedState, true);

	testDirExist("--SAVING_DIR--",m_SavingDir, false);
	testDirExist("--SAVING_DIR--",m_SavingDir+"/ABUND_perPFG_perStrata/", false);
	testDirExist("--SAVING_DIR--",m_SavingDir+"/ABUND_perPFG_allStrata/", false);
	testDirExist("--SAVING_DIR--",m_SavingDir+"/ABUND_allPFG_perStrata/", false);

	testFileExist("--SAVING_YEARS_MAPS--",m_SavingTimesMaps, true);
	testFileExist("--SAVING_YEARS_OBJECTS--",m_SavingTimesObjects, true);

	testFileExist("--MASK--",m_Mask, false);
	testFileExist("--MASK_CHANGEMASK_YEARS--",m_MaskChangemaskYears, true);
	testFileExist("--MASK_CHANGEMASK_FILES--",m_MaskChangemaskFiles, true);
	testFileExist_changeFile("--MASK_CHANGEMASK_FILES--",m_MaskChangemaskFiles, true);

	testFileExist("--PFG_PARAMS_LIFE_HISTORY--",m_FGLifeHistory, false);

	if (doLight) checkCorrectParams_light();
	if (doHabSuit) checkCorrectParams_habSuit();
	if (doDisp) checkCorrectParams_disp();
	if (doDist) checkCorrectParams_dist();
	if (doSoil) checkCorrectParams_soil();
	if (doFire) checkCorrectParams_fire();
	if (doDrought) checkCorrectParams_drought();
	if (doAliens) checkCorrectParams_aliens();

	logg.info("===========> Check OK!\n");
} // end of checkCorrectParams(...)

void FOPL::checkCorrectParams()
{
	logg.info("\n===========> RUNNING Check of PARAM SIMUL FILES :");

	testFileExist("--GLOBAL_PARAMS--",m_GlobSimulParams, false);
	testFileExist("--SAVED_STATE--",m_SavedState, true);

	testDirExist("--SAVING_DIR--",m_SavingDir, false);
	testDirExist("--SAVING_DIR--",m_SavingDir+"/ABUND_perPFG_perStrata/", false);
	testDirExist("--SAVING_DIR--",m_SavingDir+"/ABUND_perPFG_allStrata/", false);
	testDirExist("--SAVING_DIR--",m_SavingDir+"/ABUND_allPFG_perStrata/", false);
	testDirExist("--SAVING_DIR--",m_SavingDir+"/SOIL/", false);
	testDirExist("--SAVING_DIR--",m_SavingDir+"/LIGHT/", false);
	testDirExist("--SAVING_DIR--",m_SavingDir+"/DISPERSAL/", false);

	testFileExist("--SAVING_YEARS_MAPS--",m_SavingTimesMaps, true);
	testFileExist("--SAVING_YEARS_OBJECTS--",m_SavingTimesObjects, true);
	testFileExist("--MASK--",m_Mask, false);

	testFileExist("--DIST_MASK--",m_MaskDist, true);
	testFileExist("--FIRE_MASK--",m_MaskFire, true);
	testFileExist("--DROUGHT_MASK--",m_MaskDrought, true);
	testFileExist("--ELEVATION_MASK--",m_MaskElevation, true);
	testFileExist("--SLOPE_MASK--",m_MaskSlope, true);
	testFileExist("--MASK_CHANGEMASK_YEARS--",m_MaskChangemaskYears, true);
	testFileExist("--MASK_CHANGEMASK_FILES--",m_MaskChangemaskFiles, true);
	testFileExist_changeFile("--MASK_CHANGEMASK_FILES--",m_MaskChangemaskFiles, true);
	testFileExist("--HABSUIT_CHANGEMASK_YEARS--",m_HabSuitChangemaskYears, true);
	testFileExist("--HABSUIT_CHANGEMASK_FILES--",m_HabSuitChangemaskFiles, true);
	testFileExist_changeFile("--HABSUIT_CHANGEMASK_FILES--",m_HabSuitChangemaskFiles, true);
	testFileExist("--DIST_CHANGEMASK_YEARS--",m_DistChangemaskYears, true);
	testFileExist("--DIST_CHANGEMASK_FILES--",m_DistChangemaskFiles, true);
	testFileExist_changeFile("--DIST_CHANGEMASK_FILES--",m_DistChangemaskFiles, true);
	testFileExist("--FIRE_CHANGEMASK_YEARS--",m_FireChangemaskYears, true);
	testFileExist("--FIRE_CHANGEMASK_FILES--",m_FireChangemaskFiles, true);
	testFileExist_changeFile("--FIRE_CHANGEMASK_FILES--",m_FireChangemaskFiles, true);
	testFileExist("--FIRE_CHANGEFREQ_FILES--",m_FireChangefreqFiles, true);
	testFileExist("--FIRE_CHANGEFREQ_YEARS--",m_FireChangefreqYears, true);
	testFileExist("--DROUGHT_CHANGEMASK_YEARS--",m_DroughtChangemaskYears, true);
	testFileExist("--DROUGHT_CHANGEMASK_FILES--",m_DroughtChangemaskFiles, true);
	testFileExist_changeFile("--DROUGHT_CHANGEMASK_FILES--",m_DroughtChangemaskFiles, true);

	testFileExist("--PFG_PARAMS_LIFE_HISTORY--",m_FGLifeHistory, false);
	testFileExist("--PFG_PARAMS_LIGHT--",m_FGLight, true);
	testFileExist("--PFG_MASK_HABSUIT--",m_FGMapsHabSuit, true);
	testFileExist("--PFG_PARAMS_DISPERSAL--",m_FGDispersal, true);
	testFileExist("--PFG_PARAMS_DISTURBANCES--",m_FGDisturbance, true);
	testFileExist("--PFG_PARAMS_SOIL--",m_FGSoil, true);
	testFileExist("--PFG_PARAMS_FIRE--",m_FGFire, true);
	testFileExist("--PFG_PARAMS_DROUGHT--",m_FGDrought, true);
	testFileExist("--ALIENS_MASK--",m_FGMapsAliens, true);

	testFileExist("--ALIENS_CHANGEMASK_YEARS--",m_AliensChangemaskYears, true);
	testFileExist("--ALIENS_CHANGEMASK_FILES--",m_AliensChangemaskFiles, true);
	testFileExist_changeFile("--ALIENS_CHANGEMASK_FILES--",m_AliensChangemaskFiles, true);
	testFileExist("--ALIENS_CHANGEFREQ_FILES--",m_AliensChangefreqFiles, true);
	testFileExist("--ALIENS_CHANGEFREQ_YEARS--",m_AliensChangefreqYears, true);

	logg.info("===========> Check OK!\n");
} // end of checkCorrectParams(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FOPL::testSameCoord(const string& param, const string& file_name, const string& ext_REF, Coordinates<double>& coord_REF)
{
	if (file_name != "0")
	{
		boost::filesystem::path file_to_test(file_name);
		if (file_to_test.extension()!=ext_REF)
		{ // TEST file extension
			logg.error("!!! Parameter ", param,  " : the file ",  file_name,
			 					 " does not have the same extension than --MASK-- file. Please check!");
		}
		Coordinates<double> coord_to_test = ReadCoordinates(file_name);
		if (!(coord_to_test==coord_REF))
		{ // TEST file coordinates
			logg.error("> TESTED Coordinates - Xmax : ", coord_to_test.getXmax(),
								 "\n> TESTED Coordinates - Xmin : ", coord_to_test.getXmin(),
								 "\n> TESTED Coordinates - Xres : ", coord_to_test.getXres(),
								 "\n> TESTED Coordinates - Xncell : ", coord_to_test.getXncell(),
								 "\n> TESTED Coordinates - Ymax : ", coord_to_test.getYmax(),
								 "\n> TESTED Coordinates - Ymin : ", coord_to_test.getYmin(),
								 "\n> TESTED Coordinates - Yres : ", coord_to_test.getYres(),
								 "\n> TESTED Coordinates - Yncell : ", coord_to_test.getYncell(),
								 "\n> TESTED Coordinates - Totncell : ", coord_to_test.getTotncell(),
								 "\n!!! Parameter ", param, " : the file ", file_name,
								 " does not have the same coordinates than --MASK-- file. Please check!");
		}
	}
}

void FOPL::testSameCoord(const string& param, vector<string> vector_name, const string& ext_REF, Coordinates<double>& coord_REF)
{
	for (vector<string>::iterator file_name=vector_name.begin(); file_name!=vector_name.end(); ++file_name)
	{
		testSameCoord(param, *file_name, ext_REF, coord_REF);
	}
}

void FOPL::checkCorrectMasks()
{
	logg.info("\n===========> RUNNING Check of RASTER MASKS :");

	/* Get mask extension */
	boost::filesystem::path mask_path(m_Mask.c_str());
	string ext_REF = mask_path.extension().string();

	if (ext_REF==".asc")
	{ // ASCII file
		logg.error("!!! The --MASK-- file is an ASCII file. ",
							 "You must provide either a .img or .tif file.");
	}
	if (ext_REF!=".img" && ext_REF!=".tif")
	{ // ASCII file
		logg.error("!!! The extension of the --MASK-- file (", ext_REF,
		 					 ") is not supported. You must provide either a .img or .tif file.");
	}

	/* Get mask coordinates */
	Coordinates<double> coord_REF = ReadCoordinates(m_Mask);
	logg.debug(">> MASK Coordinates - Xmax : ", coord_REF.getXmax(),
						 "\n>> MASK Coordinates - Xmin : ", coord_REF.getXmin(),
						 "\n>> MASK Coordinates - Xres : ", coord_REF.getXres(),
						 "\n>> MASK Coordinates - Xncell : ", coord_REF.getXncell(),
						 "\n>> MASK Coordinates - Ymax : ", coord_REF.getYmax(),
						 "\n>> MASK Coordinates - Ymin : ", coord_REF.getYmin(),
						 "\n>> MASK Coordinates - Yres : ", coord_REF.getYres(),
						 "\n>> MASK Coordinates - Yncell : ", coord_REF.getYncell(),
						 "\n>> MASK Coordinates - Totncell : ", coord_REF.getTotncell());

	testSameCoord("--PFG_MASK_HABSUIT--",m_FGMapsHabSuit,ext_REF,coord_REF);
	testSameCoord("--DIST_MASK--",m_MaskDist,ext_REF,coord_REF);

	testSameCoord("--FIRE_MASK--",m_MaskFire,ext_REF,coord_REF);
	testSameCoord("--DROUGHT_MASK--",m_MaskDrought,ext_REF,coord_REF);
	testSameCoord("--ELEVATION_MASK--",m_MaskElevation,ext_REF,coord_REF);
	testSameCoord("--SLOPE_MASK--",m_MaskSlope,ext_REF,coord_REF);

	testSameCoord("--PFG_MASK_ALIENS--",m_FGMapsAliens,ext_REF,coord_REF);

	logg.info("===========> Check OK!\n");
} // end of checkCorrectMasks(...)
