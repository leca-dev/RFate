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
	cout << endl;
	cout << "List of parameters files paths:" << endl;
	cout << endl;
	
	/* Global simulation parameters */
	cout << "m_GlobSimulParams = " << m_GlobSimulParams << endl;
	
	/* Saving parameters */
	cout << "m_SavingDir = " << m_SavingDir << endl;
	cout << "m_SavingTimesMaps = " << m_SavingTimesMaps << endl;
	cout << "m_SavingTimesObjects = " << m_SavingTimesObjects << endl;
	
	/* Spatial parameters */
	cout << "m_Mask = " << m_Mask << endl;
	cout << "m_MaskDist = ";
	copy(m_MaskDist.begin(), m_MaskDist.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_MaskFire = ";
	copy(m_MaskFire.begin(), m_MaskFire.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_MaskDrought = " << m_MaskDrought << endl;
	cout << "m_MaskElevation = " << m_MaskElevation << endl;
	cout << "m_MaskSlope = " << m_MaskSlope << endl;
	
	/* Simulation Timing parameters */
	cout << "m_MaskChangemaskFiles = ";
	copy(m_MaskChangemaskFiles.begin(), m_MaskChangemaskFiles.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_MaskChangemaskYears = " << m_MaskChangemaskYears << endl;
	cout << "m_HabSuitChangemaskFiles = ";
	copy(m_HabSuitChangemaskFiles.begin(), m_HabSuitChangemaskFiles.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_HabSuitChangemaskYears = " << m_HabSuitChangemaskYears << endl;
	cout << "m_DistChangemaskFiles = ";
	copy(m_DistChangemaskFiles.begin(), m_DistChangemaskFiles.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_DistChangemaskYears = " << m_DistChangemaskYears << endl;
	cout << "m_FireChangemaskFiles = ";
	copy(m_FireChangemaskFiles.begin(), m_FireChangemaskFiles.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_FireChangemaskYears = " << m_FireChangemaskYears << endl;
	cout << "m_FireChangefreqFiles = ";
	copy(m_FireChangefreqFiles.begin(), m_FireChangefreqFiles.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_FireChangefreqYears = " << m_FireChangefreqYears << endl;
	cout << "m_DroughtChangemaskFiles = ";
	copy(m_DroughtChangemaskFiles.begin(), m_DroughtChangemaskFiles.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_DroughtChangemaskYears = " << m_DroughtChangemaskYears << endl;
	
	/* FG specific parameters */
	cout << "m_FGLifeHistory = ";
	copy(m_FGLifeHistory.begin(), m_FGLifeHistory.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_FGLight = ";
	copy(m_FGLight.begin(), m_FGLight.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_FGMapsHabSuit = ";
	copy(m_FGMapsHabSuit.begin(), m_FGMapsHabSuit.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_FGDispersal = ";
	copy(m_FGDispersal.begin(), m_FGDispersal.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_FGDisturbance = ";
	copy(m_FGDisturbance.begin(), m_FGDisturbance.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_FGSoil = ";
	copy(m_FGSoil.begin(), m_FGSoil.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_FGFire = ";
	copy(m_FGFire.begin(), m_FGFire.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_FGDrought = ";
	copy(m_FGDrought.begin(), m_FGDrought.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_FGMapsAliens = ";
	copy(m_FGMapsAliens.begin(), m_FGMapsAliens.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	
	/* Aliens introduction parameters */
	cout << "m_AliensChangemaskFiles = ";
	copy(m_AliensChangemaskFiles.begin(), m_AliensChangemaskFiles.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_AliensChangemaskYears = " << m_AliensChangemaskYears << endl;
	cout << "m_AliensChangefreqFiles = ";
	copy(m_AliensChangefreqFiles.begin(), m_AliensChangefreqFiles.end(), ostream_iterator<string>(cout, " "));
	cout << endl;
	cout << "m_AliensChangefreqYears = " << m_AliensChangefreqYears << endl;
	cout << endl;
} // end of show()


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FOPL::checkCorrectParams_light()
{
	testDirExist("--SAVING_DIR--",m_SavingDir+"/LIGHT/");
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
	testDirExist("--SAVING_DIR--",m_SavingDir+"/DISPERSAL/");
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
	testDirExist("--SAVING_DIR--",m_SavingDir+"/SOIL/");
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
	cout << endl;
	cout << "===========> RUNNING Check of PARAM SIMUL FILES :" << endl;
	
	testFileExist("--GLOBAL_PARAMS--",m_GlobSimulParams, false);
	testFileExist("--SAVED_STATE--",m_SavedState, true);
	
	testDirExist("--SAVING_DIR--",m_SavingDir);
	testDirExist("--SAVING_DIR--",m_SavingDir+"/ABUND_perPFG_perStrata/");
	testDirExist("--SAVING_DIR--",m_SavingDir+"/ABUND_perPFG_allStrata/");
	testDirExist("--SAVING_DIR--",m_SavingDir+"/ABUND_allPFG_perStrata/");
	
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
	
	cout << "===========> Check OK!" << endl;
	cout << endl;
} // end of checkCorrectParams(...)

void FOPL::checkCorrectParams()
{
	cout << endl;
	cout << "===========> RUNNING Check of PARAM SIMUL FILES :" << endl;
	
	testFileExist("--GLOBAL_PARAMS--",m_GlobSimulParams, false);
	testFileExist("--SAVED_STATE--",m_SavedState, true);

	testDirExist("--SAVING_DIR--",m_SavingDir);
	testDirExist("--SAVING_DIR--",m_SavingDir+"/ABUND_perPFG_perStrata/");
	testDirExist("--SAVING_DIR--",m_SavingDir+"/ABUND_perPFG_allStrata/");
	testDirExist("--SAVING_DIR--",m_SavingDir+"/ABUND_allPFG_perStrata/");
	testDirExist("--SAVING_DIR--",m_SavingDir+"/SOIL/");
	testDirExist("--SAVING_DIR--",m_SavingDir+"/LIGHT/");
	testDirExist("--SAVING_DIR--",m_SavingDir+"/DISPERSAL/");

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
	
	cout << "===========> Check OK!" << endl;
	cout << endl;
} // end of checkCorrectParams(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FOPL::testSameCoord(const string& param, const string& file_name, const string& ext_REF, Coordinates<double>& coord_REF)
{
	if (file_name != "0")
	{
		boost::filesystem::path file_to_test(file_name);
		if (file_to_test.extension()!=ext_REF)
		{ // TEST file extension
			cerr << "!!! Parameter " << param << " : the file " << file_name << " does not have the same extension than --MASK-- file. Please check!" << endl;
			terminate();
		}
		Coordinates<double> coord_to_test = ReadCoordinates(file_name);
		if (!(coord_to_test==coord_REF))
		{ // TEST file coordinates
			cout << "> TESTED Coordinates - Xmax : " << coord_to_test.getXmax() << endl;
			cout << "> TESTED Coordinates - Xmin : " << coord_to_test.getXmin() << endl;
			cout << "> TESTED Coordinates - Xres : " << coord_to_test.getXres() << endl;
			cout << "> TESTED Coordinates - Xncell : " << coord_to_test.getXncell() << endl;
			cout << "> TESTED Coordinates - Ymax : " << coord_to_test.getYmax() << endl;
			cout << "> TESTED Coordinates - Ymin : " << coord_to_test.getYmin() << endl;
			cout << "> TESTED Coordinates - Yres : " << coord_to_test.getYres() << endl;
			cout << "> TESTED Coordinates - Yncell : " << coord_to_test.getYncell() << endl;
			cout << "> TESTED Coordinates - Totncell : " << coord_to_test.getTotncell() << endl;
			cerr << "!!! Parameter " << param << " : the file " << file_name << " does not have the same coordinates than --MASK-- file. Please check!" << endl;
			terminate();
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
	cout << endl;
	cout << "===========> RUNNING Check of RASTER MASKS :" << endl;
	
	/* Get mask extension */
	boost::filesystem::path mask_path(m_Mask.c_str());
	string ext_REF = mask_path.extension().string();
	
	if (ext_REF==".asc")
	{ // ASCII file
		cerr << "!!! The --MASK-- file is an ASCII file. You must provide either a .img or .tif file." << endl;
		terminate();
	}
	if (ext_REF!=".img" && ext_REF!=".tif")
	{ // ASCII file
		cerr << "!!! The extension of the --MASK-- file (" << ext_REF << ") is not supported. You must provide either a .img or .tif file." << endl;
		terminate();
	}
	
	/* Get mask coordinates */
	Coordinates<double> coord_REF = ReadCoordinates(m_Mask);
	cout << ">> MASK Coordinates - Xmax : " << coord_REF.getXmax() << endl;
	cout << ">> MASK Coordinates - Xmin : " << coord_REF.getXmin() << endl;
	cout << ">> MASK Coordinates - Xres : " << coord_REF.getXres() << endl;
	cout << ">> MASK Coordinates - Xncell : " << coord_REF.getXncell() << endl;
	cout << ">> MASK Coordinates - Ymax : " << coord_REF.getYmax() << endl;
	cout << ">> MASK Coordinates - Ymin : " << coord_REF.getYmin() << endl;
	cout << ">> MASK Coordinates - Yres : " << coord_REF.getYres() << endl;
	cout << ">> MASK Coordinates - Yncell : " << coord_REF.getYncell() << endl;
	cout << ">> MASK Coordinates - Totncell : " << coord_REF.getTotncell() << endl;
	
	testSameCoord("--PFG_MASK_HABSUIT--",m_FGMapsHabSuit,ext_REF,coord_REF);
	testSameCoord("--DIST_MASK--",m_MaskDist,ext_REF,coord_REF);
	
	testSameCoord("--FIRE_MASK--",m_MaskFire,ext_REF,coord_REF);
	testSameCoord("--DROUGHT_MASK--",m_MaskDrought,ext_REF,coord_REF);
	testSameCoord("--ELEVATION_MASK--",m_MaskElevation,ext_REF,coord_REF);
	testSameCoord("--SLOPE_MASK--",m_MaskSlope,ext_REF,coord_REF);
	
	testSameCoord("--PFG_MASK_ALIENS--",m_FGMapsAliens,ext_REF,coord_REF);
		
	cout << "===========> Check OK!" << endl;
	cout << endl;
} // end of checkCorrectMasks(...)



