/*============================================================================*/
/*                          Files of Parameters Class                         */
/*============================================================================*/

/*!
 * \file FilesOfParamsList.h
 * \brief Parameters files list managing class
 * \author Damien Georges
 * \version 1.0
 */

#ifndef FOPL_H
#define FOPL_H

#include "FGUtils.h"
#include "Logger.h"
#include <boost/filesystem.hpp>

using namespace std;


/*!
 * \class FOPL
 * \brief Parameters files list managing class
 *
 * This class contains a set of parameters corresponding to parameters text
 * files. It allows correct reading and creation. All paths should be given as
 * relative paths.
 * Basic parameters are mandatory : they concern global simulation (Global
 * simulations parameters, saving state and directory, ...), timings (to
 * save abundances and objects), spatial characteristics (mask) and functional
 * groups (life history, ...).
 * If some modules are activated (dispersal, habitat suitability, light, ...),
 * some specific parameters might also be required.
 */

class FOPL
{
	private:

	/* Global simulation parameters */
	string m_GlobSimulParams; /*!< path to global simul params file */

	/* Saving parameters */
	string m_SavedState; /*!< path to previous FATEHDD simulation output objects */
	string m_SavingDir; /*!< Saving directory path */
	string m_SavingTimesMaps; /*!< path to file containing summarised maps saving dates */
	string m_SavingTimesObjects; /*!< path to file containing simul state objects saving dates */

	/* Spatial parameters */
	string m_Mask; /*!< path to .asc mask file */
	vector<string> m_MaskDist; /*!< list of path to disturbances masks */
	vector<string> m_MaskFire; /*!< list of path to fire disturbances masks */
	string m_MaskDrought; /*!< path to drought index mask file */
	string m_MaskElevation; /*!< path to elevation mask file */
	string m_MaskSlope; /*!< path to slope mask file */

	// TODO (damien#1#): change way disturbances params are given as smth like :
	/*
	time=0
	dist1=..
	dist2=...
	dist3=...

	time=xx
	dist1=...
	dist2=...
	dist3=...
	*/

	/* Simulation Timing parameters */
	vector<string> m_MaskChangemaskFiles; /*!< list of files containing list of masks of studied area change scenario */
	string m_MaskChangemaskYears; /*!< list of studied area changes times */
	vector<string> m_HabSuitChangemaskFiles; /*!< list of files containing list of masks of habitat change scenario */
	string m_HabSuitChangemaskYears; /*!< list of habitat changes times */
	vector<string> m_DistChangemaskFiles; /*!< list of files containing list of masks of land use change scenario */
	string m_DistChangemaskYears; /*!< list of land use changes times */
	vector<string> m_FireChangemaskFiles; /*!< list of files containing list of masks of fire change scenario */
	string m_FireChangemaskYears; /*!< list of land use changes times */
	vector<string> m_FireChangefreqFiles; /*!< list of files containing list of fire frequencies files change scenario */
	string m_FireChangefreqYears; /*!< list of fire frequencies changes times */
	vector<string> m_DroughtChangemaskFiles; /*!< list of files containing list of masks of drought index change scenario */
	string m_DroughtChangemaskYears; /*!< list of drought index changes times */
	vector<string> m_AliensChangemaskFiles;	/*!< list of files containing list of masks of aliens introduction change scenario */
	string m_AliensChangemaskYears; /*!< list of aliens introduction changes times */
	vector<string> m_AliensChangefreqFiles;	/*!< list of files containing list of aliens introduction frequencies files change scenario */
	string m_AliensChangefreqYears; /*!< list of aliens introduction frequencies changes times */

	/* FG specific parameters */
	vector<string> m_FGLifeHistory; /*!< list of path to FG life history parameters files */
	vector<string> m_FGLight; /*!< list of path to FG light parameters files */
	vector<string> m_FGMapsHabSuit; /*!< list of path to FG habitat suitability maps */
	vector<string> m_FGDispersal; /*!< list of path to FG dispersal parameters files */
	vector<string> m_FGDisturbance; /*!< list of path to FG disturbance parameters files */
	vector<string> m_FGSoil; /*!< list of path to FG soil parameters files */
	vector<string> m_FGFire; /*!< list of path to FG fire disturbance parameters files */
	vector<string> m_FGDrought; /*!< list of path to FG drought disturbance parameters files */
	vector<string> m_FGMapsAliens; /*!< list of path to FG introduction points maps */


	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		ar & m_GlobSimulParams;
		ar & m_SavedState;
		ar & m_SavingDir;
		ar & m_SavingTimesMaps;
		ar & m_SavingTimesObjects;
		ar & m_Mask;
		ar & m_MaskDist;
		ar & m_MaskFire;
		ar & m_MaskDrought;
		ar & m_MaskElevation;
		ar & m_MaskSlope;
		ar & m_MaskChangemaskFiles;
		ar & m_MaskChangemaskYears;
		ar & m_HabSuitChangemaskFiles;
		ar & m_HabSuitChangemaskYears;
		ar & m_DistChangemaskFiles;
		ar & m_DistChangemaskYears;
		ar & m_FireChangemaskFiles;
		ar & m_FireChangemaskYears;
		ar & m_FireChangefreqFiles;
		ar & m_FireChangefreqYears;
		ar & m_DroughtChangemaskFiles;
		ar & m_DroughtChangemaskYears;
		ar & m_FGLifeHistory;
		ar & m_FGLight;
		ar & m_FGMapsHabSuit;
		ar & m_FGDispersal;
		ar & m_FGDisturbance;
		ar & m_FGSoil;
		ar & m_FGFire;
		ar & m_FGDrought;
		ar & m_FGMapsAliens;
		ar & m_AliensChangemaskFiles;
		ar & m_AliensChangemaskYears;
		ar & m_AliensChangefreqFiles;
		ar & m_AliensChangefreqYears;
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	FOPL default constructor => All parameters are set to 0, False or None
	 */
	FOPL();

	/*!
	 *	\brief Full constructor
	 *
	 *	FOPL full constructor
	 *
	 *	\param paramSimulFile : path to text file containing well-formatted
	 * simulation related parameters
	 */
	FOPL(string paramSimulFile);

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	FOPL destructor
	 */
	~FOPL();

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const FOPL& o) const
	{
		return (m_GlobSimulParams == o.m_GlobSimulParams &&
		m_SavedState == o.m_SavedState &&
		m_SavingDir == o.m_SavingDir &&
		m_SavingTimesMaps == o.m_SavingTimesMaps &&
		m_SavingTimesObjects == o.m_SavingTimesObjects &&
		m_Mask == o.m_Mask &&
		m_MaskDist == o.m_MaskDist &&
		m_MaskFire == o.m_MaskFire &&
		m_MaskDrought == o.m_MaskDrought &&
		m_MaskElevation == o.m_MaskElevation &&
		m_MaskSlope == o.m_MaskSlope &&
		m_MaskChangemaskFiles == o.m_MaskChangemaskFiles &&
		m_MaskChangemaskYears == o.m_MaskChangemaskYears &&
		m_HabSuitChangemaskFiles == o.m_HabSuitChangemaskFiles &&
		m_HabSuitChangemaskYears == o.m_HabSuitChangemaskYears &&
		m_DistChangemaskFiles == o.m_DistChangemaskFiles &&
		m_DistChangemaskYears == o.m_DistChangemaskYears &&
		m_FireChangemaskFiles == o.m_FireChangemaskFiles &&
		m_FireChangemaskYears == o.m_FireChangemaskYears &&
		m_FireChangefreqFiles == o.m_FireChangefreqFiles &&
		m_FireChangefreqYears == o.m_FireChangefreqYears &&
		m_DroughtChangemaskFiles == o.m_DroughtChangemaskFiles &&
		m_DroughtChangemaskYears == o.m_DroughtChangemaskYears &&
		m_FGLifeHistory == o.m_FGLifeHistory &&
		m_FGLight == o.m_FGLight &&
		m_FGMapsHabSuit == o.m_FGMapsHabSuit &&
		m_FGDispersal == o.m_FGDispersal &&
		m_FGDisturbance == o.m_FGDisturbance &&
		m_FGSoil == o.m_FGSoil &&
		m_FGFire == o.m_FGFire &&
		m_FGDrought == o.m_FGDrought &&
		m_FGMapsAliens == o.m_FGMapsAliens &&
		m_AliensChangemaskFiles == o.m_AliensChangemaskFiles &&
		m_AliensChangemaskYears == o.m_AliensChangemaskYears &&
		m_AliensChangefreqFiles == o.m_AliensChangefreqFiles &&
		m_AliensChangefreqYears == o.m_AliensChangefreqYears);
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	const string& getGlobSimulParams() const;
	const string& getSavedState() const;
	const string& getSavingDir() const;
	const string& getSavingTimesMaps() const;
	const string& getSavingTimesObjects() const;
	const string& getMask() const;
	const vector<string>& getMaskDist() const;
	const vector<string>& getMaskFire() const;
	const string& getMaskDrought() const;
	const string& getMaskElevation() const;
	const string& getMaskSlope() const;
	const vector<string>& getMaskChangemaskFiles() const;
	const string& getMaskChangemaskYears() const;
	const vector<string>& getHabSuitChangemaskFiles() const;
	const string& getHabSuitChangemaskYears() const;
	const vector<string>& getDistChangemaskFiles() const;
	const string& getDistChangemaskYears() const;
	const vector<string>& getFireChangemaskFiles() const;
	const string& getFireChangemaskYears() const;
	const vector<string>& getFireChangefreqFiles() const;
	const string& getFireChangefreqYears() const;
	const vector<string>& getDroughtChangemaskFiles() const;
	const string& getDroughtChangemaskYears() const;
	const vector<string>& getFGLifeHistory() const;
	const vector<string>& getFGLight() const;
	const vector<string>& getFGMapsHabSuit() const;
	const vector<string>& getFGDispersal() const;
	const vector<string>& getFGDisturbance() const;
	const vector<string>& getFGSoil() const;
	const vector<string>& getFGFire() const;
	const vector<string>& getFGDrought() const;
	const vector<string>& getFGMapsAliens() const;
	const vector<string>& getAliensChangemaskFiles() const;
	const string& getAliensChangemaskYears() const;
	const vector<string>& getAliensChangefreqFiles() const;
	const string& getAliensChangefreqYears() const;

	void setGlobSimulParams(const string& globSimulParams);
	void setSavedState(const string& savedState);
	void setSavingDir(const string& savingDir);
	void setSavingTimesMaps(const string& savingTimesMaps);
	void setSavingTimesObjects(const string& savingTimesObjects);
	void setMask(const string& mask);
	void setMaskDist(const vector<string>& maskDist);
	void setMaskFire(const vector<string>& maskFire);
	void setMaskDrought(const string& maskDrought);
	void setMaskElevation(const string& maskElevation);
	void setMaskSlope(const string& maskSlope);
	void setMaskChangemaskFiles(const vector<string>& maskChangemaskFiles);
	void setMaskChangemaskYears(const string& maskChangemaskYears);
	void setHabSuitChangemaskFiles(const vector<string>& habSuitChangemaskFiles);
	void setHabSuitChangemaskYears(const string& habSuitChangemaskYears);
	void setDistChangemaskFiles(const vector<string>& distChangemaskFiles);
	void setDistChangemaskYears(const string& distChangemaskYears);
	void setFireChangemaskFiles(const vector<string>& fireChangemaskFiles);
	void setFireChangemaskYears(const string& fireChangemaskYears);
	void setFireChangefreqFiles(const vector<string>& fireChangefreqFiles);
	void setFireChangefreqYears(const string& fireChangefreqYears);
	void setDroughtChangemaskFiles(const vector<string>& droughtChangemaskFiles);
	void setDroughtChangemaskYears(const string& droughtChangemaskYears);
	void setFGLifeHistory(const vector<string>& fgLifeHistory);
	void setFGLight(const vector<string>& fgLight);
	void setFGMapsHabSuit(const vector<string>& fgMapsHabSuit);
	void setFGDispersal(const vector<string>& fgDispersal);
	void setFGDisturbance(const vector<string>& fgDisturbance);
	void setFGSoil(const vector<string>& fgSoil);
	void setFGFire(const vector<string>& fgFire);
	void setFGDrought(const vector<string>& fgDrought);
	void setFGMapsAliens(const vector<string>& fgMapsAliens);
	void setAliensChangemaskFiles(const vector<string>& aliensChangemaskFiles);
	void setAliensChangemaskYears(const string& aliensChangemaskYears);
	void setAliensChangefreqFiles(const vector<string>& aliensChangefreqFiles);
	void setAliensChangefreqYears(const string& aliensChangefreqYears);

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	void show();

	/*!
	 *	\brief Check existence of parameter files retrieved : light competition
	 *
	 *	This function checks if all the files related to light competition module
	 * exist.
	 */
	void checkCorrectParams_light();

	/*!
	 *	\brief Check existence of parameter files retrieved : habitat suitability
	 *
	 *	This function checks if all the files related to habitat suitability
	 * module exist.
	 */
	void checkCorrectParams_habSuit();

	/*!
	 *	\brief Check existence of parameter files retrieved : dispersal
	 *
	 *	This function checks if all the files related to dispersal module exist.
	 */
	void checkCorrectParams_disp();

	/*!
	 *	\brief Check existence of parameter files retrieved : disturbances
	 *
	 *	This function checks if all the files related to disturbances module
	 * exist.
	 */
	void checkCorrectParams_dist();

	/*!
	 *	\brief Check existence of parameter files retrieved : soil competition
	 *
	 *	This function checks if all the files related to soil competition module
	 * exist.
	 */
	void checkCorrectParams_soil();

	/*!
	 *	\brief Check existence of parameter files retrieved : fire disturbances
	 *
	 *	This function checks if all the files related to fire disturbances module
	 * exist.
	 */
	void checkCorrectParams_fire();

	/*!
	 *	\brief Check existence of parameter files retrieved : drought disturbances
	 *
	 *	This function checks if all the files related to drought disturbances
	 * module exist.
	 */
	void checkCorrectParams_drought();

	/*!
	 *	\brief Check existence of parameter files retrieved : aliens introduction
	 *
	 *	This function checks if all the files related to aliens introduction
	 * module exist.
	 */
	void checkCorrectParams_aliens();


	/*!
	 *	\brief Check existence of parameter files retrieved
	 *
	 *	This function checks if all the files given within the simulation file
	 * exist. It is the generic function, and parameter files can be tagged as
	 * optional, so required values are for basic parameters, such as the ones
	 * related to simulation parametrization (e.g. GLOBAL_PARAMS) or demographic
	 * model (e.g. PFG_LIFE_HISTORY_PARAMS). It also checks if the folder
	 * organization is correct and creates missing folders (especially for
	 * results).
	 */
	void checkCorrectParams();

	/*!
	 *	\brief Check existence of parameter files retrieved
	 *
	 *	This function checks if all the files given within the simulation file
	 * exist. It takes into account which modules are activated or not, and
	 * hence which files should be checked for existence.
	 *
	 * \param doLight : is light competition module activated
	 * \param doHabSuit : is habitat suitability module activated
	 * \param doDisp : is dispersal module activated
	 * \param doDist : is disturbances module activated
	 * \param doSoil : is soil competition module activated
	 * \param doFire : is fire disturbances module activated
	 * \param doDrought : is drought disturbances module activated
	 * \param doAliens : is aliens introduction module activated
	 */
	void checkCorrectParams(const bool& doLight, const bool& doHabSuit, const bool& doDisp, const bool& doDist,
	const bool& doSoil, const bool& doFire, const bool& doDrought, const bool& doAliens);

	/*!
	 *	\brief Compare extension and coordinates of raster file with reference
	 *
	 *	This function checks if a given raster file has the same extension (tif
	 * or img) and the same coordinates (xmin, xmax, xres, xncell, ymin, ymax,
	 * yres, yncell) than a reference raster file (usually the one obtained from
	 * the tag --MASK-- within the simulation parameter file).
	 *
	 * \param param : name of the concerned parameter
	 * \param file_name : path to raster file to be checked
	 * \param ext_REF : extension of the reference file
	 * \param coord_REF : coordinates of the reference file
	 */
	void testSameCoord(const string& param, const string& file_name, const string& ext_REF, Coordinates<double>& coord_REF);

	/*!
	 *	\brief Compare extension and coordinates of raster file with reference
	 *
	 *	This function checks if a list of raster files have the same extension
	 * (tif or img) and the same coordinates (xmin, xmax, xres, xncell, ymin,
	 * ymax, yres, yncell) than a reference raster file (usually the one
	 * obtained from the tag --MASK-- within the simulation parameter file).
	 *
	 * \param param : name of the concerned parameter
	 * \param vector_name : a vector of paths to raster files to be checked
	 * \param ext_REF : extension of the reference file
	 * \param coord_REF : coordinates of the reference file
	 */
	void testSameCoord(const string& param, vector<string> vector_name, const string& ext_REF, Coordinates<double>& coord_REF);

	/*!
	 *	\brief Routine to compare extension and coordinates of raster file with
	 * reference
	 *
	 *	This function gets the information about the raster reference file (path,
	 * extension, coordinates) and compare them to all the raster files given
	 * within the simulation parameter file which will later be used in the
	 * simulation if the corresponding modules are activated (PFG_MASK_HABSUIT,
	 * DIST_MASK, FIRE_MASK, DROUGHT_MASK, ALIENS_MASK, FIRE_MASK, ELEVATION_MASK,
	 * SLOPE_MASK).
	 */
	void checkCorrectMasks();
};


BOOST_CLASS_VERSION(FOPL, 0)
#endif //FOPL_H
