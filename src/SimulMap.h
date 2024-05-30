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
/*                            Simulation Map Class                            */
/*============================================================================*/

/*!
 * \file SimulMap.h
 * \brief Spatial simulation map
 * \author Damien Georges, Maya Gueguen
 * \version 1.0
 * \date 2020
 */

#ifndef SIMULMAP_H
#define SIMULMAP_H

#include "SuFateH.h"
#include "Disp.h"
#include "GlobalSimulParameters.h"
#include "FilesOfParamsList.h"
#include "FGUtils.h"
#include "Logger.h"

#include <iostream>
#include <string>
#include <sstream>
#include "openmp.h"

typedef SuFate* SuFatePtr;
using namespace std;


/*!
 * \class SimulMap
 * \brief Spatial simulation map
 *
 * This class is the main brick of a FATE simulation.
 * It gathers all simulation parameters (GSP, FOPL), functional group
 * parameters (FG, FGresponse, ...) and spatial information (coordinates, mask,
 * ...). All spatial objects have the same coordinates and extent.
 *
 * A mask of simulation map differentiates study/non-study pixels. To each
 * pixel is assigned a succession model. All succession models are linked to a
 * SpatialStack of seed maps, insuring communication between demographic and
 * dispersal model (if activated). A SpatialStack of habitat suitability maps
 * is also defined if the module is activated and linked as well to all
 * succession models.
 * In other words, each pixel has a succession model (SuFate), which can be
 * linked to habitat suitability maps (SuFateH). Disturbances, if activated,
 * are also supported at the pixel level.
 */

class SimulMap
{
	private:

	GSP m_glob_params; /*!< Object containing simulation parameters */
	vector<FG> m_FGparams; /*!< List of FG parameters objects*/

	Coordinates<double> m_Coord; /*!< Coordinates of study area */
	SpatialMap<double, int> m_Mask; /*!< Map referencing if a point belong (1) or not (0) to the studied area  */
	vector<unsigned> m_MaskCells; /*!< List of the cells belonging to the studied area */
	SpatialStack<double, int> m_SeedMapIn; /*!< Map of seeds produced at the end of succession step that will be dispersed	*/
	SpatialStack<double, int> m_SeedMapOut; /*!< Map of dispersed seeds == succession seed rain*/
	SpatialStack<double, double> m_EnvSuitMap; /*!< Stack of FG environmental suitability maps */
	SpatialStack<double, double> m_EnvSuitRefMap; /*!< Environmental suitability reference maps for current year */

	SpatialStack<double, double> m_DistMap; /*!< Stack of disturbances mask */
	SpatialStack<double, int> m_FireMap; /*!< Stack of disturbances mask */
	SpatialMap<double, int> m_TslfMap; /*!< Map referencing the Time Since Last Fire (TSLF) in each cell  */
	SpatialMap<double, double> m_DroughtMap; /*!< Moisture index mask  */
	SpatialMap<double, double> m_ElevationMap; /*!< Elevation mask  */
	SpatialMap<double, double> m_SlopeMap; /*!< Slope mask  */
	SpatialStack<double, unsigned> m_PostDroughtMap; /*!< Stack of maps referencing for each FG if a point will suffer from post drought effects (0 or 1) */
	SpatialStack<double, int> m_CountDroughtMap; /*!< Stack of maps referencing for each FG the number of consecutive years with drought */
	SpatialStack<double, unsigned> m_IsDroughtMap; /*!< Stack of maps referencing for each FG if a point suffered from drought this year (0 or 1) */
	SpatialStack<double, unsigned> m_ApplyCurrDroughtMap; /*!< Stack of maps referencing for each FG if a point will suffer from current drought effects (0 or 1) this year */
	SpatialStack<double, unsigned> m_ApplyPostDroughtMap; /*!< Stack of maps referencing for each FG if a point will suffer from post drought effects (0 or 1) this year */
	SpatialStack<double, double> m_CondInitMap; /*!< Stack of aliens introduction mask */

	SpatialMap<double, SuFatePtr> m_SuccModelMap; /*!< Map of succession models ( stored a pointers ) */

	Disp m_DispModel; /*!< Seed dispersal model */


	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class SuFate;
	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		logg.info(">> Saving/Loading GLOBAL PARAMETERS...");
		ar & m_glob_params;
		logg.info(">> Saving/Loading FG PARAMETERS...");
		ar & m_FGparams;
		logg.info(">> Saving/Loading COORDINATES...");
		ar & m_Coord;
		logg.info(">> Saving/Loading MASK...");
		ar & m_Mask;
		logg.info(">> Saving/Loading MASK CELLS...");
		ar & m_MaskCells;
		logg.info(">> Saving/Loading SEED MAP IN...");
		ar & m_SeedMapIn;
		logg.info(">> Saving/Loading SEED MAP OUT...");
		ar & m_SeedMapOut;
		logg.info(">> Saving/Loading HABITAT SUITABILITY MAPS...");
		ar & m_EnvSuitMap;
		logg.info(">> Saving/Loading HABITAT SUITABILITY REFERENCE MAPS...");
		ar & m_EnvSuitRefMap;
		logg.info(">> Saving/Loading DISTURBANCE MAPS...");
		ar & m_DistMap;
		logg.info(">> Saving/Loading FIRE MAPS...");
		ar & m_FireMap;
		ar & m_TslfMap;
		logg.info(">> Saving/Loading DROUGHT MAPS...");
		ar & m_DroughtMap;
		ar & m_ElevationMap;
		ar & m_SlopeMap;
		ar & m_PostDroughtMap;
		ar & m_CountDroughtMap;
		ar & m_IsDroughtMap;
		ar & m_ApplyCurrDroughtMap;
		ar & m_ApplyPostDroughtMap;
		logg.info(">> Saving/Loading ALIEN MAPS...");
		ar & m_CondInitMap;
		logg.info(">> Saving/Loading SUCCESSION MODEL MAP...");
		ar & m_SuccModelMap;
		logg.info(">> Saving/Loading DISPERSAL MODEL...");
		ar & m_DispModel;
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	SimulMap default constructor => All parameters are set to 0, False or None
	 */
	SimulMap();

	/*!
	 *	\brief Full constructor
	 *
	 *	SimulMap full constructor
	 *
	 *	\param file_of_params : FOPL class object containing path to text files
	 * containing well-formatted PFG (LIFE_HISTORY, LIGHT, SOIL, DIST, DISP) or
	 * simulation (MASK, SAVING_DIR, GLOBAL_PARAMETERS, ...) related parameters
	 */
	SimulMap(FOPL file_of_params);

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	SimulMap destructor
	 */
	~SimulMap();

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(SimulMap& o)
	{
		/* check equality between all simple elements */
		bool is_equal = (m_glob_params == o.m_glob_params &&
		m_FGparams == o.m_FGparams &&
		m_Coord == o.m_Coord &&
		m_Mask == o.m_Mask &&
		m_MaskCells == o.m_MaskCells &&
		m_SeedMapIn == o.m_SeedMapIn &&
		m_SeedMapOut == o.m_SeedMapOut &&
		m_EnvSuitMap == o.m_EnvSuitMap &&
		m_EnvSuitRefMap == o.m_EnvSuitRefMap &&
		m_DistMap == o.m_DistMap &&
		m_FireMap == o.m_FireMap &&
		m_TslfMap == o.m_TslfMap &&
		m_ElevationMap == o.m_ElevationMap &&
		m_SlopeMap == o.m_SlopeMap &&
		m_DroughtMap == o.m_DroughtMap &&
		m_PostDroughtMap == o.m_PostDroughtMap &&
		m_CountDroughtMap == o.m_CountDroughtMap &&
		m_IsDroughtMap == o.m_IsDroughtMap &&
		m_ApplyCurrDroughtMap == o.m_ApplyCurrDroughtMap &&
		m_ApplyPostDroughtMap == o.m_ApplyPostDroughtMap &&
		m_CondInitMap == o.m_CondInitMap &&
		m_DispModel == o.m_DispModel);

		/* Compare succession models maps */
		if (is_equal)
		{ // don't do it if there is yet some differences
			is_equal = is_equal && ( *(m_SuccModelMap.getCoordinates()) == *( o.m_SuccModelMap.getCoordinates() ) );
			omp_set_num_threads( m_glob_params.getNoCPU() );
			#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
			for (unsigned i=0; i<m_SuccModelMap.getTotncell(); i++)
			{
				is_equal = is_equal && ( *(m_SuccModelMap(i)) == *(o.m_SuccModelMap(i)) );
			}
		}
		return is_equal;
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	GSP& getGlobalParameters();
	vector<FG>& getFGparams();
	Coordinates<double>& getCoord();
	SpatialMap<double, int>& getMask();
	vector<unsigned>& getMaskCells();
	SpatialStack<double, int>& getSeedMapIn();
	SpatialStack<double, int>& getSeedMapOut();
	SpatialStack<double, double>& getEnvSuitMap();
	SpatialStack<double, double>& getEnvSuitRefMap();
	SpatialStack<double, double>& getDistMap();
	SpatialStack<double, int>& getFireMap();
	SpatialMap<double, int>& getTslfMap();
	SpatialMap<double, double>& getElevationMap();
	SpatialMap<double, double>& getSlopeMap();
	SpatialMap<double, double>& getDroughtMap();
	SpatialStack<double, unsigned>& getPostDroughtMap();
	SpatialStack<double, int>& getCountDroughtMap();
	SpatialStack<double, unsigned>& getIsDroughtMap();
	SpatialStack<double, unsigned>& getApplyCurrDroughtMap();
	SpatialStack<double, unsigned>& getApplyPostDroughtMap();
	SpatialStack<double, double>& getCondInitMap();
	SpatialMap<double, SuFatePtr>& getSuccModelMap();
	Disp& getDispModel();

	void setGlobalParameters(GSP globalParameters);
	void setFGparams(vector<FG> FGparams);
	void setCoord(Coordinates<double> coord);
	void setMask(SpatialMap<double, int> mask);
	void setMaskCells(vector<unsigned> maskCells);
	void setSeedMapIn(SpatialStack<double, int> seedMapIn);
	void setSeedMapOut(SpatialStack<double, int> seedMapOut);
	void setEnvSuitMap(SpatialStack<double, double> envSuitMap);
	void setEnvSuitRefMap(SpatialStack<double, double> envSuitRefMap);
	void setDistMap(SpatialStack<double, double> distMap);
	void setFireMap(SpatialStack<double, int> fireMap);
	void setTslfMap(SpatialMap<double, int> tslfMap);
	void setElevationMap(SpatialMap<double, double> elevationMap);
	void setSlopeMap(SpatialMap<double, double> slopeMap);
	void setDroughtMap(SpatialMap<double, double> droughtMap);
	void setPostDroughtMap(SpatialStack<double, unsigned> postDroughtMap);
	void setCountDroughtMap(SpatialStack<double, int> countDroughtMap);
	void setIsDroughtMap(SpatialStack<double, unsigned> isDroughtMap);
	void setApplyCurrDroughtMap(SpatialStack<double, unsigned> applyCurrDroughtMap);
	void setApplyPostDroughtMap(SpatialStack<double, unsigned> applyPostDroughtMap);
	void setCondInitMap(SpatialStack<double, double> condInitMap);
	void setSuccModelMap(SpatialMap<double, SuFatePtr> succModelMap);
	void setDispModel(Disp dispModel);

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Start seeding process
	 *
	 *	This function activates the production of maximal quantity of seeds
	 * (defined by SEEDING_INPUT in GSP) in all pixels for each PFG.
	 */
	void StartSeeding();

	/*!
	 *	\brief Start seeding process
	 *
	 *	This function desactivates the production of maximal quantity of seeds
	 * (defined by SEEDING_INPUT in GSP) in all pixels for each PFG.
	 * Seeds will only be produced by currently living and mature individuals.
	 */
	void StopSeeding();

	/*-------------------------------------------*/

	/*!
	 *	\brief Change reference maps
	 *
	 *	This function will update all mask maps according to paths to raster
	 * files. Be sure that the order of maps is coherent with the original order
	 * of disturbances or PFG.
	 *
	 * \param newChangeFile : path to a text file containing a list of path to
	 * raster file(s) (.img or .tif), one line for each new file. Be careful to
	 * the order of those files.
	 * \param typeFile : string indicating the type of files to be changed, and
	 * hence the number of expected file(s) and the type of expected values
	 *   "mask" for simulation map
	 *   "habSuit" for habitat suitability maps
	 *   "dist" for disturbance maps
	 *   "fire" for fire disturbance maps
	 *   "drought" for moisture index maps used for drought module
	 *   "aliens" for introduction maps used for aliens module
	 */
	void DoFileChange(string newChangeFile, string typeFile);

	/*!
	 *	\brief Change reference frequencies
	 *
	 *	This function will update all event frequencies.
	 * Be sure that the new number of frequencies is equal to the number of
	 * disturbances of PFG.
	 *
	 * \param freqChangeFile : path to a text file containing a vector of new
	 * event frequencies, one line for each new year. Be careful to the order of
	 * those values.
	 * \param typeFile : string indicating the type of files to be changed, and
	 * hence the number of expected file(s) and the type of expected values
	 *   "fire" for fire disturbance
	 *   "aliens" for aliens module
	 */
	void DoFreqChange(string freqChangeFile, string typeFile);

	/*-------------------------------------------*/

	/*!
	 *	\brief Do succession (demographic) model within each pixel
	 *
	 *	This function runs DoSuccessionPart1 function of each SuFate or SuFateH
	 * model within each study pixel.
	 */
	void DoSuccession();

	/*-------------------------------------------*/

	/*!
	 *	\brief Do dispersal model within each pixel
	 *
	 *	This function runs DoDispersalPacket function of dispersal model.
	 * It takes seeds produced by each FG (SeedMapOut) and disperses them all
	 * over the landscape according to the selected dispersal process. Dispersed
	 * seeds are stored (SeedMapIn) and will constitute the seed rain of the
	 * following year.
	 */
	void DoDispersal();

	/*-------------------------------------------*/

	/*!
	 *	\brief Do ignition of fire disturbance model
	 *
	 *	This function finds cells in which a fire disturbance will start,
	 * according to the selected fire model (random distribution, ChaoLi
	 * probability).
	 *
	 * \param dist : id of considered disturbance
	 * \param availCells : vector of cells that can be impacted by the disturbance
	 */
	vector<unsigned> DoIgnition(int dist, vector<unsigned> availCells);

	/*!
	 *	\brief Do propagation of fire disturbance model
	 *
	 *	This function finds cells in which a fire disturbance will propagate,
	 * starting to previously identified cells (start) and according to the
	 * selected propagation model (probability dependent, based on neighbouring
	 * cells, max amount of fuel, LandClim, ...).
	 *
	 * \param dist : id of considered disturbance
	 * \param start : vector of cells where there is an ignition of fire
	 * \param availCells : vector of cells that can be impacted by the disturbance
	 */
	vector<unsigned> DoPropagation(int dist, vector<unsigned> start, vector<unsigned> availCells);

	/*!
	 *	\brief Update the TimeSinceLastFire mask
	 *
	 *	This function fills a map counting in each cell since when has a fire not
	 * occurred. Pixels impacted this year (burnt) are set to 0, others are
	 * incremented by one.
	 *
	 * \param burnt : vector of cells impacted by the disturbance
	 */
	void DoUpdateTslf(vector<unsigned> burnt);

	/*!
	 *	\brief Apply fire disturbance model
	 *
	 *	This function defines, if a fire disturbance should occur this year,
	 * the ignition cells, does the propagation of fire, and applies the
	 * DoDisturbance function of each SuFate or SuFateH model within each
	 * impacted study pixel.
	 *
	 * \param yr : current year of simulation
	 */
	void DoFireDisturbance(int yr);

	/*-------------------------------------------*/

	/*!
	 *	\brief Apply drought disturbance model
	 *
	 *	This function updates 5 different maps accounting for the drought
	 * process :
	 *   - IsDroughtMap : is there or not drought this year
	 *   - ApplyPostDroughtMap : do drought effects occur next year
	 *   - ApplyCurrDroughtMap : do drought effects occur this year
	 *   - PostDroughtMap : is there post drought mortality
	 *   - CountDroughtMap : how many cumulated years of drought
	 */
	void DoDroughtDisturbance_part1();

	/*!
	 *	\brief Apply drought disturbance model
	 *
	 *	This function defines, if a drought disturbance should occur this year,
	 * the impacted cells and applies the DoDisturbance function of each SuFate
	 * or SuFateH model within each impacted study pixel.
	 *
	 * \param chrono : string indicating if drought effects that should be
	 * applied are before ("prev") or after ("post") succession
	 */
	void DoDroughtDisturbance_part2(string chrono);

	/*-------------------------------------------*/

	/*!
	 *	\brief Apply disturbance model
	 *
	 *	This function defines, if a disturbance should occur this year, the
	 * impacted cells and applies the DoDisturbance function of each SuFate
	 * or SuFateH model within each impacted study pixel.
	 *
	 * \param yr : current year of simulation
	 */
	void DoDisturbance(int yr);

	/*-------------------------------------------*/

	/*!
	 *	\brief Apply aliens introduction model
	 *
	 *	This function activates, if invasive introduction should occur this year,
	 * the production of maximal quantity of seeds (defined by SEEDING_INPUT in
	 * GSP) in pixels defined by alien mask for each alien PFG.
	 *
	 * \param yr : current year of simulation
	 */
	void DoAliensIntroduction(int yr);

	/*-------------------------------------------*/

	/*!
	 *	\brief Update the current year environmental reference value
	 *
	 *	This function assigns a number between 0 and 1 to each pixel, and for
	 * each PFG. This number will be the new reference for habitat suitability
	 * of the coming year. It is an index of how the following year will be
	 * stressful or not for each PFG.
	 * Two methods are available to draw these numbers :
	 *   - 1 : for each pixel, a number is drawn from uniform distribution, the
	 *         same for all PFG within this pixel
	 *   - 2 : for each PFG, two values are drawn from uniform distribution to
	 *         represent mean and standard deviation. Then, for each pixel, a
	 *         number is drawn from a normal distribution defined by these two
	 *         values.
	 *
	 * \param option : 1 (random) or 2 (PFG distribution)
	 */
	void UpdateEnvSuitRefMap(unsigned option);

	/*!
	 *	\brief Update simulation parameters when starting from backup
	 *
	 *	This function checks and updates simulation parameters when a simulation
	 * is started from the outputs of a previous run (SAVED_STATE), in case some
	 * parameters have changed (e.g. SIMULATION_DURATION, MASK, ...).
	 *
	 * \param file_of_params : FOPL class object containing path to text files
	 * containing well-formatted PFG (LIFE_HISTORY, LIGHT, SOIL, DIST, DISP) or
	 * simulation (MASK, SAVING_DIR, GLOBAL_PARAMETERS, ...) related parameters
	 */
	void UpdateSimulationParameters(FOPL file_of_params);

	/*-------------------------------------------*/

	/*!
	 *	\brief Save simulation outputs into raster files
	 *
	 *	This function saves as raster files the different outputs produced by a
	 * FATE simulation :
	 *   - abundances per PFG and per stratum
	 *   - abundances per PFG for all strata
	 *   - light resources (if light interaction activated)
	 *   - soil resources (if soil interaction activated)
	 *
	 * \param saveDir : string with the simulation results folder path
	 * \param year : current year of simulation
	 * \param prevFile : path to mask raster file to initiate saved rasters
	 */
	void SaveRasterAbund(string saveDir, int year, string prevFile);

};

BOOST_CLASS_VERSION(SimulMap, 0)
#endif //MAP_H
