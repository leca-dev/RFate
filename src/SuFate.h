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
/*                                SuFate Class                                */
/*============================================================================*/

/*!
 * \file SuFate.h
 * \brief Succession (demographic model) class
 * \author Damien Georges, Maya Gueguen
 * \version 1.0
 * \date 2020
 */

#ifndef SUFATE_H
#define SUFATE_H

#include <numeric>
#include <boost/serialization/export.hpp>
#include "Community.h"
#include "LightResources.h"
#include "Logger.h"

typedef Community* CommunityPtr;
typedef LightResources* LightResourcesPtr;
typedef SpatialStack<double, double>* DoubleMapPtr;
typedef SpatialStack<double, int>* IntMapPtr;
typedef GSP* GSPPtr;
typedef FG* FGPtr;
typedef FuncGroup* FuncGroupPtr;
typedef Legion* LegionPtr;

 /*!
 * \class SuFate
 * \brief Succession (demographic model) class
 *
 * This class contains basic tools to perform Fate succession.
 * It is defined as a point model (one object in each unit of space) and is
 * considered as the basic / core module of the model. Other succession models
 * will inherit of this class.
 */

class SuFate
{
	protected:

	/* pixel attributes */
	int m_CellID; /*!< Cell grid id */

	/* community attributes */
	Community m_Comm; /*!< Vector of FG Communities : state of each FG population at one time in one space */
	
	/* light resources attributes */
	LightResources m_LightR; /*!< Light Resources state in each stratum */

	/* soil resources attribute */
	double m_SoilR; /*!< Soil resource in the pixel */

	/* Seeds attributes */
	IntMapPtr m_SeedRainMap; /*!< pointer to dispersed seeds maps == seed rain */
	IntMapPtr m_SeedProdMap; /*!< pointer to maps where produced seeds will be stored */

	GSPPtr m_GSP; /*!< pointer to global simulation parameters */

	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		ar & m_CellID;
		ar & m_Comm;
		ar & m_LightR;
		ar & m_SoilR;
		// ar & m_SeedRainMap; // ALREADY SAVED in SimulMap.h
		// ar & m_SeedProdMap; // ALREADY SAVED in SimulMap.h
		// ar & m_GSP; // ALREADY SAVED in SimulMap.h
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	SuFate default constructor => All parameters are set to 0, False or None
	 */
	SuFate();

	/*!
	 *	\brief Semi-default constructor (null community and resources)
	 *
	 *	SuFate semi-default constructor (null community and resources)
	 *
	 *	\param cellID : id of pixel this succession model is linked to
	 */
	SuFate(int cellID);

	/*!
	 *	\brief Full constructor
	 *
	 *	SuFate full constructor
	 *
	 * \param cellID : id of pixel this succession model is linked to
	 * \param comm : community of the pixel
	 * \param lightR : light resources of the pixel
	 * \param soilR : soil resources of the pixel
	 * \param seedRainMap : pointer to the simulation seeds rain maps
	 * \param SeedProdMap : pointer to the simulation produced seeds maps
	 * \param gspPtr : pointer to GSP class object containing global simulation
	 * related parameters, and modules specific (e.g number of strata, number of
	 * disturbances...)
	 */
	SuFate(int cellID, Community comm, LightResources lightR, double soilR,
	IntMapPtr seedRainMap, IntMapPtr SeedProdMap, GSPPtr gspPtr);

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	SuFate destructor
	 */
	virtual ~SuFate();

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const SuFate& o) const
	{
		return (m_CellID == o.m_CellID &&
		m_Comm == o.m_Comm &&
		m_LightR == o.m_LightR &&
		m_SoilR == o.m_SoilR &&
		*m_SeedRainMap == *(o.m_SeedRainMap) &&
		*m_SeedProdMap == *(o.m_SeedProdMap) &&
		*m_GSP == *(o.m_GSP));
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	int getCellID() const;
	const Community getCommunity() const;
	LightResources getLightResources();
	double getSoilResources();
	SpatialStack<double, int> getSeedRain();
	SpatialStack<double, int> getSeedProd();
	int getSeedRain(unsigned fg);
	int getSeedProd(unsigned fg);
	GSP getGSP();

	CommunityPtr getCommunity_();
	LightResourcesPtr getLightResources_();
	IntMapPtr getSeedRain_();
	IntMapPtr getSeedProd_();
	int* getSeedRain_(unsigned fg);
	int* getSeedProd_(unsigned fg);
	GSPPtr getGSP_();

	void setCommunity(const Community comm);
	void setLightResources(const LightResources lightR);
	void setSoilResources(double soilR);
	void setSeedRain(unsigned fg, int seedRain);
	void setSeedProd(unsigned fg, int seedProd);

	virtual double getEnvSuitRefVal(){ return 0.0;};
	virtual void setEnvSuitRefMap_( DoubleMapPtr /*envSuitRefVal_*/ ){};

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	virtual void show();

	/*-------------------------------------------*/

	/*!
	 *	\brief Calculate FG abundances per stratum to update light / soil values
	 *
	 *	Within each height stratum, FG abundances (theoretical number of
	 * individuals) are calculated, taking into account the relative size of
	 * mature and immature plants.
	 *
	 * For light :
	 * Light resources are calculated per stratum, according to the FG
	 * abundances and the thresholds defined in the GSP object :
	 * LIGHT_THRESH_MEDIUM and LIGHT_THRESH_LOW.
	 * If the cumulated abundance in one stratum is superior to one of these
	 * thresholds, the light resources in the stratum below will decrease.
	 * It is impossible to have more light in one stratum than in the upper
	 * stratum.
	 *
	 * For Soil :
	 * The soil condition is linked to the relative importance of each PFG
	 * within the pixel. It is calculated as the weighted mean of each
	 * theoretical maximum contribution of each PFG to soil resources.
	 * Weights are the relative abundance of each PFG within the pixel.
	 */
	void CalculateEnvironment();

	/*!
	 *	\brief Check PFG survival under light and/or soil conditions
	 *
	 * This function compares the light and/or soil resources of the pixel with
	 * the tolerance of each PFG. PFG which are not tolerant to the current
	 * conditions of the pixel are killed (removed) or reduced.
	 *
	 * For light :
	 * The check is done for each stratum. Individuals are treated according to
	 * their height, by converting their age according to the stratum break ages
	 * of the PFG. Individuals not tolerant to the light condition are killed
	 * and removed from the cohort.
	 *
	 * For soil :
	 * On the contrary to light, there is only one value for soil resources
	 * within a pixel, and PFG can tolerate it or not according to their
	 * lifestage (as it it the case for light). Individuals not tolerant to the
	 * soil condition are reduced in abundance (or can be killed if the
	 * reduction rate is of 100%).
	 *
	 * Finally, once all cohorts have been covered, the pickupCohorts function
	 * is called to merge adjacent cohorts to save memory and time.
	 */
	void CheckSurvival();

	/*-------------------------------------------*/

	/*!
	 *	\brief Get environmental influence on recruitment rate
	 *
	 * Recruitment rate can be influenced by environmental conditions if habitat
	 * suitability module is activated. This is not the case here, so the
	 * influence is null.
	 *
	 *	\param fg : id of considered PFG
	 * \return : 1 (no dependence on environment)
	 */
	virtual double getEnvRecrRate(int fg);

	/*!
	 *	\brief Get environmental influence on mortality
	 *
	 * Mortality can be influenced by environmental conditions if habitat
	 * suitability module is activated. This is not the case here, so the
	 * influence is null.
	 *
	 *	\param fg : id of considered PFG
	 * \return : 1 (no dependence on environment)
	 */
	virtual double getEnvMort(int fg);

	/*!
	 *	\brief Get environmental influence on growth
	 *
	 * Growth can be influenced by environmental conditions if habitat
	 * suitability module is activated. This is not the case here, so the
	 * influence is null.
	 *
	 *	\param fg : id of considered PFG
	 * \return : 1 (no dependence on environment)
	 */
	virtual double getEnvGroth(int fg);

	/*!
	 *	\brief Get environmental influence on fecundity
	 *
	 * Fecundity can be influenced by environmental conditions if habitat
	 * suitability module is activated. This is not the case here, so the
	 * influence is null.
	 *
	 *	\param fg : id of considered PFG
	 * \return : 1 (no dependence on environment)
	 */
	virtual double getEnvFecund(int fg);

	/*-------------------------------------------*/

	/*!
	 *	\brief Calculate recruitment : number of plants that will be born this
	 * year for a given PFG
	 *
	 * The PFG recruitment is represented as a theoretical number of
	 * individuals. Each PFG has a potential number of individuals to be born
	 * each year, or number of available seeds. This number is represented by
	 * GerminRate, and can be partially reduced if light and/or soil conditions
	 * are not good. This potential number is weighted by the maximum abundance
	 * of the PFG, divided by its theoretical number of mature years (Lifespan -
	 * Maturity age).
	 *
	 *	\param fg : id of considered PFG
	 * \param GerminRate : PFG germination rate
	 * \return : number of individuals that will be born this year for this PFG
	 */
	int Recruitment( int fg, double GerminRate );

	/*-------------------------------------------*/

	/*!
	 *	\brief Calculate fecundity : amount of seeds that will be produced this
	 * year for a given PFG
	 *
	 * The PFG fecundity is represented as a theoretical number of seeds. Each
	 * PFG has a potential number of seeds to be produced each year (Potential
	 * Fecund). This number is weighted (understand that it can only be
	 * decreased) by an evaluation of the importance of mature individuals for
	 * the current year compared to its maximal potential through time.
	 * This number is represented by the total abundance of mature individuals
	 * (between Maturity age and Lifespan) ; divided by the maximum abundance of
	 * the PFG, divided by its theoretical number of mature years (Lifespan -
	 * Maturity age).
	 *
	 *	\param fg : id of considered PFG
	 * \return : number of seeds that will be produced this year for this PFG
	 */
	double calcFecund(int fg);

	/*!
	 *	\brief Get seeding input
	 *
	 * During seeding period, if the PFG is not an introduced alien (invasive),
	 * a constant supply of seeds occurs, fixed for all PFG (and defined within
	 * the GSP object).
	 *
	 *	\param fg : id of considered PFG
	 * \return : number of seeds that will be dispersed this year for this PFG
	 */
	virtual int getSeedInput(int fg);

	/*!
	 *	\brief Get Maturity age influenced by environment
	 *
	 * Maturity age can be influenced by environmental conditions if habitat
	 * suitability module is activated. This is not the case here, so the
	 * influence is null.
	 *
	 *	\param fg : id of considered PFG
	 * \return : maturity age of the PFG (no dependence on environment)
	 */
	int getMatTime(int fg);

	/*!
	 *	\brief Get Lifespan influenced by environment
	 *
	 * Lifespan can be influenced by environmental conditions if habitat
	 * suitability module is activated. This is not the case here, so the
	 * influence is null.
	 *
	 *	\param fg : id of considered PFG
	 * \return : lifespan of the PFG (no dependence on environment)
	 */
	int getLifeSpan(int fg);

	/*-------------------------------------------*/

	/*!
	 *	\brief Do annual step of demographic FATE model : STEP 1
	 *
	 * This is the main succession routine. It has been coded for clarity as
	 * opposed to speed; it works by simulating forward in single time steps.
	 *
	 * In each time step the procedure is :
	 * 1. CheckSurvival : check the tolerance of established plants, and kill
	 *    those that do not tolerate current light and/or soil conditions
	 * 2. ageLegions : age established plants
	 * 3. CalculateEnvironment : update the plant abundances, and calculate the
	 *    environmental conditions (light, soil)
	 * 4. DoSuccessionPart2
	 * 5. SimplifyLegions : round up cohort sizes
	 *
	 *	\param isDrought : vector of boolean indicating for each PFG is drought
	 * effect must be modelled
	 */
	void DoSuccessionPart1(vector<int> isDrought);

	/*!
	 *	\brief Do annual step of demographic FATE model : STEP 2
	 *
	 * For each PFG :
	 * 4a. Get soil resources
	 * 4b. getSeedInput : determine the size of the propagules rain
	 * 4c. Determine the germination rate from the number of seeds available for
	 *     germination and the environmental conditions (light, soil)
	 * 4d. If the species has pulse germination, empty the active seed pool
	 * 4e. AgePool1 : age the seed pools
	 * 4f. PutSeedInPool : place the propagules in the pools to ensure that
	 *     shortlived propagules at least survive through disturbances
	 * 4g. Do recruitement if germinants are able to withstand conditions
	 * 4i. calcFecund : update fecundity
	 *
	 *	\param isDrought : vector of boolean indicating for each PFG is drought
	 * effect must be modelled
	 */
	void DoSuccessionPart2(vector<int> isDrought);

	/*-------------------------------------------*/

	/*!
	 *	\brief Deal with unaffected individuals under disturbance
	 *
	 * This function deals with PFG which are unaffected by a disturbance.
	 * Given the percentages of unaffected plants, cohorts are reduced or cut
	 * accordingly.
	 *
	 *	\param fg : id of considered PFG
	 *	\param Dstb : id of considered disturbance
	 *	\param FGresp : FG response depending on the type of perturbation
	 * ("dist", "fire", "drought")
	 */
	void DoUnaffected(int fg, int Dstb, FGresponse FGresp);

	/*!
	 *	\brief Check PFG survival under disturbance
	 *
	 * This function deals with PFG which are not tolerant to a disturbance.
	 * Four stages are considered : PFG resprouting, PFG killed, propagules
	 * killed, and PFG unaffected.
	 *
	 *	\param fg : id of considered PFG
	 *	\param Dstb : id of considered disturbance
	 *	\param Dstb_val : pixel value of considered disturbance
	 *	\param FGresp : FG response depending on the type of perturbation
	 * ("dist", "fire", "drought")
	 */
	void DoDisturbance(int fg, int Dstb, double Dstb_val, FGresponse FGresp);
	void DoDisturbance(int Dstb, double Dstb_val);
	
};

BOOST_CLASS_VERSION(SuFate, 0)
#endif //SUFATE
