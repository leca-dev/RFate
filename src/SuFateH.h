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
/*                                SuFateH Class                               */
/*============================================================================*/

/*!
 * \file SuFateH.h
 * \brief Succession (demographic model) with habitat suitability class
 * \author Damien Georges
 * \version 1.0
 * \date 2013/10/21
 */

#ifndef SUFATEH_H
#define SUFATEH_H

#include "SuFate.h"
#include <boost/serialization/base_object.hpp>

using namespace std;


 /*!
 * \class SuFateH
 * \brief Succession (demographic model) with habitat suitability class
 *
 * This class inherits from the SuFate class : it contains basic tools to
 * perform Fate succession. In addition, it is linked to habitat suitability
 * maps, allowing the model to weight demographic processes (such as
 * recruitment, fecundity, maturity age, ...) according to each FG preferences.
 */

 // TODO (damien#1#): Reimplement several way to influence demo parameters

class SuFateH : public SuFate
{
	protected :

	DoubleMapPtr m_EnvSuitMapPtr; /*!< pointer to habitat suitability maps */
	DoubleMapPtr m_EnvSuitRefMapPtr; /*!< pointer to stochastic environmental value consider as yearly reference */

	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar , const unsigned int /*version*/)
	{
		ar & boost::serialization::base_object<SuFate>(*this);
		ar & m_EnvSuitMapPtr; // ALREADY SAVED in SimulMap.h
		ar & m_EnvSuitRefMapPtr; // ALREADY SAVED in SimulMap.h
	}

	public :

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	SuFateH default constructor => All parameters are set to 0, False or None
	 */
	SuFateH();

	/*!
	 *	\brief Semi-default constructor (null community and resources)
	 *
	 *	SuFateH semi-default constructor (null community and resources)
	 *
	 *	\param cellID : id of pixel this succession model is linked to
	 */
	SuFateH(unsigned cellID);

	/*!
	 *	\brief Full constructor
	 *
	 *	SuFateH full constructor
	 *
	 *	\param cellID : id of pixel this succession model is linked to
	 * \param comm : community of the pixel
	 * \param lightR : light resources of the pixel
	 * \param soilR : soil resources of the pixel
	 * \param seedRainMap : pointer to the simulation seeds rain maps
	 * \param SeedProdMap : pointer to the simulation produced seeds maps
	 * \param gspPtr : pointer to GSP class object containing global simulation
	 * related parameters, and modules specific (e.g number of strata, number of
	 * disturbances...)
	 * \param envSuitMapPtr : pointer to SpatialStack class object containing
	 * habitat suitability maps (between 0 and 1) for each FG
	 * \param envSuitRefMapPtr : pointer to SpatialStack class object containing
	 * habitat reference maps (between 0 and 1) for each FG
	 */
	SuFateH(unsigned cellID, Community comm, LightResources lightR, double soilR,
	IntMapPtr seedRainMap, IntMapPtr SeedProdMap, GSPPtr gspPtr,
	DoubleMapPtr envSuitMapPtr, DoubleMapPtr envSuitRefMapPtr);

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	SuFateH destructor
	 */
	~SuFateH();

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const SuFateH& o) const
	{
		return (SuFate::operator==(o) &&
		*m_EnvSuitMapPtr == *(o.m_EnvSuitMapPtr) &&
		*m_EnvSuitRefMapPtr == *(o.m_EnvSuitRefMapPtr));
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	SpatialStack<double, double> getEnvSuitMap();
	SpatialStack<double, double> getEnvSuitRefMap();
	double getEnvSuit(unsigned fg);

	using SuFate::getEnvSuitRefVal;
	double getEnvSuitRefVal(unsigned fg);

	DoubleMapPtr getEnvSuitMap_();
	DoubleMapPtr getEnvSuitRefMap_();

	virtual void setEnvSuitRefMap_( DoubleMapPtr envSuitRefVal_);


	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	virtual void show();

	/*-------------------------------------------*/

	/*!
	 *	\brief Get environmental influence on recruitment rate
	 *
	 * Recruitment rate can be influenced by environmental conditions if habitat
	 * suitability module is activated. The habitat suitability value of the PFG
	 * is compared to the habitat reference value of the current year : if it is
	 * above, there is no influence on environment, otherwise there is one.
	 *
	 *	\param fg : id of considered PFG
	 * \return : 1 (habSuit >= habRef) or 0 (habSuit < habRef)
	 */
	virtual double getEnvRecrRate(int fg);

	/*!
	 *	\brief Get environmental influence on mortality
	 *
	 * Mortality can be influenced by environmental conditions if habitat
	 * suitability module is activated. This is not the case here, so the
	 * influence is null.
	 * (Possibility would be to decrease the PFG lifespan.)
	 *
	 *	\param fg : id of considered PFG
	 * \return : 1 (no dependence on environment)
	 */
	virtual double getEnvMort(int fg);

	/*!
	 *	\brief Get environmental influence on growth
	 *
	 * Growth can be influenced by environmental conditions if habitat
	 * suitability module is activated. The habitat suitability value of the PFG
	 * is compared to the habitat reference value of the current year : if it is
	 * above, there is no influence on environment, otherwise there is one.
	 *
	 *	\param fg : id of considered PFG
	 * \return : 1 (habSuit >= habRef) or 0 (habSuit < habRef)
	 */
	virtual double getEnvGroth(int fg);

	/*!
	 *	\brief Get environmental influence on fecundity
	 *
	 * Fecundity can be influenced by environmental conditions if habitat
	 * suitability module is activated. The habitat suitability value of the PFG
	 * is compared to the habitat reference value of the current year : if it is
	 * above, there is no influence on environment, otherwise there is one.
	 *
	 *	\param fg : id of considered PFG
	 * \return : 1 (habSuit >= habRef) or 0 (habSuit < habRef)
	 */
	virtual double getEnvFecund(int fg);

	/*-------------------------------------------*/

	/*!
	 *	\brief Get seeding input
	 *
	 * During seeding period, if the PFG is not an introduced alien (invasive),
	 * a constant supply of seeds occurs, fixed for all PFG (and defined within
	 * the GSP object). The habitat suitability value of the PFG is compared to
	 * the habitat reference value of the current year : if it is above, there
	 * is no influence on environment, otherwise there is one.
	 *
	 *	\param fg : id of considered PFG
	 * \return : number of seeds that will be dispersed this year for this PFG
	 * (getSeedingInput (habSuit >= habRef) or 0 (habSuit < habRef))
	 */
	virtual int getSeedInput(int fg);

	/*!
	 *	\brief Get Maturity age influenced by environment
	 *
	 * Maturity age can be influenced by environmental conditions if habitat
	 * suitability module is activated. If there is an influence of environment
	 * on growth (getEnvGroth), then Maturity age is shifted to Lifespan.
	 *
	 *	\param fg : id of considered PFG
	 * \return : maturity age (no influence) or lifespan (influence) of the PFG
	 */
	int getMatTime(int fg);

	/*!
	 *	\brief Get Lifespan influenced by environment
	 *
	 * Lifespan can be influenced by environmental conditions if habitat
	 * suitability module is activated. If there is an influence of environment
	 * on mortality, then Lifespan is shifted to 0. As there is no change in
	 * mortality, the influence is null.
	 *
	 *	\param fg : id of considered PFG
	 * \return : lifespan of the PFG (no dependence on environment)
	 */
	int getLifeSpan(int fg);

};

BOOST_CLASS_VERSION(SuFateH, 0)
#endif // SUFATEH_H
