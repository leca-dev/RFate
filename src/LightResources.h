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
/*                            Light Resources Class                           */
/*============================================================================*/

/*!
 * \file LightResources.h
 * \brief Structure to store available light resources in each stratum
 * \author Damien Georges
 * \version 1.0
 * \date 2013/10/21
 */

#ifndef LIGHTRESOURCES_H
#define LIGHTRESOURCES_H

#include "FGUtils.h"
#include "Logger.h"

using namespace std;


/*!
 * \class LightResources
 * \brief Structure to store available light resources in each stratum
 *
 * This object stores available light resources in each stratum of a given
 * pixel. It is represented by a vector of size the number of strata, and
 * filled with values of enum Resource (RLow, RMedium or RHigh).
 */

class LightResources
{
	private:

	vector< Resource > m_ResourceList; /*!< List of light resources */

	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		ar & m_ResourceList;
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	LightResources default constructor => All parameters are set to 0, False or None
	 */
	LightResources();

	/*!
	 *	\brief Full constructor
	 *
	 *	LightResources full constructor
	 *
	 *	\param noStrata : number of height strata
	 */
	LightResources(int noStrata);

	/*!
	 *	\brief Full constructor
	 *
	 *	LightResources full constructor
	 *
	 *	\param resourceList : vector of light resources
	 */
	LightResources(vector<Resource> resourceList);

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	LightResources destructor
	 */
	virtual ~LightResources();

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const LightResources& o) const
	{
		return ( m_ResourceList == o.m_ResourceList );
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	const vector<Resource>& getResourceList() const;
	const Resource& getResource(const int& id) const;

	void setResourceList(const vector<Resource>& resourceList);
	void setResource(const int& id, const Resource& resource);

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	void show();

};

BOOST_CLASS_VERSION(LightResources, 0)
#endif //LIGHTRESOURCES_H
