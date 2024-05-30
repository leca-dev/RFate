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
/*                        Plant Functional Group Class                        */
/*============================================================================*/

/*!
 * \file FuncGroup.h
 * \brief Plant Functional Group Class
 * \author Damien Georges
 * \version 1.0
 * \date 2013/10/21
 */

#ifndef FUNCGROUP_H
#define FUNCGROUP_H

#include "FG.h"
#include "PropPool.h"
#include "Legion.h"
#include "Logger.h"

typedef FG* FGPtr;
using namespace std;


/*!
 * \class FuncGroup
 * \brief Plant Functional Group Class
 *
 * A Functional Group is defined by a vector of (two) Propagules Pools (active
 * and dormant) and a Legion object, which is a list of cohorts containing the
 * abundances of all individuals of specific ages.
 * It is defined for one particular group of plants (FG) having the same life
 * attributes.
 * This Functional Group may be brought together with other Functional Groups
 * to form a Community.
 */

class FuncGroup
{
	private:

	vector<PropPool> m_Pools; /*!< Propagules pool array (active & dormant seed pool) */
	Legion m_LList; /*!< FG legion list */
	FGPtr m_FGparams; /*!< FG parameters */

	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		ar & m_Pools;
		ar & m_LList;
		ar & m_FGparams;
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	*	\brief Constructor
	*
	*	FuncGroup default constructor => All parameters are set to 0, False or None
	*/
	FuncGroup();

	/*!
	*	\brief Constructor
	*
	*	FuncGroup standard constructor based only on a given FG object
	*
	*	\param fgparams : pointer to a FG object
	*/
	FuncGroup(FGPtr fgparams);

	/*!
	*	\brief Constructor
	*
	*	FuncGroup full constructor
	*
	*	\param pools : vector containing two PropPool objects
	*	\param llist : PFG Legion List
	*	\param fgparams : pointer to a FG object
	*/
	FuncGroup(const vector<PropPool>& pools, const Legion& llist, FGPtr fgparams);

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	*	\brief Destructor
	*
	*	FuncGroup  destructor
	*
	*	\param fg : the FuncGroup object to copy
	*/
	virtual ~FuncGroup();

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const FuncGroup& o) const
	{
		return ( m_Pools == o.m_Pools && m_LList == o.m_LList && *m_FGparams == *(o.m_FGparams) );
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	const vector<PropPool>& getPools() const;
	const PropPool& getPools(const PoolType& pt) const;
	const Legion& getLList() const;
	const FG& getFGparams() const;

	Legion* getLList_();
	PropPool* getPools_(const PoolType& pt);
	FGPtr getFGparams_();

	void setPools( const vector<PropPool>& pools);
	void setPools( const PropPool& pool, const PoolType& pt );
	void setLList( const Legion llist );
	void setFGparams( FGPtr fgparams );

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	void show();
	void summary();

	/*!
	 *	\brief Calculate abundance of individuals between two ages
	 *
	 *	This function returns the abundance of individuals of a Legion list
	 * between two given ages. It considers relative size of mature and immature
	 * individuals (abundMature + abundImmature * ImmSize).
	 *
	 *	\param Age0 : age of the youngest individuals
	 *	\param Age1 : age of the oldest individuals
	 * \return : the number of individuals within the legion between two ages
	 */
	int totalNumAbund(int Age0, int Age1);

	/*!
	 *	\brief Calculate abundance of the complete legion
	 *
	 *	This function returns the abundance of individuals of a Legion list. It
	 * considers relative size of mature and immature individuals (abundMature
	 * + abundImmature * ImmSize).
	 *
	 * \return : the number of total individuals within the legion
	 */
	int totalNumAbund();

	/*!
	 *	\brief Age all individuals in the legion list
	 *
	 *	This functions increments the age of all individuals of all cohorts of
	 * the legion list by 1. If the older cohort reaches the maximum age of the
	 * functional group, this last cohort is removed from the legion list.
	 *
	 *	\param maxAge : maximum age that can be reached by an individual. If -1,
	 * it corresponds to the functional group life span.
	 */
	void ageLegions(int maxAge = -1);

	/*!
	 *	\brief Age seeds in a pool and calculate new abundance
	 *
	 * Seed mortality rate is considered to follow a linear relationship as a
	 * function of seed life :
	 * size (n+1) = size (n) - size(n) * (1 / (pl + 1))
	 *
	 *	\param pt : selected pool (DormantP or ActiveP)
	 */
	void AgePool1(const PoolType& pt);

	/*!
	 *	\brief Age seeds in all pools and calculate new abundances
	 *
	 * Seed mortality rate is considered to follow a linear relationship as a
	 * function of seed life :
	 * size (n+1) = size (n) - size(n) * (1 / (pl + 1))
	 */
	void AgePool1();

};

BOOST_CLASS_VERSION(FuncGroup, 0)
#endif //FUNCGROUP
