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
/*                                Cohort Class                                */
/*============================================================================*/

/*!
 * \file Cohort.h
 * \brief Basal structure to store Plants abundance
 * \author Damien Georges
 * \version 1.0
 * \date 2013/10/21
 */

#ifndef COHORT_H
#define COHORT_H

#include <iostream>
#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include "Logger.h"
/*#include <boost/serialization/version.hpp>*/

using namespace std;


/*!
 * \class Cohort
 * \brief Basal Structure to store PFG abundances
 *
 * A cohort is the formal unity for storing plant abundances in optimal way.
 * It is represented by individuals of different age but of the same abundance.
 * Thus, a cohort is defined by the ages of younger and older individuals, and
 * by an integer representing the abundance of each age.
 * For example, a cohort defined by (100, 2, 4) contains 300 individuals aged
 * from 2 to 4 years.
 */

class Cohort
{
	private:

	int m_CSize; /*!< Abundance of each age of the Cohort */
	int m_Ay; /*!< Age of the Youngest individuals */
	int m_Ao; /*!< Age of the Oldest individuals */

	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		ar & m_CSize;
		ar & m_Ay;
		ar & m_Ao;
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	Cohort default constructor => All parameters are set to 0, False or None
	 */
	Cohort();

	/*!
	 *	\brief Full constructor
	 *
	 *	Cohort full constructor
	 *
	 *	\param cSize : size of cohort (abundance, virtual number of individuals)
	 *	\param ay : youngest individuals of the cohort
	 *	\param ao : oldest individuals of the cohort
	 */
	Cohort(int cSize, int ay, int ao);

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	Cohort destructor
	 */
	virtual ~Cohort();

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const Cohort& o) const
	{
		return (m_CSize == o.m_CSize &&
		m_Ay == o.m_Ay &&
		m_Ao == o.m_Ao);
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	int getCSize() const;
	int getAy() const;
	int getAo() const;

	void setCSize(const int& cSize);
	void setAy(const int& ay);
	void setAo(const int& ao);

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	void show();

};

BOOST_CLASS_VERSION(Cohort, 0)
#endif //COHORT_H
