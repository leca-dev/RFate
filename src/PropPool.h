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
/*                            Propagules Pool Class                           */
/*============================================================================*/

/*!
 * \file PropPool.h
 * \brief Propagules Pool Class
 * \author Damien Georges
 * \version 1.0
 * \date 2013/10/21
 */

#ifndef PROPOOL_H
#define PROPOOL_H

#include <iostream>
#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include "Logger.h"

using namespace std;

/*!
 * \class PropPool
 * \brief Basal Structure to store PFG propagules
 *
 * A pool is the formal unity for storing plant propagules. It is defined by
 * its size (seeds abundance), the age of the last seeds put in the pool, and a
 * boolean defining if seeds are declining through time or not (meaning some of
 * them will die at the next time step).
 * Within FATE, each plant functional group (defined by the FuncGroup class)
 * has two pools of seeds, one active and one dormant. In most cases, only the
 * active pool will be used.
 */

class PropPool
{
	private:

	int m_Size; /*!< Abundance of seeds of the pool, % of Potential Fecundity */
	bool m_Declining; /*!< Is the pool declining ? */
	int m_DTime; /*!< Age of the youngest seeds in */

	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		ar & m_Size;
		ar & m_Declining;
		ar & m_DTime;
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	PropPool default constructor => All parameters are set to 0, False or None
	 */
	PropPool();

	/*!
	 *	\brief Full constructor
	 *
	 *	PropPool full constructor
	 *
	 *	\param size : size of pool (abundance, percentage of potential fecundity)
	 *	\param declining : is the pool declining ?
	 *	\param dTime : age of the youngest seeds of the pool
	 */
	PropPool(int size, bool declining, int dTime);

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	PropPool destructor
	 */
	virtual ~PropPool();

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const PropPool& o) const
	{
		return (m_Size == o.m_Size &&
		m_Declining == o.m_Declining &&
		m_DTime == o.m_DTime);
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	int getSize() const;
	bool getDeclining() const;
	int getDTime() const;

	void setSize(const int& size);
	void setDeclining(const bool& declining);
	void setDTime(const int& dTime);

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	void show();

	/*!
	 *	\brief Add seeds in the pool
	 *
	 *	This function compares the new abundance of seeds (Inp) to the abundance
	 * of seeds already present in the pool.
	 * If the new abundance is greater, the pool becomes :
	 * Pool(size = Inp, declining = false, dTime = 0),
	 * otherwise, the propagules pool is not modified.
	 *
	 *	\param Inp : abundance of seeds to be added
	 */
	void PutSeedInPool(int Inp);

	/*!
	 *	\brief Empty the seeds pool
	 *
	 *	All parameters of the propagules pool are set to 0, none or FALSE.
	 */
	void EmptyPool();

	/*!
	 *	\brief Age seeds in the pool and calculate new abundance
	 *
	 * Seed mortality rate is considered to follow a linear relationship as a
	 * function of seed life :
	 * size (n+1) = size (n) - size(n) * (1 / (pl + 1))
	 *
	 *	\param pl : seeds life span
	 */
	void AgePool1(int pl);

};

BOOST_CLASS_VERSION(PropPool, 0)
#endif // PROPOOL
