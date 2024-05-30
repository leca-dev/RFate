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
/*              Functional Group response to a perturbation Class             */
/*============================================================================*/

/*!
 * \file FGresponse.h
 * \brief Functional Group response to perturbation(s)
 * \author Maya Gueguen
 * \version 1.0
 * \date 2015/06/04
 */

#ifndef FGresponse_H
#define FGresponse_H

#include "FGUtils.h"
#include "GlobalSimulParameters.h"

using namespace std;


/*!
 * \class FGresponse
 * \brief Functional Group response to perturbation(s)
 *
 * This object stores all the parameters characterizing the response of a Plant
 * Functional Group to a perturbation (disturbance, fire, drought...) in terms
 * of killed, resprouting or unaffected individuals.
 * It also defines the number of ways for each PFG to react to a perturbation,
 * and the different ages at which an individual changes response class.
 */

class FGresponse
{
	private:
	  
	unsigned m_NoPert; /*!< Number of different levels of perturbation */
  unsigned m_NoPertSub; /*!< Number of perturbation subdivision (number of way to react to pert) */

	vector<int> m_PropKilled; /*!< Proportion of propagules killed */
	vector<vector<int> > m_BreakAge; /*!< Age representing shift in response of FG */
	vector<vector<int> > m_ResprAge; /*!< Age of re-sprouting for each age class */
	vector<vector< vector<int> > > m_Fates; /*!< Proportion of FG unaffected, re-sprouted or killed for each age class */
	vector<int> m_ActiveSeeds; /*!< Proportion of Dormant seeds activated */

	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		ar & m_NoPert;
		ar & m_NoPertSub;
		ar & m_PropKilled;
		ar & m_BreakAge;
		ar & m_ResprAge;
		ar & m_Fates;
		ar & m_ActiveSeeds;
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	FGresponse default constructor => All parameters are set to 0, False or None
	 */
	FGresponse();

	/*!
	 *	\brief Full constructor
	 *
	 *	FGresponse full constructor => Some parameters are set to 0, False or None
	 *
	 *	\param noPert : number of perturbations
	 *	\param noPertSub : number of sub-perturbations
	 */
	FGresponse(unsigned noPert, unsigned noPertSub );

	/*!
	 *	\brief Full constructor
	 *
	 *	FGresponse full constructor : parameters are filled with values stored
	 * into text file
	 *
	 *	\param PFG_PerturbationsFile : path to text file containing well formated
	 * FG perturbations behavior parameters
	 *	\param noPert : number of perturbations
	 *	\param noPertSub : number of sub-perturbations
	 */
	FGresponse(const string& PFG_PerturbationsFile, unsigned noPert, unsigned noPertSub );

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	FGresponse destructor
	 */
	virtual ~FGresponse();

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const FGresponse& o) const
	{
		return (m_NoPert == o.m_NoPert &&
		m_NoPertSub == o.m_NoPertSub &&
		m_PropKilled == o.m_PropKilled &&
		m_BreakAge == o.m_BreakAge &&
		m_ResprAge == o.m_ResprAge &&
		m_Fates == o.m_Fates &&
		m_ActiveSeeds == o.m_ActiveSeeds);
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	unsigned getNoPert() const;
	unsigned getNoPertSub() const;
	const vector<int>& getPropKilled() const;
	const int& getPropKilled(const unsigned& dist) const;
	const vector< vector<int> >& getBreakAge() const;
	int getBreakAge(const unsigned& dist, const unsigned& range) const;
	const vector< vector<int> >& getResprAge() const;
	int getResprAge(const unsigned& dist, const unsigned& range) const;
	const vector<vector< vector<int> > >& getFates() const;
	const int& getFates(const unsigned& dist, const unsigned& range, const DistFate& df) const;
	const vector<int>& getActiveSeeds() const;
	const int& getActiveSeeds(const unsigned& dist) const;

	void setNoPert(const unsigned& noPert);
	void setNoPertSub(const unsigned& noPertSub);
	void setPropKilled(const vector<int>& propKilled);
	void setPropKilled(const int& propKilled, const unsigned& dist);
	void setBreakAge(const vector< vector<int> >& breakAge);
	void setBreakAge(const int& breakAge, const unsigned& dist, const unsigned& range);
	void setResprAge(const vector< vector<int> >& resprAge);
	void setResprAge(const int& resprAge, const unsigned& dist, const unsigned& range);
	void setFates(const vector<vector< vector<int> > >& fates);
	void setFates(const int& fates, const unsigned& dist, const unsigned& range, const DistFate& df);
	void setActiveSeeds(const vector<int>& activeSeeds);
	void setActiveSeeds(const int& activeSeeds, const unsigned& dist);

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	void show();

};

BOOST_CLASS_VERSION(FGresponse, 0)
#endif // FGresponse_H
