/*============================================================================*/
/*                                Legion Class                                */
/*============================================================================*/

/*!
 * \file Legion.h
 * \brief List of Cohort of individuals having the same abundance
 * \author Damien Georges
 * \version 1.0
 * \date 2013/10/21
 */

#ifndef LEGION_H
#define LEGION_H

#include <vector>
#include "Cohort.h"

using namespace std;


/*!
 * \class Legion
 * \brief List of Cohort of individuals having the same abundance
 *
 * A Legion is a structure to store plant abundances.
 * It is represented by a vector of Cohort objects, each containing individuals
 * of different ages but with the same abundance.
 */

class Legion
{
	private:
	
	vector< Cohort > m_CohortList; /*!< List of succeeding cohorts */
	
	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/
	
	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		//cout << "> Serializing Legion..." << endl;
		ar & m_CohortList;
	}
	
	public:
	
	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/
	
	/*!
	 *	\brief Default constructor
	 *
	 *	Legion default constructor => All parameters are set to 0, False or None
	 */
	Legion();
	
	/*!
	 *	\brief Full constructor
	 *
	 *	Legion full constructor
	 *
	 *	\param cohortList : vector of cohorts
	 */
	Legion(vector<Cohort> cohortList);
	
	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/
	
	/*!
	 *	\brief Destructor
	 *
	 *	Legion destructor
	 */
	virtual ~Legion();
	
	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/
	
	bool operator==(const Legion& o) const
	{
		return ( m_CohortList == o.m_CohortList );
	}
	
	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/
	
	const vector<Cohort>& getCohortList() const;
	const Cohort& getCohort(const int& id) const;
	
	vector<Cohort>* getCohortList_();
	Cohort* getCohort_(const int& id);
	const int& getNoCohort() const;
	
	void setCohortList(const vector<Cohort>& cohortList);
	void setCohort(const int& id, const Cohort& cohort);
	
	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/
	
	void show();
	
	/*!
	 *	\brief Insert a cohort of individuals (one or several generations) into
	 * a cohort list
	 *
	 *	This routine inserts a cohort composed of (ay - ao) generations of cSize
	 * individuals into a legion (list of cohorts).
	 * A new cohort can be created, an older one can be split or modified, to
	 * keep a well sorted Legion list.
	 * 
	 *	\param cSize : abundance of each generation of the new cohort
	 *	\param ay : age of the youngest individuals of the new cohort
	 *	\param ao : age of the oldest individuals of the new cohort
	 */
	void addCohort(const int& cSize, const int& ay, const int& ao);
	
	/*!
	 *	\brief Split a cohort of individuals in 2 parts
	 *
	 *	This function split a given cohort in 2 parts to let other functions
	 * manipulate them correctly (increase or decrease abundance of specific
	 * generations, remove specific generations, etc).
	 *
	 *	\param i : the index within the Legion list of the cohort to split 
	 *	\param age : the breaking age used to split the selected cohort
	 */
	void splitCohort(const int& i, const int& age);
	
	/*!
	 *	\brief Remove a cohort of individuals (one or several generations) from
	 * a cohort list
	 *
	 *	This routine removes all cohorts whose age is between ay and ao.
	 * A whole cohort can be removed, an older one can be split or modified, to
	 * keep a well sorted Legion list.
	 * Given ages are strictly removed from the Legion list.
	 * 
	 *	\param ay : age of the youngest individuals to be removed
	 *	\param ao : age of the oldest individuals to be removed
	 */
	void removeCohort(const int& ay, const int& ao);

	/*!
	 *	\brief Reduce the abundance of a cohort of individuals (one or several
	 * generations) from a cohort list
	 *
	 *	This routine reduces the size of all cohorts whose age is between ay and
	 * ao. The reduction factor is given by reducFact.
	 * A whole cohort can be removed, an older one can be split or modified, to
	 * keep a well sorted Legion list.
	 * 
	 *	\param ay : age of the youngest individuals whose abundance should be
	 * reduced
	 *	\param ao : age of the oldest individuals whose abundance should be
	 * reduced
	 * \param reducFact : the cohort abundance reduction factor
	 */
	void reduceCohort(const int& ay, const int& ao, const double& reducFact);
	
	/*!
	 *	\brief Pick up all cohorts of individuals from a legion list if possible
	 *
	 *	This routine picks up adjacent cohorts (successive generations) having
	 * the same size.
	 * The aim is to prevent from memory wasting.
	 */
	void pickupCohorts();
	
};

BOOST_CLASS_VERSION(Legion, 0)
#endif //LEGION_H
