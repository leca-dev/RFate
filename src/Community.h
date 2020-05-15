/*============================================================================*/
/*                               Community Class                              */
/*============================================================================*/

/*!
 * \file Community.h
 * \brief Structure to store list of Plant Functional Group populations
 * \author Damien Georges
 * \version 1.0
 */

#ifndef COMMUNITY_H
#define COMMUNITY_H

#include "FuncGroup.h"

using namespace std;


/*!
 * \class Community
 * \brief Structure to store list of Plant Functional Group populations
 *
 * A Community is a structure to store plant abundances, and all its associated
 * parameters. It is represented by a vector of FuncGroup objects, containing
 * for each plant functional group its propagule pools, cohort abundances and
 * parameters.
 */

class Community
{
	private:
	
	vector< FuncGroup > m_FuncGroupList; /*!< List of FuncGroup objects */
	
	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/
	
	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		//cout << "> Serializing Community..." << endl;
		ar & m_FuncGroupList;
	}

	public:
	
	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/
	
	/*!
	 *	\brief Default constructor
	 *
	 *	Community default constructor => All parameters are set to 0, False or None
	 */
	Community();
	
	/*!
	 *	\brief Full constructor
	 *
	 *	Community full constructor
	 *
	 *	\param funcGroupList : vector of funcGroups
	 */
	Community(vector<FuncGroup> funcGroupList);
	
	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/
	
	/*!
	 *	\brief Destructor
	 *
	 *	Community destructor
	 */
	virtual ~Community();
	
	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/
	
	bool operator==(const Community& o) const
	{
		return (m_FuncGroupList == o.m_FuncGroupList);
	}
	
	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/
	
	const vector<FuncGroup>& getFuncGroupList() const;
	const FuncGroup& getFuncGroup(const int& id) const;
	
	FuncGroup* getFuncGroup_(const int& id);
	const int& getNoCohort(const int& id) const;
	const int& getAy(const int& id, const int& co) const;
	const int& getAo(const int& id, const int& co) const;
	const int& getCSize(const int& id, const int& co) const;
	
	void setFuncGroupList(const vector<FuncGroup>& funcGroupList);
	void setFuncGroup(const int& id, const FuncGroup& funcGroup);
	
	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/
	
	void show();
	void summary();

};

BOOST_CLASS_VERSION(Community, 0)
#endif //COMMUNITY_H
