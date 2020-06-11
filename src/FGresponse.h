/*============================================================================*/
/*              Functional Group response to a perturbation Class             */
/*============================================================================*/

/*!
 * \file FGresponse.h
 * \brief Functional Group response to perturbation(s)
 * \author Maya Gueguen
 * \version 5.5-0
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

	vector<Fract> m_PropKilled; /*!< Proportion of propagules killed */
	vector<vector<int> > m_BreakAge; /*!< Age representing shift in response of FG */
	vector<vector<int> > m_ResprAge; /*!< Age of re-sprouting for each age class */
	vector<vector< vector<Fract> > > m_Fates; /*!< Proportion of FG unaffected, re-sprouted or killed for each age class */
	vector<Fract> m_DormBreaks; /*!< Proportion of Dormant seeds activated */

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
		ar & m_DormBreaks;
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
	FGresponse(const string& PFG_PerturbationsFile, int noPert, int noPertSub );

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
		m_DormBreaks == o.m_DormBreaks);
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	const unsigned& getNoPert() const;
	const unsigned& getNoPertSub() const;
	const vector<Fract>& getPropKilled() const;
	const Fract& getPropKilled(const int& dist) const;
	const vector< vector<int> >& getBreakAge() const;
	const int& getBreakAge(const int& dist, const int& range) const;
	const vector< vector<int> >& getResprAge() const;
	const int& getResprAge(const int& dist, const int& range) const;
	const vector<vector< vector<Fract> > >& getFates() const;
	const Fract& getFates(const int& dist, const int& range, const DistFate& df) const;
	const vector<Fract>& getDormBreaks() const;
	const Fract& getDormBreaks(const int& dist) const;

	void setNoPert(const unsigned& noPert);
	void setNoPertSub(const unsigned& noPertSub);
	void setPropKilled(const vector<Fract>& propKilled);
	void setPropKilled(const Fract& propKilled, const int& dist);
	void setBreakAge(const vector< vector<int> >& breakAge);
	void setBreakAge(const int& breakAge, const int& dist, const int& range);
	void setResprAge(const vector< vector<int> >& resprAge);
	void setResprAge(const int& resprAge, const int& dist, const int& range);
	void setFates(const vector<vector< vector<Fract> > >& fates);
	void setFates(const Fract& fates, const int& dist, const int& range, const DistFate& df);
	void setDormBreaks(const vector<Fract>& dormBreaks);
	void setDormBreaks(const Fract& dormBreaks, const int& dist);

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	void show();

};

BOOST_CLASS_VERSION(FGresponse, 0)
#endif // FGresponse_H
