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

/*=============================================================*/
/*     Plants Functionals Groups response to perturbations     */
/*                       Definition Class                      */
/*=============================================================*/

#include "FGresponse.h"
#include "FGUtils.h"
#include "Params.h"

using namespace std;

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FGresponse::FGresponse() : m_NoPert(1), m_NoPertSub(1), m_PropKilled(m_NoPert, 0),
m_BreakAge(m_NoPert, vector<int>(m_NoPertSub+1, 0)),
m_ResprAge(m_NoPert, vector<int>(m_NoPertSub+1, 0)),
m_Fates(m_NoPert, vector< vector<int> >(m_NoPertSub+1, vector<int>(DFcount, 0))),
m_DormBreaks(m_NoPert, 0)
{
  	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FGresponse::FGresponse(unsigned noPert, unsigned noPertSub) : m_NoPert(noPert), m_NoPertSub(noPertSub), m_PropKilled(m_NoPert, 0),
m_BreakAge(m_NoPert, vector<int>(m_NoPertSub+1, 0)),
m_ResprAge(m_NoPert, vector<int>(m_NoPertSub+1, 0)),
m_Fates(m_NoPert, vector< vector<int> >(m_NoPertSub+1, vector<int>(DFcount, 0))),
m_DormBreaks(m_NoPert, 0)
{
  	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FGresponse::FGresponse(const string& PFG_PerturbationsFile, int noPert, int noPertSub) : m_NoPert(noPert), m_NoPertSub(noPertSub)
{
	testFileExist("--PFG_PARAMS_...PERT...--", PFG_PerturbationsFile, false);

		par::Params PertParms(PFG_PerturbationsFile.c_str(), " = \"", "#"); /* opening PFG perturbations parameters file */
    
    m_PropKilled = PertParms.get_val<int>("PROP_KILLED");
		if (m_PropKilled.size() != noPert)
		{
			logg.error("!!! Wrong number of parameters provided for PROP_KILLED (",
              m_PropKilled.size(), " instead of ", noPert, "). Please check!");
		}
		for(unsigned i=1; i<m_PropKilled.size(); i++)
		{
		  if (m_PropKilled[i] < 0 || m_PropKilled[i] > 100)
		  {
		    logg.error("!!! PROP_KILLED values must be superior or equal to 0, and inferior or equal to 100. Please check!");
		  }
		}

		vector<int> v_int = PertParms.get_val<int>("BREAK_AGE");
		if (v_int.size() < noPert * (noPertSub-1))
		{
			logg.error("!!! Wrong number of parameters provided for BREAK_AGE (",
								 v_int.size(), " instead of ", noPert * (noPertSub-1),
								 "). Please check!");
		}
		int counter = 0;
		m_BreakAge.resize(noPert);
		for (unsigned i=0; i<m_BreakAge.size(); i++)
		{
			m_BreakAge[i].resize(noPertSub+1);
			/* The first class age break must be 0 */
			/* The last class age break must be 'infinite' */
			m_BreakAge[i][0] = 0; m_BreakAge[i][noPertSub] = 10000;
			/* Other are filled according to parameters file */
			for (unsigned j=1; j<m_BreakAge[i].size()-1; j++)
			{
				m_BreakAge[i][j] = v_int[counter];
				counter++;
			}
		}
		for(int i=0; i<noPert; i++)
		{
			bool is_sup = false;
			int prev_age = m_BreakAge[i][1];
			for(int j=2; j<noPertSub - 1; j++)
			{
				if (m_BreakAge[i][j] < prev_age)
				{
					is_sup = true;
				}
				prev_age = m_BreakAge[i][j];
			}
			if (is_sup)
			{
				logg.error("!!! BREAK_AGE must be given in ascending order. Please check!");
			}
		}

		v_int = PertParms.get_val<int>("RESPR_AGE");
		if (v_int.size() != noPert * noPertSub)
		{
			logg.error("!!! Wrong number of parameters provided for RESPR_AGE (",
								 v_int.size(), " instead of ", noPert * noPertSub,
								 "). Please check!");
		}
		counter = 0;
		m_ResprAge.resize(noPert);
		for (unsigned i=0; i<m_ResprAge.size(); i++)
		{
			m_ResprAge[i].resize(noPertSub);
			for (unsigned j=0; j<m_ResprAge[i].size(); j++)
			{
				m_ResprAge[i][j] = v_int[counter];
				counter++;
			}
		}

		v_int = PertParms.get_val<int>("FATES");
		if (v_int.size() != noPert * noPertSub * (DFcount-1))
		{
			logg.error("!!! Wrong number of parameters provided for FATES (",
								 v_int.size(), " instead of ", noPert * noPertSub * (DFcount-1),
								 "). Please check!");
		}
		counter = 0;
		m_Fates.resize(noPert);
		for (unsigned i=0; i<m_Fates.size(); i++)
		{
			m_Fates[i].resize(noPertSub);
			for (unsigned j=0; j<m_Fates[i].size(); j++)
			{
				m_Fates[i][j].resize(DFcount);
			  if (v_int[counter] < 0 || v_int[counter] > 100)
			  {
			    logg.error("!!! FATES values must be superior or equal to 0, and inferior or equal to 100. Please check!");
			  }
				m_Fates[i][j][0] = v_int[counter]; /* killed */
				counter++;
				if (v_int[counter] < 0 || v_int[counter] > 100)
				{
				  logg.error("!!! FATES values must be superior or equal to 0, and inferior or equal to 100. Please check!");
				}
				m_Fates[i][j][2] = v_int[counter]; /* resprout */
				counter++;
				
				/* Automatic filling of several parameters for compatibility between perturbations index */
				/* Proportion of individuals unaffected is set to 100% - ( fract(killed) + fract(resprout)) */
				if ((m_Fates[i][j][0] + m_Fates[i][j][2]) > 100)
				{
					logg.error("!!! Wrong values of parameters provided for FATES : ",
										 "Kill and Resprout percentages for perturbation ", i,
										 " and sub-perturbation ", j,
										 " sum is superior to 100%. Please check!");
				}
				m_Fates[i][j][1] = getLeavingFract( m_Fates[i][j][0], m_Fates[i][j][2] );
			}
		}

		m_DormBreaks = PertParms.get_val<int>("ACTIVATED_SEED");
		if (m_DormBreaks.size() != noPert)
		{
			logg.error("!!! Wrong number of parameters provided for ACTIVATED_SEED (",
              m_DormBreaks.size(), " instead of ", noPert, "). Please check!");
		}
		for(unsigned i=1; i<m_DormBreaks.size(); i++)
		{
		  if (m_DormBreaks[i] < 0 || m_DormBreaks[i] > 100)
		  {
		    logg.error("!!! ACTIVATED_SEED values must be superior or equal to 0, and inferior or equal to 100. Please check!");
		  }
		}
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FGresponse::~FGresponse()
{
  	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

unsigned FGresponse::getNoPert() const {return m_NoPert;}
unsigned FGresponse::getNoPertSub() const {return m_NoPertSub;}
const vector<int>& FGresponse::getPropKilled() const {return m_PropKilled;}
const int& FGresponse::getPropKilled(const int& dist) const {
	if (dist<0 || dist > m_NoPert)
	{
		logg.error("!!! Try to access value of m_PropKilled for a non-existing perturbation. Please check!");
	}
	return m_PropKilled[dist];
}
const vector< vector<int> >& FGresponse::getBreakAge() const {return m_BreakAge;}
int FGresponse::getBreakAge(const int& dist, const int& range) const {
	if (dist<0 || dist > m_NoPert || range<0 || range > m_NoPertSub)
	{
		logg.error("!!! Try to access value of m_BreakAge for a non-existing perturbation or sub-perturbation. Please check!");
	}
	return m_BreakAge[dist][range];
}
const vector< vector<int> >& FGresponse::getResprAge() const {return m_ResprAge;}
int FGresponse::getResprAge(const int& dist, const int& range) const {
	if (dist<0 || dist > m_NoPert || range<0 || range > m_NoPertSub)
	{
		logg.error("!!! Try to access value of m_ResprAge for a non-existing perturbation or sub-perturbation. Please check!");
	}
	return m_ResprAge[dist][range];
}
const vector<vector< vector<int> > >& FGresponse::getFates() const {return m_Fates;}
const int& FGresponse::getFates(const int& dist, const int& range, const DistFate& df) const {
	if (dist<0 || dist > m_NoPert || range<0 || range > m_NoPertSub)
	{
		logg.error("!!! Try to access value of m_Fates for a non-existing perturbation or sub-perturbation. Please check!");
	}
	return m_Fates[dist][range][df];
}
const vector<int>& FGresponse::getDormBreaks() const {return m_DormBreaks;}
const int& FGresponse::getDormBreaks(const int& dist) const {
	if (dist<0 || dist > m_NoPert)
	{
		logg.error("!!! Try to access value of m_BreakAge for a non-existing perturbation. Please check!");
	}
	return m_DormBreaks[dist];
}

void FGresponse::setNoPert(const unsigned& noPert){m_NoPert = noPert;}
void FGresponse::setNoPertSub(const unsigned& noPertSub){m_NoPertSub = noPertSub;}
void FGresponse::setPropKilled(const vector<int>& propKilled){m_PropKilled = propKilled;}
void FGresponse::setPropKilled(const int& propKilled, const int& dist){m_PropKilled[dist] = propKilled;}
void FGresponse::setBreakAge(const vector< vector<int> >& breakAge){m_BreakAge = breakAge;}
void FGresponse::setBreakAge(const int& breakAge, const int& dist, const int& range){m_BreakAge[dist][range] = breakAge;}
void FGresponse::setResprAge(const vector< vector<int> >& resprAge){m_ResprAge = resprAge;}
void FGresponse::setResprAge(const int& resprAge, const int& dist, const int& range){m_ResprAge[dist][range] = resprAge;}
void FGresponse::setFates(const vector<vector< vector<int> > >& fates){m_Fates = fates;}
void FGresponse::setFates(const int& fates, const int& dist, const int& range, const DistFate& df){m_Fates[dist][range][df] = fates;}
void FGresponse::setDormBreaks(const vector<int>& dormBreaks){m_DormBreaks = dormBreaks;}
void FGresponse::setDormBreaks(const int& dormBreaks, const int& dist){m_DormBreaks[dist] = dormBreaks;}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FGresponse::show()
{
	logg.debug("m_NoPert = ", m_NoPert,
						 "\nm_NoPertSub = ", m_NoPertSub,
						 "\nm_PropKilled = ", m_PropKilled,
						 "\nm_BreakAge = (line: perturbation, column: reaction level)", m_BreakAge,
						 "\nm_ResprAge = (line: perturbation, column: reaction level)", m_ResprAge,
						 "\nm_Fates = (block: perturbation, line: reaction level, column: plant behaviour)", m_Fates,
						 "\nm_DormBreaks = ", m_DormBreaks);
}
