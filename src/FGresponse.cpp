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

FGresponse::FGresponse() : m_NoPert(1), m_NoPertSub(1), m_PropKilled(m_NoPert,PC00),
m_BreakAge(m_NoPert, vector<int>(m_NoPertSub+1, 0)),
m_ResprAge(m_NoPert, vector<int>(m_NoPertSub+1, 0)),
m_Fates(m_NoPert, vector< vector<Fract> >(m_NoPertSub+1, vector<Fract>(DFcount, PC00))),
m_DormBreaks(m_NoPert, PC00)
{
  	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FGresponse::FGresponse(unsigned noPert, unsigned noPertSub) : m_NoPert(noPert), m_NoPertSub(noPertSub), m_PropKilled(m_NoPert,PC00),
m_BreakAge(m_NoPert, vector<int>(m_NoPertSub+1, 0)),
m_ResprAge(m_NoPert, vector<int>(m_NoPertSub+1, 0)),
m_Fates(m_NoPert, vector< vector<Fract> >(m_NoPertSub+1, vector<Fract>(DFcount, PC00))),
m_DormBreaks(m_NoPert, PC00)
{
  	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FGresponse::FGresponse(const string& PFG_PerturbationsFile, int noPert, int noPertSub) : m_NoPert(noPert), m_NoPertSub(noPertSub)
{
	testFileExist("--PFG_PARAMS_...PERT...--", PFG_PerturbationsFile, false);

		par::Params PertParms(PFG_PerturbationsFile.c_str(), " = \"", "#"); /* opening PFG perturbations parameters file */		
		
		vector<int> v_int = PertParms.get_val<int>("PROP_KILLED");
		if (v_int.size() != noPert)
		{
			cerr << "!!! Wrong number of parameters provided for PROP_KILLED (" << v_int.size() << " instead of " << noPert << "). Please check!" << endl;
			terminate();
		}
		m_PropKilled = convert_int_to_enum<Fract>("PROP_KILLED", v_int, "Fract", Fcount);		
		
		v_int = PertParms.get_val<int>("BREAK_AGE");
		if (v_int.size() < noPert * (noPertSub-1))
		{
			cerr << "!!! Wrong number of parameters provided for BREAK_AGE (" << v_int.size() << " instead of " << noPert * (noPertSub-1) << "). Please check!" << endl;
			terminate();
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
		for(unsigned i=0; i<noPert; i++)
		{
			bool is_sup = false;
			int prev_age = m_BreakAge[i][1];
			for(unsigned j=2; j<noPertSub - 1; j++)
			{
				if (m_BreakAge[i][j] < prev_age)
				{
					is_sup = true;
				}
				prev_age = m_BreakAge[i][j];
			}
			if (is_sup)
			{
				cerr << "!!! BREAK_AGE must be given in ascending order. Please check!" << endl;
				terminate();
			}
		}
		
		v_int = PertParms.get_val<int>("RESPR_AGE");
		if (v_int.size() != noPert * noPertSub)
		{
			cerr << "!!! Wrong number of parameters provided for RESPR_AGE (" << v_int.size() << " instead of " << noPert * noPertSub << "). Please check!" << endl;
			terminate();
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
			cerr << "!!! Wrong number of parameters provided for FATES (" << v_int.size() << " instead of " << noPert * noPertSub * (DFcount-1) << "). Please check!" << endl;
			terminate();
		}
		counter = 0;
		m_Fates.resize(noPert);
		for (unsigned i=0; i<m_Fates.size(); i++)
		{
			m_Fates[i].resize(noPertSub);
			for (unsigned j=0; j<m_Fates[i].size(); j++)
			{
				m_Fates[i][j].resize(int(DFcount), Fract(4));
				m_Fates[i][j][0] = Fract(v_int[counter]); /* killed */
				counter++;
				m_Fates[i][j][2] = Fract(v_int[counter]); /* resprout */
				counter++;
				/* Automatic filling of several parameters for compatibility between perturbations index */
				/* Proportion of individuals unaffected is set to 100% - ( fract(killed) + fract(resprout)) */
				if ((FractToDouble(m_Fates[i][j][0]) + FractToDouble(m_Fates[i][j][2])) > 1)
				{
					cerr << "!!! Wrong values of parameters provided for FATES : Kill and Resprout percentages for perturbation ";
					cerr << i << " and sub-perturbation " << j << " sum is superior to 100%. Please check!" << endl;
					terminate();
				}
				m_Fates[i][j][1] = getLeavingFract( m_Fates[i][j][0], m_Fates[i][j][2] );
			}
		}
		
		v_int = PertParms.get_val<int>("ACTIVATED_SEED");
		if (v_int.size() != noPert)
		{
			cerr << "!!! Wrong number of parameters provided for ACTIVATED_SEED (" << v_int.size() << " instead of " << noPert << "). Please check!" << endl;
			terminate();
		}
		m_DormBreaks = convert_int_to_enum<Fract>("ACTIVATED_SEED", v_int, "Fract", Fcount);
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

const unsigned& FGresponse::getNoPert() const {return m_NoPert;}
const unsigned& FGresponse::getNoPertSub() const {return m_NoPertSub;}
const vector<Fract>& FGresponse::getPropKilled() const {return m_PropKilled;}
const Fract& FGresponse::getPropKilled(const int& dist) const {
	if (dist<0 || dist>m_NoPert)
	{
		cerr << "!!! Try to access value of m_PropKilled for a non-existing perturbation. Please check!" << endl;
		terminate();
	} else
	{
		return m_PropKilled[dist];
	}	
}
const vector< vector<int> >& FGresponse::getBreakAge() const {return m_BreakAge;}
const int& FGresponse::getBreakAge(const int& dist, const int& range) const {
	if (dist<0 || dist>m_NoPert || range<0 || range>m_NoPertSub)
	{
		cerr << "!!! Try to access value of m_BreakAge for a non-existing perturbation or sub-perturbation. Please check!" << endl;
		terminate();
	} else
	{
		return m_BreakAge[dist][range];
	}	
}
const vector< vector<int> >& FGresponse::getResprAge() const {return m_ResprAge;}
const int& FGresponse::getResprAge(const int& dist, const int& range) const {
	if (dist<0 || dist>m_NoPert || range<0 || range>m_NoPertSub)
	{
		cerr << "!!! Try to access value of m_ResprAge for a non-existing perturbation or sub-perturbation. Please check!" << endl;
		terminate();
	} else
	{
		return m_ResprAge[dist][range];
	}	
}
const vector<vector< vector<Fract> > >& FGresponse::getFates() const {return m_Fates;}
const Fract& FGresponse::getFates(const int& dist, const int& range, const DistFate& df) const {
	if (dist<0 || dist>m_NoPert || range<0 || range>m_NoPertSub)
	{
		cerr << "!!! Try to access value of m_Fates for a non-existing perturbation or sub-perturbation. Please check!" << endl;
		terminate();
	} else
	{
		return m_Fates[dist][range][df];
	}	
}
const vector<Fract>& FGresponse::getDormBreaks() const {return m_DormBreaks;}
const Fract& FGresponse::getDormBreaks(const int& dist) const {
	if (dist<0 || dist>m_NoPert)
	{
		cerr << "!!! Try to access value of m_BreakAge for a non-existing perturbation. Please check!" << endl;
		terminate();
	} else
	{
		return m_DormBreaks[dist];
	}	
}

void FGresponse::setNoPert(const unsigned& noPert){m_NoPert = noPert;}
void FGresponse::setNoPertSub(const unsigned& noPertSub){m_NoPertSub = noPertSub;}
void FGresponse::setPropKilled(const vector<Fract>& propKilled){m_PropKilled = propKilled;}
void FGresponse::setPropKilled(const Fract& propKilled, const int& dist){m_PropKilled[dist] = propKilled;}
void FGresponse::setBreakAge(const vector< vector<int> >& breakAge){m_BreakAge = breakAge;}
void FGresponse::setBreakAge(const int& breakAge, const int& dist, const int& range){m_BreakAge[dist][range] = breakAge;}
void FGresponse::setResprAge(const vector< vector<int> >& resprAge){m_ResprAge = resprAge;}
void FGresponse::setResprAge(const int& resprAge, const int& dist, const int& range){m_ResprAge[dist][range] = resprAge;}
void FGresponse::setFates(const vector<vector< vector<Fract> > >& fates){m_Fates = fates;}
void FGresponse::setFates(const Fract& fates, const int& dist, const int& range, const DistFate& df){m_Fates[dist][range][df] = fates;}
void FGresponse::setDormBreaks(const vector<Fract>& dormBreaks){m_DormBreaks = dormBreaks;}
void FGresponse::setDormBreaks(const Fract& dormBreaks, const int& dist){m_DormBreaks[dist] = dormBreaks;}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FGresponse::show()
{
	/* iterator definition for multi-dim vectors printing */
	vector<int>::iterator it1, end1;
	vector<vector<int> >::iterator it2, end2;

	vector<Fract>::iterator it1f, end1f;
	vector<vector<Fract> >::iterator it2f, end2f;
	vector<vector<vector<Fract> > >::iterator it3f, end3f;

	cout << "m_NoPert = " << m_NoPert << endl;
	cout << "m_NoPertSub = " << m_NoPertSub << endl;
	cout << "m_PropKilled = " ;
	copy(m_PropKilled.begin(), m_PropKilled.end(), ostream_iterator<int>(cout, " "));
	cout << endl;
	cout << "m_BreakAge = (line: perturbation, column: reaction level)" << endl;
	it2=m_BreakAge.begin();
	end2=m_BreakAge.end();
	while (it2!=end2)
	{
		it1=it2->begin();
		end1=it2->end();
		copy(it1,end1, ostream_iterator<int>(cout, " "));
		cout << endl;
		++it2;
	}
	cout << "m_ResprAge = (line: perturbation, column: reaction level)" << endl;
	it2=m_ResprAge.begin();
	end2=m_ResprAge.end();
	while (it2!=end2)
	{
		it1=it2->begin();
		end1=it2->end();
		copy(it1,end1, ostream_iterator<int>(cout, " "));
		cout << endl;
		++it2;
	}
	cout << "m_Fates = (block: perturbation, line: reaction level, column: plant behaviour)"  << endl;
	it3f=m_Fates.begin();
	end3f=m_Fates.end();
	while (it3f != end3f)
	{
		it2f=it3f->begin();
		end2f=it3f->end();
		while (it2f != end2f)
		{
			it1f=it2f->begin();
			end1f=it2f->end();
			copy(it1f,end1f, ostream_iterator<Fract>(cout, " "));
			cout << endl;
			++it2f;
		}
		cout << endl;
		++it3f;
	}
	cout << "m_DormBreaks = " ;
	copy(m_DormBreaks.begin(), m_DormBreaks.end(), ostream_iterator<int>(cout, " "));
	cout << endl;
}

