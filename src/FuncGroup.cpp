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

#include "FuncGroup.h"

using namespace std;

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FuncGroup::FuncGroup() : m_Pools(PTcount, PropPool()), m_LList(Legion()), m_FGparams(new FG())
{
	/* Nothing to do */
}

FuncGroup::FuncGroup(FGPtr fgparams) : m_Pools(PTcount, PropPool()), m_LList(Legion()), m_FGparams(fgparams)
{
	/* Nothing to do */
}

FuncGroup::FuncGroup(const vector<PropPool>& pools, const Legion& llist, FGPtr fgparams) : m_Pools(pools), m_LList(llist), m_FGparams(fgparams)
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

FuncGroup::~FuncGroup()
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

const vector<PropPool>&  FuncGroup::getPools() const { return m_Pools; }
const PropPool& FuncGroup::getPools( const PoolType& pt) const { return m_Pools[static_cast<int>(pt)]; }
const Legion& FuncGroup::getLList() const { return m_LList; }
const FG& FuncGroup::getFGparams() const { return *m_FGparams; }

Legion* FuncGroup::getLList_() { return &m_LList; }
PropPool* FuncGroup::getPools_( const PoolType& pt) { return &m_Pools[static_cast<int>(pt)];}
FGPtr FuncGroup::getFGparams_() { return m_FGparams; }

void FuncGroup::setPools( const vector<PropPool>& pools) { m_Pools = pools; }
void FuncGroup::setPools( const PropPool& pool, const PoolType& pt ) { m_Pools[static_cast<int>(pt)] = pool; }
void FuncGroup::setLList( const Legion llist ) { m_LList = llist; }
void FuncGroup::setFGparams( FGPtr fgparams ) { m_FGparams = fgparams; }

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FuncGroup::show()
{
	logg.debug("FunctGroup :",
						 "\nFG = ", m_FGparams->getName());
	m_LList.show();
	for (unsigned i=0; i<m_Pools.size();i++)
	{
		m_Pools[i].show();
	}
} // end of show()

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
void FuncGroup::summary()
{
	unsigned Ay = 0, Ao = 0, TotAbund = 0;

	if (m_LList.getNoCohort() > 0)
	{
		Ay = m_LList.getCohortList().front().getAy();
		Ao = m_LList.getCohortList().back().getAo();
		TotAbund = this->totalNumAbund();
	}

	logg.debug("FunctGroup summary :",
						 "\nFG = ", m_FGparams->getName(),
						 "\tYoungest = ", Ay,
						 "\tOldest = ", Ao,
						 "\tTotAbund = ", TotAbund);
} // end of summary()

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

unsigned FuncGroup::totalNumAbund(unsigned Age0, unsigned Age1)
{
	/* initialize count to 0 */
	unsigned CsizeTot = 0;

	/* check that Age0 < Age1 */
	if (Age0 > Age1)
	{
		unsigned AgeTmp = Age0;
		Age0 = Age1;
		Age1 = AgeTmp;
	}

	if (m_LList.getNoCohort() > 0)
	{
	for (int i=0; i<m_LList.getNoCohort(); i++)
	{
		unsigned Ay = m_LList.getCohort(i).getAy();
		unsigned Ao = m_LList.getCohort(i).getAo();

		/* check if legion is concerned */
		if (Age0 > Ao || Age1 < Ay)
		{
			continue;
		} else
		{
			/* get size of cohorts of the legion */
			unsigned Csize = m_LList.getCohort(i).getCSize();

			/* initialize the legion part (mature and immature) counts */
			unsigned matLegPart = 0, immLegPart = 0;
			unsigned MatTime = m_FGparams->getMatTime();
			double ImmSize = IntToDouble(m_FGparams->getImmSize());

			if (Ao>=MatTime && Age1>=MatTime)
			{ // some matures in this legion
				matLegPart = min(Ao, Age1) - max(MatTime, max(Ay, Age0) ) + 1;
				CsizeTot += matLegPart * Csize;
			}

			if (Ay<MatTime && Age0<MatTime)
			{ // some immatures in this legion
				immLegPart = min( MatTime-1, min(Ao, Age1) ) - max(Ay,Age0) + 1;
				CsizeTot += (unsigned) (immLegPart * Csize * ImmSize);
			}

		}
	} // end of loop on cohorts
	}
	return CsizeTot;
} // end of totalNumAbund(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

unsigned FuncGroup::totalNumAbund()
{
	return totalNumAbund( 0, m_FGparams->getLifeSpan() );
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FuncGroup::ageLegions(int maxAge)
{
	/* if no maxAge given we take the FG lifespan */
	if (maxAge == -1)
	{
		maxAge = m_FGparams->getLifeSpan();
	}

	if (m_LList.getNoCohort() > 0)
	{ // if there is some cohort within the legion
		for (int i=0; i<m_LList.getNoCohort(); i++)
		{
			m_LList.getCohort_(i)->setAy(m_LList.getCohort(i).getAy() + 1);
			m_LList.getCohort_(i)->setAo(min( m_LList.getCohort(i).getAo() + 1,  maxAge));
		}
		/* check that maximum age has not been exceeded in all last cohort */
		/* if it is the case, remove the last cohort */
		if (m_LList.getCohortList().back().getAy() >  m_LList.getCohortList().back().getAo())
		{
			m_LList.getCohortList_()->pop_back();
		}
	}
} // end of ageLegions(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FuncGroup::AgePool1(const PoolType& pt)
{
	m_Pools[pt].AgePool1( m_FGparams->getPoolLife(pt) );
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void FuncGroup::AgePool1()
{
	for (int pt=0; pt<PTcount; pt++)
	{
		this->AgePool1((PoolType) pt);
	}
}
