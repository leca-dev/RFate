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

#include "Legion.h"

using namespace std;

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Legion::Legion() : m_CohortList(0, Cohort())
{
	/* Nothing to do */
}

Legion::Legion(vector<Cohort> cohortList) : m_CohortList(cohortList)
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Legion::~Legion()
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

const vector<Cohort>& Legion::getCohortList() const{ return m_CohortList; }
const Cohort& Legion::getCohort(const int& id) const{ return m_CohortList[id]; }

vector<Cohort>* Legion::getCohortList_() { return &m_CohortList; }
Cohort* Legion::getCohort_(const int& id) { return &m_CohortList[id]; }

void Legion::setCohortList(const vector<Cohort>& cohortList){ m_CohortList = cohortList;}
void Legion::setCohort(const int& id, const Cohort& cohort){ m_CohortList[id] = cohort;}

int Legion::getNoCohort() const { return m_CohortList.size(); }

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void Legion::show()
{
	logg.debug("Legion object :");
	for(unsigned i=0; i<m_CohortList.size(); i++)
	{
		m_CohortList[i].show();
	}
} // end of show()

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void Legion::addCohort(const int& cSize, const int& ay, const int& ao)
{

	if (cSize > 0)
	{ // if the given size is positive

		int tempCSize = cSize, tempAy = ay, tempAo = ao;

		/* check that ay < ao */
		if (ay > ao)
		{
			tempAy = ao;
			tempAo = ay;
		}

		/* initialize the position within the legion (index of the cohort) */
		unsigned i = 0;
		while (true)
		{ // recursive infinite loop

			/* initialise the cohort to add */
			Cohort newCohort(tempCSize, tempAy, tempAo);

			/* 1. PUSH_BACK : add at the end of the vector if it is the last element */
			if (i >= m_CohortList.size())
			{
				m_CohortList.push_back(newCohort);
				break;
			}

			/* 2. FUSION : merge at the tail if ay = getAo + 1 and cSize = getCSize */
			if (tempAy == m_CohortList[i].getAo()+1 && tempCSize == m_CohortList[i].getCSize())
			{
				tempAy = m_CohortList[i].getAy();
				m_CohortList.erase(m_CohortList.begin() + i);
				continue;
			}

			/* 3. NEXT : switch to next element if ay > getAo */
			if (tempAy > m_CohortList[i].getAo())
			{
				i++;
				continue;
			}

			/* 4. INSERT : add at the beginning if ao < getAy */
			if (tempAo < m_CohortList[i].getAy())
			{
				m_CohortList.insert(m_CohortList.begin() + i, newCohort);
				break;
			}

			/* 5. OVERLAP with beginning of an old cohort : if ao > getAy and ay < getAy */
			if (tempAo > m_CohortList[i].getAy() && tempAy <  m_CohortList[i].getAy())
			{

				if (tempCSize == m_CohortList[i].getCSize())
				{ /* 5a. REDIRECT TO 7 : if same size, cohorts are merged by reducing ay of the old one */
					m_CohortList[i].setAy(tempAy);
				} else
				{ /* 5b. INSERT + REDIRECT TO 7 : different size */
					/* new cohort is split in 2 : one is added before the old one */
					newCohort.setAo(m_CohortList[i].getAy() - 1);
					m_CohortList.insert(m_CohortList.begin() + i , newCohort);
					/* switch to the second part */
					i++;
				}
				/* update cohort to add : shift ay to getAy of the old one */
				tempAy = m_CohortList[i].getAy();
				newCohort.setAy(tempAy);
				continue;
			}

			/* 6. INSERT + REDIRECT TO 7: if ao = getAy and ay < getAy */
			if (tempAo == m_CohortList[i].getAy() && tempAy < m_CohortList[i].getAy())
			{
				/* new cohort is split in 2 : one is added before the old one */
				newCohort.setAo(tempAo - 1);
				m_CohortList.insert(m_CohortList.begin() + i, newCohort);
				/* switch to the second part */
				tempAy = tempAo;
				i++;
				continue;
			}

			/* 7. OVERLAP with the middle */
			if (tempAy == m_CohortList[i].getAy())
			{ // same starting age
				if (tempAo == m_CohortList[i].getAo())
				{ /* 7a. FUSION : same starting and ending age */
					m_CohortList[i].setCSize(m_CohortList[i].getCSize() + tempCSize);
					break;
				} else if (tempAo > m_CohortList[i].getAo())
				{ /* 7b. FUSION + NEXT : same starting age and superior ending age */
					tempCSize = m_CohortList[i].getCSize() + tempCSize;
					m_CohortList[i].setCSize(tempCSize);
					/* update cohort to add */
					tempAy = m_CohortList[i].getAo() + 1;
					newCohort.setAy(tempAy);
					newCohort.setAo(tempAo);
					tempCSize = newCohort.getCSize();
					i++;
					continue;
				} else
				{ /* 7c. FUSION + INSERT : same starting age and inferior ending age */
					tempAo++;
					m_CohortList[i].setAy(tempAo);
					newCohort.setCSize(m_CohortList[i].getCSize() + tempCSize);
					m_CohortList.insert(m_CohortList.begin() + i, newCohort);
					break;
				}
			} else
			{ // different starting age
				/* 8. SPLIT + REDIRECT TO 7 */
				if (tempAo > m_CohortList[i].getAy())
				{
					this->splitCohort(i,tempAy-1);
				}
				i++;
				continue;
			}
		}
	}
} // end of addCohort(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void Legion::splitCohort(const int& i, const int& age)
{
	/* create a new cohort of same size between (age + 1, ao) */
	Cohort newCohort(m_CohortList[i].getCSize(), age + 1, m_CohortList[i].getAo());

	/* change ao of original cohort to age */
	m_CohortList[i].setAo(age);

	/* insert the new cohort after the original one */
	m_CohortList.insert(m_CohortList.begin() + i + 1, newCohort);
} // end of splitCohort(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void Legion::removeCohort(const int& ay, const int& ao)
{
	/* declare modifiable variables */
	int tempAy = ay, tempAo = ao;
	/* check that ay < ao */
	if (ay > ao)
	{
		tempAy = ao;
		tempAo = ay;
	}
	/* get minimum (youngest) and maximum (oldest) age of individuals within the legion */
	int minAge = m_CohortList.front().getAy(), maxAge = m_CohortList.back().getAo();

	/* initialize the position within the legion (index of the cohort) */
	unsigned i = 0;
	if (tempAy > maxAge || tempAo < minAge)
	{ /* 1. DELETE NOTHING : outside the legion */
		return;
	} else if (tempAy <= minAge && tempAo >= maxAge)
	{ /* 2. DELETE ALL : include all the legions */
		m_CohortList.resize(0);
		return;
	} else if (tempAo >= maxAge)
	{ /* 3. DELETE ALL OLDER THAN ... : ao outside the legion */
		while (tempAy > m_CohortList[i].getAo())
		{
			i++;
		}
		/* reset older age of the last cohort before removal */
		/* (becoming last or penultimate cohort of the legion) */
		m_CohortList[i].setAo(min(tempAy-1, m_CohortList[i].getAo()));
		if (m_CohortList[i].getAo() < m_CohortList[i].getAy())
		{ // remove also the last cohort
			m_CohortList.resize(i);
		} else
		{
			m_CohortList.resize(i+1);
		}
		return;
	} else if (tempAy <= minAge)
	{ /* 4. DELETE ALL YOUNGER THAN ... : ay outside the legion */
		while (tempAo >= m_CohortList[0].getAo())
		{
			m_CohortList.erase(m_CohortList.begin());
		}
		/* reset younger age of the first cohort after removal */
		/* (becoming first cohort of the legion) */
		m_CohortList.front().setAy(max(tempAo+1 , m_CohortList.front().getAy()));
		return;
	} else if (tempAy >= minAge && tempAo <= maxAge)
	{ /* 5. DELETE WITHIN INTERVAL */
		i = 0;
		/* find the first cohort concerned */
		while (tempAy > m_CohortList[i].getAo())
		{
			i++;
		}
		if (m_CohortList[i].getAo() <= tempAo)
		{ // 5a. NOT IN THE SAME COHORT
			/* reset older age of the last cohort before removal */
			m_CohortList[i].setAo(min(tempAy-1 , m_CohortList[i].getAo()));
			if (m_CohortList[i].getAo() < m_CohortList[i].getAy())
			{ // remove also the last cohort
				m_CohortList.erase(m_CohortList.begin() + i);
			} else
			{
				i++;
			}
			while (tempAo >= m_CohortList[i].getAo())
			{ // remove all concerned cohort
				m_CohortList.erase(m_CohortList.begin() + i);
			}
			/* reset younger age of the first cohort after removal */
			m_CohortList[i].setAy(max(tempAo+1 , m_CohortList[i].getAy()));
			if (m_CohortList[i].getAo() < m_CohortList[i].getAy())
			{ // remove also the last cohort
				m_CohortList.erase(m_CohortList.begin() + i);
			}
			return;
		} else
		{ // 5b. ONLY ONE COHORT CONCERNED
			this->splitCohort(i, tempAy); // split cohort
			/* reset older age of the concerned cohort */
			m_CohortList[i].setAo(tempAy-1);
			if (m_CohortList[i].getAo() < m_CohortList[i].getAy())
			{ // remove also the last cohort
				m_CohortList.erase(m_CohortList.begin() + i);
			} else
			{
				i++;
			}
			/* reset younger age of the concerned cohort */
			m_CohortList[i].setAy(min(tempAo+1 , m_CohortList[i].getAo()));
			if (m_CohortList[i].getAo() < m_CohortList[i].getAy())
			{ // remove also the last cohort
				m_CohortList.erase(m_CohortList.begin() + i);
			}
			return;
		}
	}
} // end of removeCohort(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void Legion::reduceCohort(const int& ay, const int& ao, const double& reducFact)
{
	if (m_CohortList.size() > 0)
	{ /* Check that there is some individuals in the legion */

		/* declare modifiable variables */
		int tempAy = ay, tempAo = ao;

		/* check that ay < ao */
		if (ay > ao)
		{
			tempAy = ao;
			tempAo = ay;
		}

		/* 1. DELETE ALL */
		if (reducFact <= 0)
		{
			this->removeCohort(tempAy, tempAo);
		}

		for (unsigned co=0; co<m_CohortList.size(); co++)
		{
			if (m_CohortList[co].getAy() > tempAo)
			{ // no more concerned cohort : ao < getAy
				break;
			}
			if (m_CohortList[co].getAo() < tempAy)
			{ // find the first concerned cohort
				continue;
			}
			if (m_CohortList[co].getAy() < tempAy)
			{ // INSIDE THE COHORT : ay > getAy : split the cohort and call recursively
				this->splitCohort(co, tempAy-1);
				continue;
			}
			if (m_CohortList[co].getAo() > tempAo)
			{ // INSIDE THE COHORT : ao < getAo : split the cohort and call recursively
				this->splitCohort(co, tempAo);
			}

			unsigned newSize = (unsigned) ( m_CohortList[co].getCSize() * reducFact );
			if (newSize > 0)
			{ /* 2. REDUCE THIS LEGION ABUNDANCE + CONTINUE THE LOOP */
				m_CohortList[co].setCSize(newSize);
			} else
			{ /* 3. REMOVE THE COHORT */
				this->removeCohort(m_CohortList[co].getAy(), m_CohortList[co].getAo());
				co--;
			}
		}
	}
} // end of reduceCohort(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void Legion::pickupCohorts()
{
	if (m_CohortList.size() > 1)
	{ /* Check that there is more than a single cohort within the legion */
		for (unsigned co=0; co<m_CohortList.size()-1; co++)
		{
			/* check if successive cohorts :
			- have the same abundance
			- are juxtaposed (ao + 1 = ay)
			 */
			if ((m_CohortList[co].getCSize() == m_CohortList[co+1].getCSize() &&
				 (m_CohortList[co].getAo() + 1) == m_CohortList[co+1].getAy()))
				 //(m_CohortList[co].getCSize() == m_CohortList[co+1].getCSize() &&
				 //(m_CohortList[co].getAo()) == m_CohortList[co+1].getAy()))
			{
				/* pick up the 2 legions */
				m_CohortList[co].setAo(m_CohortList[co+1].getAo()); // change co legion older individuals age
				m_CohortList.erase(m_CohortList.begin() + co + 1 ); // remove co+1 legion
				pickupCohorts(); // call recursively the function
			}
		}
	}
} // end of pickupCohorts()

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
