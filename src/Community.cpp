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

#include "Community.h"

using namespace std;

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Community::Community() : m_FuncGroupList(0,FuncGroup())
{
	/* Nothing to do */
}

Community::Community(vector<FuncGroup> funcGroupList) : m_FuncGroupList(funcGroupList)
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Community::~Community()
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

const vector<FuncGroup>& Community::getFuncGroupList() const{ return m_FuncGroupList; }
const FuncGroup& Community::getFuncGroup(const int& id) const{ return m_FuncGroupList[id]; }

FuncGroup* Community::getFuncGroup_(const int& id) { return &m_FuncGroupList[id]; }

int Community::getNoCohort(const int& id) { return m_FuncGroupList[id].getLList().getNoCohort(); }

int Community::getAy(const int& id, const int& co) const { return m_FuncGroupList[id].getLList().getCohort(co).getAy(); }
int Community::getAo(const int& id, const int& co) const { return m_FuncGroupList[id].getLList().getCohort(co).getAo(); }
int Community::getCSize(const int& id, const int& co) const { return m_FuncGroupList[id].getLList().getCohort(co).getCSize(); }

void Community::setFuncGroupList(const vector<FuncGroup>& funcGroupList){ m_FuncGroupList = funcGroupList;}
void Community::setFuncGroup(const int& id, const FuncGroup& funcGroup){ m_FuncGroupList[id] = funcGroup;}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void Community::show()
{
	logg.debug("Community object :");
	for(unsigned i=0; i<m_FuncGroupList.size(); i++)
	{
		m_FuncGroupList[i].show();
	}
} // end of show()

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void Community::summary()
{
	logg.debug("Community summary :");
	for(unsigned i=0; i<m_FuncGroupList.size(); i++)
	{
		m_FuncGroupList[i].summary();
	}
} // end of summary()


//void Community::showNames()
//{
//	string names("( ");
//	typename vector<FuncGroup>::const_iterator fg;
//	for (fg = m_FuncGroupList.begin(); fg != m_FuncGroupList.end(); ++fg)
//	{
//		names += fg->getFGparams_()->getName() + " ";
//	}
//	names += ")";
//	logg.debug(names);
//}
