#include "Cohort.h"

using namespace std;

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Cohort::Cohort() : m_CSize(0), m_Ay(0), m_Ao(0)
{
	/* Nothing to do */
}

Cohort::Cohort(int cSize, int ay, int ao) : m_CSize(cSize), m_Ay(ay), m_Ao(ao)
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Cohort::~Cohort()
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

const int& Cohort::getCSize() const{ return m_CSize; }
const int& Cohort::getAy() const{ return m_Ay; }
const int& Cohort::getAo() const{ return m_Ao; }

void Cohort::setCSize(const int& cSize){ m_CSize = cSize; }
void Cohort::setAy(const int& ay){ m_Ay = ay; }
void Cohort::setAo(const int& ao){ m_Ao = ao; }

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void Cohort::show()
{
	cout << "from " << m_Ay << " to " << m_Ao << " (size: " << m_CSize << ")" << endl;
}
