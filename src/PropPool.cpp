/*================================================*/
/*             Propagules Pool Class              */
/*================================================*/

#include "PropPool.h"
#include <cmath>

using namespace std;

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

PropPool::PropPool() : m_Size(0), m_Declining(false), m_DTime(0)
{
	/* Nothing to do */
}

PropPool::PropPool(int size, bool declining, int dTime) : m_Size(size), m_Declining(declining), m_DTime(dTime)
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

PropPool::~PropPool()
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

const int& PropPool::getSize() const { return m_Size; }
const bool& PropPool::getDeclining() const { return m_Declining; }
const int& PropPool::getDTime() const { return m_DTime; }

void PropPool::setSize( const int& size ) { m_Size = size; }
void PropPool::setDeclining( const bool& declining) { m_Declining = declining; }
void PropPool::setDTime( const int& dTime) { m_DTime = dTime; }

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void PropPool::show()
{
	cout << "Seed Pool : size = " << m_Size << ", declining = " << m_Declining << ", age = " << m_DTime << endl;
} // end of show()

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void PropPool::PutSeedInPool(int Inp)
{
	if(Inp >= m_Size)
	{ // if new abundance is greater, reset the pool
		m_Size = Inp;
		m_Declining = false;
		m_DTime = 0;
	}
} // end of PutSeedInPool(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void PropPool::EmptyPool()
{
	m_Size = 0;
	m_Declining = false;
	m_DTime = 0;
} // end of EmptyPool(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void PropPool::AgePool1(int pl)
{
	if (m_Size > 0)
	{
		/* Seed mortality rate follow a linear relationship as a function of seed life */
		/* size (n+1) = size (n) - size(n) * (1 / (pl + 1)) */
		
		double decRate = 1.0 / ( (double) pl + 1.0 ); // calculate decreasing rate
		m_Declining = true; // new seeds, so the pool is declining
		m_DTime = m_DTime + 1; // increase age of youngest seeds
		m_Size = floor(m_Size - decRate * m_Size);
		
		if (m_Size == 0)
		{
			m_Declining = false;
			m_DTime = 0;
		}
	}
} // end of AgePool1(...)
