#include "SuFateH.h"

using namespace std;

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

SuFateH::SuFateH() : SuFate(), m_EnvSuitMapPtr(new SpatialStack<double,double>()), m_EnvSuitRefMapPtr(new SpatialStack<double,double>())
{
	/* Nothing to do */
}

SuFateH::SuFateH(unsigned cellID) : SuFate(cellID), m_EnvSuitMapPtr(new SpatialStack<double,double>()), m_EnvSuitRefMapPtr(new SpatialStack<double,double>())
{
	/* Nothing to do */
}

SuFateH::SuFateH(unsigned cellID, Community comm, LightResources lightR, double soilR, IntMapPtr seedRainMap, IntMapPtr seedProdMap,
GSPPtr gspPtr, DoubleMapPtr envSuitMapPtr, DoubleMapPtr envSuitRefMapPtr) :
SuFate( cellID, comm, lightR, soilR, seedRainMap, seedProdMap, gspPtr), m_EnvSuitMapPtr(envSuitMapPtr), m_EnvSuitRefMapPtr(envSuitRefMapPtr)
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

SuFateH::~SuFateH()
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters and setters                                                                             */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

SpatialStack<double, double> SuFateH::getEnvSuitMap() { return *m_EnvSuitMapPtr; }
SpatialStack<double, double> SuFateH::getEnvSuitRefMap() { return *m_EnvSuitRefMapPtr; }
double SuFateH::getEnvSuit(unsigned fg) { return (*m_EnvSuitMapPtr)(m_CellID, fg); }
double SuFateH::getEnvSuitRefVal(unsigned fg) { return (*m_EnvSuitRefMapPtr)(m_CellID, fg); }

DoubleMapPtr SuFateH::getEnvSuitMap_() { return m_EnvSuitMapPtr; }
DoubleMapPtr SuFateH::getEnvSuitRefMap_() { return m_EnvSuitRefMapPtr; }
	
void SuFateH::setEnvSuitRefMap_( DoubleMapPtr envSuitRefMap_ ) { m_EnvSuitRefMapPtr = envSuitRefMap_; }

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SuFateH::show()
{
	SuFate::show();
	cout << "Environmental suitability of cell : ";
	for (unsigned fg=0; fg<m_Comm.getFuncGroupList().size(); fg++)
	{
		cout << (*m_EnvSuitMapPtr)(m_CellID, fg) << " ";
	}
	cout << endl;
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

double SuFateH::getEnvRecrRate(int fg)
{
	//cout << "***" << " fg" << fg << " HS" << getEnvSuit(fg) << " refHS" << getEnvSuitRefVal(fg) << endl;
	if (getEnvSuit(fg) >= getEnvSuitRefVal(fg))
	{ // not affected by environment
		return 1.0;
	} else
	{ // affected by environment
		return 0.0;
	}
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

double SuFateH::getEnvMort(int /*fg*/)
{
	return 1.0;
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

double SuFateH::getEnvGroth(int fg)
{
	if (getEnvSuit(fg) >= getEnvSuitRefVal(fg))
	{ // not affected by environment
		return 1.0;
	} else
	{ // affected by environment
		return 0.0;
	}
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

double SuFateH::getEnvFecund(int fg)
{
	if (getEnvSuit(fg) >= getEnvSuitRefVal(fg))
	{ // not affected by environment
		return 1.0;
	} else
	{ // affected by environment
		return 0.0;
	}
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

int SuFateH::getMatTime(int fg)
{
	// shift the maturation time up in proportion to life span.
	return (ceil( ( m_Comm.getFuncGroup_(fg)->getFGparams_()->getLifeSpan() - m_Comm.getFuncGroup_(fg)->getFGparams_()->getMatTime() ) * (1.0 - this->getEnvGroth(fg))
	+ m_Comm.getFuncGroup_(fg)->getFGparams_()->getMatTime() ));
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

int SuFateH::getLifeSpan(int fg)
{
	return ceil( m_Comm.getFuncGroup_(fg)->getFGparams_()->getLifeSpan() * this->getEnvMort(fg) );
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

int SuFateH::getSeedInput(int fg)
{
	if (m_Comm.getFuncGroup_(fg)->getFGparams_()->getIsAlien())
	{ // alien, not fully dispersed
		return 0;
	} else if (getEnvSuit(fg) >= getEnvSuitRefVal(fg))
	{ // not affected by environment
		return m_GSP->getSeedingInput();
	} else
	{
		return 0;
	} // affected by environment
}

