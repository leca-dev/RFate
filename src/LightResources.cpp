#include "LightResources.h"

using namespace std;

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

LightResources::LightResources() : m_ResourceList(0,RHigh)
{
	/* Nothing to do */
}

LightResources::LightResources(int noStrata) : m_ResourceList(noStrata, RHigh)
{
	/* Nothing to do */
}

LightResources::LightResources(vector<Resource> resourceList) : m_ResourceList(resourceList)
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

LightResources::~LightResources()
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

const vector<Resource>& LightResources::getResourceList() const{ return m_ResourceList; }
const Resource& LightResources::getResource(const int& id) const{ return m_ResourceList[id]; }

void LightResources::setResourceList(const vector<Resource>& resourceList){ m_ResourceList = resourceList;}
void LightResources::setResource(const int& id, const Resource& resource){ m_ResourceList[id] = resource;}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void LightResources::show()
{
	cout << "LightResources object = (column: strata)" <<endl;
	copy(m_ResourceList.begin(), m_ResourceList.end(), ostream_iterator<Resource>(cout, " "));
	cout << endl;
} // end of show()

