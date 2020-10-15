#include "SuFate.h"

using namespace std;

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

SuFate::SuFate() : m_CellID(0),
m_Comm(Community()), m_LightR(LightResources()), m_SoilR(0.0),
m_SeedRainMap(new SpatialStack<double,int>()), m_SeedProdMap(new SpatialStack<double,int>()),
m_GSP(new GSP())
{
	/* Nothing to do */
}

SuFate::SuFate(unsigned cellID) : m_CellID(cellID),
m_Comm(Community()), m_LightR(LightResources()), m_SoilR(0.0),
m_SeedRainMap(new SpatialStack<double,int>()), m_SeedProdMap(new SpatialStack<double,int>()),
m_GSP(new GSP())
{
	/* Nothing to do */
}

SuFate::SuFate(unsigned cellID, Community comm, LightResources lightR, double soilR,
IntMapPtr seedRainMap, IntMapPtr seedProdMap, GSPPtr gspPtr) : m_CellID(cellID),
m_Comm(comm), m_LightR(lightR), m_SoilR(soilR), m_SeedRainMap(seedRainMap), m_SeedProdMap(seedProdMap),
m_GSP(gspPtr)
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

SuFate::~SuFate()
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

const unsigned SuFate::getCellID() const { return m_CellID; }
const Community SuFate::getCommunity() const { return m_Comm; }
LightResources SuFate::getLightResources() { return m_LightR; }
double SuFate::getSoilResources() { return m_SoilR; }
SpatialStack<double, int> SuFate::getSeedRain() { return *m_SeedRainMap; }
SpatialStack<double, int> SuFate::getSeedProd() { return *m_SeedProdMap; }
int SuFate::getSeedRain(unsigned fg) { return (*m_SeedRainMap)( m_CellID, fg); }
int SuFate::getSeedProd(unsigned fg) { return (*m_SeedProdMap)( m_CellID, fg); }
GSP SuFate::getGSP() { return *m_GSP; }

CommunityPtr SuFate::getCommunity_() { return &m_Comm; }
LightResourcesPtr SuFate::getLightResources_() { return &m_LightR; }
IntMapPtr SuFate::getSeedRain_() { return m_SeedRainMap; }
IntMapPtr SuFate::getSeedProd_() { return m_SeedProdMap; }
int* SuFate::getSeedRain_(unsigned fg) { return &( (*m_SeedRainMap)( m_CellID, fg) ); }
int* SuFate::getSeedProd_(unsigned fg) { return &( (*m_SeedProdMap)( m_CellID, fg) ); }
GSPPtr SuFate::getGSP_() { return m_GSP; }

void SuFate::setCommunity(const Community comm) { m_Comm.setFuncGroupList(comm.getFuncGroupList()); }
void SuFate::setLightResources(const LightResources lightR) { m_LightR.setResourceList(lightR.getResourceList()); }
void SuFate::setSoilResources(double soilR) { m_SoilR = soilR; }
void SuFate::setSeedRain(unsigned fg, int seedRain) { m_SeedRainMap->setValue(m_CellID, fg, seedRain); }
void SuFate::setSeedProd(unsigned fg, int seedProd) { m_SeedProdMap->setValue(m_CellID, fg, seedProd); }


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SuFate::show()
{
	int noFG = m_Comm.getFuncGroupList().size();
	logg.debug("Fate Succession Object",
						 "\nNumber of Functional Groups = ", noFG);  // nb of PFGs
	for (int i=0; i<noFG; i++)  // print PFG names
 	{
		 logg.debug(m_Comm.getFuncGroup_(i)->getFGparams_()->getName(), " ");  // TODO : make a method on Community class
 	}
	logg.debug(")",
						 "\nSeeds rain =");
 	for (int i=0; i<noFG; i++)
 	{
 		logg.debug(" ", (*m_SeedRainMap)(m_CellID, i));  // TODO : make a method somewhere
 	}

 	/* print produced seeds */
 	logg.debug("Produced seeds =");
 	for (int i=0; i<noFG; i++)
 	{
 		logg.debug(" ", (*m_SeedProdMap)(m_CellID, i));  // TODO : make a method somewhere
 	}

 	/* print light condition */
 	m_LightR.show();

 	/* print communities summary */
 	m_Comm.summary();

 	/* print soil conditions */
 	logg.debug("Soil resources = ", m_SoilR);
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Environment response model                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

// TODO (damien#2#): add dependencies between environment and changing strata ages

/* There seems to be a fundamental flaw in this in that the more PFGs there are
   the greater the abundance */

void SuFate::CalculateEnvironment()
{
	if (m_GSP->getDoLightInteraction() || m_GSP->getDoSoilInteraction())
	{
		vector< int > stProfile(m_GSP->getNoStrata(), 0);
		int noFG = int(m_Comm.getFuncGroupList().size());
		int noFG_pres = 0;

		vector < int > AbundPFG(noFG,0); // vector to store abundances of PFGs

		for (int fg=0; fg < noFG; fg++)
		{
			/* initialize strata abundance */
			vector< int > StratX(m_GSP->getNoStrata(), 0);

			/* create a copy of FG parameters to simplify and speed up the code */
			FGPtr FGparams = m_Comm.getFuncGroup_(fg)->getFGparams_();

			/* Create a vector with stratum break ages */
			vector<int> bkStratAges = FGparams->getStrata();

			if (m_Comm.getNoCohort(fg) > 0)
			{
				noFG_pres++; /* add 1 to pfg counter */

				/* Fill the stratum according to Cohorts of each PFG */
				for (int co=0; co<m_Comm.getNoCohort(fg); co++)
				{
					/* Keep Legion Params */
					int ayTemp = m_Comm.getAy(fg, co);
					int aoTemp = m_Comm.getAo(fg, co);
					int csizeTemp = m_Comm.getCSize(fg, co);

					int st = 0; /* Stratum counter */
					while (ayTemp >= bkStratAges[st+1]){ st++; } /* Get the first stratum which Legion filled */

					if (aoTemp >= bkStratAges[st+1]) /* Legion cover more than a lone stratum */
					{
						int stm = st; /* Stratum counter */
						while (aoTemp >= bkStratAges[stm+1])
						{
							/* change stratum? */
							if(ayTemp >= bkStratAges[stm+1]) { stm++; }
							/* fill the current stratum according to mature and immature plants proportions */
							if (min(bkStratAges[stm+1]-1,aoTemp) < this->getMatTime(fg))
							{
								/* only immature plants */
								StratX[stm] += ceil(FGparams->getImmSize() * csizeTemp * ( min(bkStratAges[stm+1]-1,aoTemp) - ayTemp+1));
							} else if (ayTemp >= this->getMatTime(fg))
							{
								/* only mature plants */
								StratX[stm] += ceil(csizeTemp * ( min(bkStratAges[stm+1]-1,aoTemp) -ayTemp+1));
							} else
							{
								/* immature and mature plants are both present */
								StratX[stm] += ceil(FGparams->getImmSize() * csizeTemp * (min(this->getMatTime(fg)-1,aoTemp)-ayTemp+1)); /* Immature part */
								StratX[stm] += ceil(csizeTemp * (min(bkStratAges[stm+1]-1,aoTemp)-min(this->getMatTime(fg),aoTemp)+1)); /* Mature part */
							}
							/* remove part of legion treated */
							ayTemp = bkStratAges[stm+1];
						}
					} else
					{
						/* the entire Legion is into a unique stratum */
						/* fill the stratum according to mature and imature plants proportions */
						if (aoTemp < this->getMatTime(fg))
						{
							/* only immature plants */
							StratX[st] += ceil(FGparams->getImmSize() * csizeTemp * ( aoTemp - ayTemp +1));
						} else if (ayTemp >= this->getMatTime(fg))
						{
							/* only mature plants */
							StratX[st] += ceil(csizeTemp * ( aoTemp - ayTemp +1));
						} else
						{
							/* immature and mature plants are both present */
							StratX[st] += ceil(FGparams->getImmSize() * csizeTemp * ( this->getMatTime(fg) - 1 - ayTemp + 1 )); /* Immature part */
							StratX[st] += ceil(csizeTemp * ( aoTemp - this->getMatTime(fg)+1)); /* Mature part */
						}
					}
				} // end loop on strata

				/* add PFG strata abundances */
				for (unsigned st=0; st<stProfile.size(); st++)
				{
					stProfile[st] += StratX[st]; /* Abundances per stratum, to be converted into light resources */
					AbundPFG[fg] += StratX[st]; /* Abundances per PFG, to be converted into soil resources */
				}
			}
		} // end loop on PFG

		/* compute the weighted mean of soil contributions (optional) */
		if (m_GSP->getDoSoilInteraction())
		{
			double soilResource = 0.0;
			if (noFG_pres > 0)
			{
				int TotAbund = accumulate(AbundPFG.begin(),AbundPFG.end(),0);

				/* test if we have a full coverage or not to calculate soil resources */
				if (TotAbund > 0)
				{
					for (int fg=0; fg<noFG; fg++)
					{
						if (AbundPFG[fg] > 0)
						{
							soilResource += ( AbundPFG[fg] / double(TotAbund)) * m_Comm.getFuncGroup_(fg)->getFGparams_()->getSoilContrib();
						}
					}
					/* update soil resources */
					setSoilResources(soilResource + m_GSP->getSoilRetention() * (getSoilResources() - soilResource));
					//setSoilResources(soilResource);
				}

			}
		}

		/* attribute light values to each stratum (optional) */
		/* Work down the strata calculating */
		if (m_GSP->getDoLightInteraction())
		{
			int XAbove = 0;
			for (int Stm = (m_GSP->getNoStrata() - 1); Stm >= 0; Stm--) /* resource availabilities */
			{
				if (XAbove < m_GSP->getLightThreshMedium())
				{
					m_LightR.setResource(Stm,RHigh);
				} else if (XAbove < m_GSP->getLightThreshLow())
				{
					m_LightR.setResource(Stm,RMedium);
				} else
				{
					m_LightR.setResource(Stm,RLow);
				}
				XAbove = XAbove + stProfile[Stm];
			}
		}
	}
} // end of CalculateEnvironment(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SuFate::CheckSurvival()
{
	unsigned noFG = m_Comm.getFuncGroupList().size();

	if (m_GSP->getDoSoilInteraction())
	{
		for (unsigned fg = 0; fg < noFG; fg++)
		{
			int noCohort = m_Comm.getNoCohort(fg);
			if (noCohort > 0)
			{
				/* create a copy of FG parameters to simplify and speed up the code */
				FuncGroupPtr FuncG = m_Comm.getFuncGroup_(fg);
				FGPtr FGparams = FuncG->getFGparams_();
				LegionPtr FGlegion = FuncG->getLList_();

				Resource soilRes = RMedium;
				if (m_SoilR < FGparams->getSoilLow())
				{
					soilRes = RLow;
				} else if (m_SoilR > FGparams->getSoilHigh())
				{
					soilRes = RHigh;
				}

				/* check germinant plants survival */
				FGlegion->reduceCohort(0, 0, FractToDouble(FGparams->getSoilTolerance()[Germinant][soilRes]));
				/* check immature plants survival */
				FGlegion->reduceCohort(0, FGparams->getMatTime() - 1, FractToDouble(FGparams->getSoilTolerance()[Immature][soilRes]));
				/* check mature plants survival */
				FGlegion->reduceCohort(FGparams->getMatTime(), FGparams->getLifeSpan()+1, FractToDouble(FGparams->getSoilTolerance()[Mature][soilRes]));

			}
		}
	}

	if (m_GSP->getDoLightInteraction())
	{
		for (unsigned fg = 0; fg < noFG; fg++)
		{
			int noCohort = m_Comm.getNoCohort(fg);
			if (noCohort > 0)
			{
				/* create a copy of FG parameters to simplify and speed up the code */
				FuncGroupPtr FuncG = m_Comm.getFuncGroup_(fg);
				FGPtr FGparams = FuncG->getFGparams_();
				LegionPtr FGlegion = FuncG->getLList_();

				/* Create a vector with stratum break ages */
				vector<int> bkStratAges = FGparams->getStrata();

				/* Fill the stratum according to Cohorts of each PFG */
				int co = 0;
				while (co < noCohort)
				{
					/* Keep Legion Params */
					int ayTemp = m_Comm.getAy(fg,co);
					int aoTemp = m_Comm.getAo(fg,co);

					/* Get the first stratum which Legion filled */
					int st = 0;
					while (ayTemp >= bkStratAges[st+1]) { st++; }

					/* only matures or only immature plants in this Legion */
					if (ayTemp >= this->getMatTime(fg) || aoTemp < this->getMatTime(fg))
					{
						bool survive; /* are the plants able to survive or not */
						/* check if plants are able to survive in this strata */
						if (ayTemp >= this->getMatTime(fg))
						{ // only mature plants
							survive = FGparams->getTolerance(Mature , m_LightR.getResource(st));
						} else
						{
							survive = FGparams->getTolerance(Immature , m_LightR.getResource(st));
						}
						if (survive)
						{ /* If plants survives */
							if (aoTemp < bkStratAges[st+1])
							{ /* All Legion Plants are in the same stratum, The whole Legion survives */
								co++;
							} else
							{	/* Plants covered more than a lone stratum */
								/* We are just sure that individuals in this stratum can survive */
								FGlegion->splitCohort(co, bkStratAges[st+1]-1);
								noCohort++;
								co++;
							}
						} else
						{	/* If some plants die, individuals in this stratum die */
							FGlegion->removeCohort(ayTemp, min(aoTemp, bkStratAges[st+1]-1));
							noCohort = m_Comm.getNoCohort(fg);
						}
					} else
					{	/* If there are both immatures and matures plants in this Legion */
						/* split the legion according to maturation age */
						FGlegion->splitCohort(co, this->getMatTime(fg)-1);
						noCohort++;
					}
				}
				/* Pick up legions having same size and following ages */
				FGlegion->pickupCohorts();
			}
		}
	}
} // end of CheckSurvival(...)


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Succession model                                                                                */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

double SuFate::getEnvRecrRate(int /*fg*/)
{
	return 1;
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

double SuFate::getEnvMort(int /*fg*/)
{
	return 1;
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

double SuFate::getEnvGroth(int /*fg*/)
{
	return 1;
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

double SuFate::getEnvFecund(int /*fg*/)
{
	return 1;
}


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

int SuFate::getMatTime(int fg)
{
	return ceil( m_Comm.getFuncGroup_(fg)->getFGparams_()->getMatTime() );
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

int SuFate::getLifeSpan(int fg)
{
	return ceil( m_Comm.getFuncGroup_(fg)->getFGparams_()->getLifeSpan() );
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

double SuFate::calcFecund(int fg)
{
	FuncGroupPtr FuncG = m_Comm.getFuncGroup_(fg);
	FGPtr FGparams = FuncG->getFGparams_();

	double matAbund = 0;
	/* get mature Abundance */
	if (this->getMatTime(fg) <= this->getLifeSpan(fg))
	{
		matAbund = FuncG->totalNumAbund(this->getMatTime(fg), this->getLifeSpan(fg)) /
		(double) ( m_GSP->AbundToInt(FGparams->getMaxAbund())
		/ (this->getLifeSpan(fg) - this->getMatTime(fg)) );
	}
	return min(matAbund, 1.0) * FGparams->getPotentialFecund() * this->getEnvFecund(fg);
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

int SuFate::getSeedInput(int fg)
{
	if (m_Comm.getFuncGroup_(fg)->getFGparams_()->getIsAlien())
	{ // alien, not fully dispersed
		return 0;
	} else
	{
		return m_GSP->getSeedingInput();
	}
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

/*''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  This is the main succession routine. It has been coded for clarity as opposed
  to speed; it works by simulating forward in single time steps.

  In each time step the procedure is:
  1. Age the established plants
  2. Determine the environmental conditions and check the tolerance of
  established plants; then recalculate the environmental conditions for
  recruitment
  3. Determine the size of the propagule rain (local and external)
  4. Determine the germination rate from the number of seeds available for
  germination and the resource level in stratum 0 (i.e. enforced dormancy)
  5. If the species has pulse germination, empty the active seed pool
  6. Age the seed pools
  7. Now place the propagules in the pool (this ensures that shortlived
  propagules at least survive through disturbances).
  8. Actually recruit a cohort only if germinants are able to withstand
  the environment in stratum 0.
  ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,*/

/*  Comm is the community pointer;
    Time returns the year on return to the calling
    procedure (this occurs if the broad cescription of the community changes),
    Maxyears is the maximum time to run this succession */


void SuFate::DoSuccessionPart1(vector<unsigned> isDrought)
{
	unsigned noFG = m_Comm.getFuncGroupList().size();
	if (noFG > 0)
	{
		/* 2. Kill established plants which cannot */
		/* tolerate current resource availability  */
		/* do soil (optional) and light interaction */
		CheckSurvival();

		/* 1. Age established plants   */
		/* recalculate the environment */
		for (unsigned fg = 0; fg < noFG; fg++)
		{
			this->getCommunity_()->getFuncGroup_(fg)->ageLegions( ceil( this->getLifeSpan(fg) ) );
		}

		CalculateEnvironment();

		DoSuccessionPart2(isDrought);
	}
}

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

void SuFate::DoSuccessionPart2(vector<unsigned> isDrought)
{
	bool doLight = m_GSP->getDoLightInteraction();
	bool doSoil = m_GSP->getDoSoilInteraction();

	unsigned noFG = m_Comm.getFuncGroupList().size();
	for (unsigned fg = 0; fg < noFG; fg++)
	{
		/* create a copy of FG parameters to simplify and speed up the code */
		FuncGroupPtr FuncG = m_Comm.getFuncGroup_(fg);
		FGPtr FGparams = FuncG->getFGparams_();
		PropPool* App_ptr = FuncG->getPools_(ActiveP);
		PropPool* Dpp_ptr = FuncG->getPools_(DormantP);

		/* Soil resources */
		Resource soilRes = RMedium;
		if (m_SoilR < FGparams->getSoilLow())
		{
			soilRes = RLow;
		} else if (m_SoilR > FGparams->getSoilHigh())
		{
			soilRes = RHigh;
		}

		/* 3. Calculate the seed rain */
		int SeedInput, AvailSeeds;
		if (FGparams->getDispersed() == 1)
		{
			SeedInput = max(getSeedInput(fg), (int)getSeedRain(fg));
		} else
		{
			SeedInput = getSeedRain(fg);
		}

		if (FGparams->getInnateDormancy())
		{
			AvailSeeds = App_ptr->getSize();
		} else
		{
			AvailSeeds = max( int( App_ptr->getSize() ), int( SeedInput ) ) ;
		}

		/* 4. Germination is a function of the degree of enforced dormancy and of the size of the pool of available seeds */
		double GerminRate = AvailSeeds;
		if (doLight && doSoil)
		{
			Fract maxRecruitLight0 = FGparams->getMaxRecruitLight( m_LightR.getResource(0) );
			Fract maxRecruitSoil0 = FGparams->getMaxRecruitSoil( soilRes );

			//GerminRate *= min(FractToDouble( maxRecruitLight0 ), FractToDouble( maxRecruitSoil0 ));
			GerminRate *= (FractToDouble( maxRecruitLight0 ) * FractToDouble( maxRecruitSoil0 ));

			/* 5. If all available seeds germinated, zero the active seed pool */
			if (maxRecruitLight0 == PC100 && maxRecruitSoil0 == PC100)
			{
				App_ptr->EmptyPool();
			}
		} else if (doLight)
		{
			Fract maxRecruitLight0 = FGparams->getMaxRecruitLight( m_LightR.getResource(0) );
			GerminRate *= FractToDouble( maxRecruitLight0 );

			/* 5. If all available seeds germinated, zero the active seed pool */
			if (maxRecruitLight0 == PC100)
			{
				App_ptr->EmptyPool();
			}
		} else if (doSoil)
		{
			Fract maxRecruitSoil0 = FGparams->getMaxRecruitSoil( soilRes );
			GerminRate *= FractToDouble( maxRecruitSoil0 );

			/* 5. If all available seeds germinated, zero the active seed pool */
			if (maxRecruitSoil0 == PC100)
			{
				App_ptr->EmptyPool();
			}
		} else
		{
			/* 5. If all available seeds germinated, zero the active seed pool */
			App_ptr->EmptyPool();
		}

		/* 6. Age the propagule pools */
		App_ptr->AgePool1(FGparams->getPoolLife(ActiveP));
		if (FGparams->getInnateDormancy())
		{
			Dpp_ptr->AgePool1(FGparams->getPoolLife(ActiveP));
		}

		/* 7. Place the seeds in the appropriate pool */
		if (FGparams->getInnateDormancy())
		{
			Dpp_ptr->PutSeedInPool( SeedInput );
		} else
		{
			App_ptr->PutSeedInPool( SeedInput );
		}

		/* 8. Establishment depends upon the germinants being able to withstand the environment in stratum 0 */
		bool doRecruit = true;
		if (doLight && doSoil)
		{
			doRecruit = ( FGparams->getTolerance()[ Germinant ][ m_LightR.getResource(0) ] && (soilRes == RMedium) );
		} else if (doLight)
		{
			doRecruit = FGparams->getTolerance()[ Germinant ][ m_LightR.getResource(0) ];
		} else if (doSoil)
		{
			doRecruit = (soilRes == RMedium);
		}
		if (doRecruit)
		{
			// do recruitment only if abundance is < to max abund * (1 + ImmSize)
			double totAbund = FuncG->totalNumAbund( 1, this->getLifeSpan(fg) );
			double totMaxAbund = 1.0 * m_GSP->AbundToInt(FGparams->getMaxAbund()) * (1 + FGparams->getImmSize());
			if (totAbund < totMaxAbund)
			{
				double envRecruit = getEnvRecrRate(fg);
				if (isDrought[fg]){ envRecruit = 0.0; }
				int recrrate = ceil(GerminRate * envRecruit);   /* Recruitment is ponderated by environmental suitabilities */
				if (recrrate > 0)
				{
					FuncG->getLList_()->addCohort( (int) recrrate, 0, 0);
				}
			}
		}

		/* 9. Update Fecundity */
		if (isDrought[fg])
		{
			setSeedProd(fg, 0.0);
		} else
		{
			setSeedProd(fg, max(0.0, this->calcFecund(fg)));
		}
	}
}// end of SuFate::DoSuccessionPart2(...)

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

void SuFate::DoUnaffected(int fg, int Dstb, FGresponse FGresp)
{
	LegionPtr FGlegion = m_Comm.getFuncGroup_(fg)->getLList_();

	int noRange = FGresp.getResprAge().front().size(); /* The number of way to react to a disturbance */
	for (int range=0; range<noRange; range++)
	{
		/* get FG break age for considered disturbance and FG class age */
		int ageStart = FGresp.getBreakAge(Dstb, range); /* Disturbance age delimiters */
		int ageStop = max(FGresp.getBreakAge(Dstb, range + 1) - 1, ageStart); // if we have a only one year disturbance class then ageStart = ageStop
		double pcUnaff = FractToDouble(FGresp.getFates(Dstb, range, Unaff )); /* Unaffected plant percentage */

		if (m_Comm.getNoCohort(fg) > 0)
		{
			/* remove cohorts if all plants are affected */
			if (pcUnaff <= 0.0)
			{
				FGlegion->removeCohort(ageStart, ageStop);
			} else
			{
				/* reduce cohorts abundances if some plants are not unaffected */
				if (pcUnaff < 1.0)
				{
					FGlegion->reduceCohort(ageStart, ageStop, pcUnaff);
				}
			}
		}
	} // end loop over ranges
} // end of DoUnaffected()

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SuFate::DoDisturbance(int fg, int Dstb, FGresponse FGresp )
{
	if (m_Comm.getNoCohort(fg) > 0)
	{
		FuncGroupPtr FuncG = m_Comm.getFuncGroup_(fg);
		LegionPtr FGlegion = FuncG->getLList_();

		/* Get number of resprouting plants */
		/* Resprouting plants counter reinitialisation */
		int noRange = FGresp.getFates()[Dstb].size();
		vector< int > ResprC(noRange,0);
		//fill(ResprC.begin(), ResprC.end(), 0);

		for (int co=0; co<m_Comm.getNoCohort(fg); co++)
		{
			for (int range=0; range<noRange; range++)
			{
				ResprC[range] += ceil( FractToDouble( FGresp.getFates(Dstb, range, Respr ) ) * m_Comm.getCSize(fg,co) *
				fmax( fmin( m_Comm.getAo(fg,co), FGresp.getBreakAge(Dstb, range+1) - 1 ) -
				fmax( m_Comm.getAy(fg,co), FGresp.getBreakAge(Dstb, range) ) + 1, 0 ) ); //rescale to apply on range-1
			}
		}

		/* Calculation of unaffected plants */
		DoUnaffected( fg, Dstb, FGresp );

		/* Make plant resprout */
		for (int range=0; range<noRange; range++)
		{
			if (ResprC[range] > 0)
			{
				FGlegion->addCohort(ResprC[range], FGresp.getResprAge(Dstb, range), FGresp.getResprAge(Dstb, range));
			}
		}

		/* Seeds pool perturbation part */
		PropPool* App_ptr = FuncG->getPools_(ActiveP);
		PropPool* Dpp_ptr = FuncG->getPools_(DormantP);

		/* Kill active seeds */
		App_ptr->setSize( ceil(App_ptr->getSize() - App_ptr->getSize() * FractToDouble( FGresp.getPropKilled(Dstb) ) ) );

		/* Transfer Dormant seeds to active seed pool */
		if (FuncG->getFGparams_()->getInnateDormancy())
		{
			Fract dormbreaks = FGresp.getDormBreaks(Dstb);
			App_ptr->setSize( fmin(App_ptr->getSize() + Dpp_ptr->getSize() * FractToDouble(dormbreaks), 100) ) ;
			if (dormbreaks == PC100)
			{
				Dpp_ptr->EmptyPool();
			} else
			{
				Dpp_ptr->setSize( ceil( Dpp_ptr->getSize() * ( 1.0 - FractToDouble(dormbreaks) ) ) );
			}
		}
	}
} // end of DoDisturbance(...)
