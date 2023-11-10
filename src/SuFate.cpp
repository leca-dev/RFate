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
               m_Comm(comm), m_LightR(lightR), m_SoilR(soilR),
               m_SeedRainMap(seedRainMap), m_SeedProdMap(seedProdMap),
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

unsigned SuFate::getCellID() const { return m_CellID; }
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

void SuFate::CalculateEnvironment()
{
  
  /* if light and/or soil interaction, calculate abundances per stratum */
  if (m_GSP->getDoLightInteraction() || m_GSP->getDoSoilInteraction())
  {
    int noFG = m_Comm.getFuncGroupList().size();
    int noFG_pres = 0;
    
    vector<int> AbundPFG(noFG, 0); // vector to store abundances of PFGs
    vector<int> stProfile(m_GSP->getNoStrata()+1, 0); // vector to store light unities (abundances * shade factor)
    
    for (int fg=0; fg < noFG; fg++)
    {
      /* create a copy of FG parameters to simplify and speed up the code */
      FGPtr FGparams = m_Comm.getFuncGroup_(fg)->getFGparams_();
      
      /* Create a vector with stratum break ages */
      vector<int> bkStratAges = FGparams->getStrata();

      if (m_Comm.getNoCohort(fg) > 0)
      {
        noFG_pres++; /* add 1 to pfg counter */
      
        /* add PFG strata abundances */
        for (unsigned st=1; st<stProfile.size(); st++)
        {
          int StratX = static_cast<int>(m_Comm.getFuncGroup_(fg)->totalNumAbund( bkStratAges[st-1] , bkStratAges[st] - 1 ));
          stProfile[st] += StratX * FGparams->getLightShadeFactor(); /* Abundances per stratum, to be converted into light resources */
          AbundPFG[fg] += StratX; /* Abundances per PFG, to be converted into soil resources */
        } // end loop on strata
      }
    } // end loop on PFG
    
    /* compute the weighted mean of soil contributions (optional) */
    if (m_GSP->getDoSoilInteraction())
    {
      double soilResource = 0.0;
      if (noFG_pres > 0)
      {
        int TotAbund = accumulate(AbundPFG.begin(), AbundPFG.end(), 0);
        
        /* test if we have a full coverage or not to calculate soil resources */
        if (TotAbund > 0)
        {
          for (int fg=0; fg<noFG; fg++)
          {
            if (AbundPFG[fg] > 0)
            {
              soilResource += ( AbundPFG[fg] / static_cast<double>(TotAbund)) * m_Comm.getFuncGroup_(fg)->getFGparams_()->getSoilContrib();
            }
          }
          /* update soil resources */
          setSoilResources(soilResource + m_GSP->getSoilRetention() * (getSoilResources() - soilResource));
        }
      }
    }
    
    /* attribute light values to each stratum (optional) */
    /* Work down the strata calculating */
    if (m_GSP->getDoLightInteraction())
    {
      int XAbove = 0;
      for (int Stm = m_GSP->getNoStrata(); Stm > 0; Stm--) /* resource availabilities */
      {
        XAbove = XAbove + stProfile[Stm];
        if (XAbove < m_GSP->getLightThreshMedium())
        {
          m_LightR.setResource(Stm-1, RHigh);
        } else if (XAbove < m_GSP->getLightThreshLow())
        {
          m_LightR.setResource(Stm-1, RMedium);
        } else
        {
          m_LightR.setResource(Stm-1, RLow);
        }
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
        FGlegion->reduceCohort(0, 0, IntToDouble(FGparams->getSoilTolerance()[Germinant][soilRes]));
        /* check immature plants survival */
        FGlegion->reduceCohort(0, FGparams->getMatTime() - 1, IntToDouble(FGparams->getSoilTolerance()[Immature][soilRes]));
        /* check mature plants survival */
        FGlegion->reduceCohort(FGparams->getMatTime(), FGparams->getLifeSpan()+1, IntToDouble(FGparams->getSoilTolerance()[Mature][soilRes]));
        
        /* Pick up legions having same size and following ages */
        FGlegion->pickupCohorts();
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
          int ayTemp = m_Comm.getAy(fg, co);
          int aoTemp = m_Comm.getAo(fg, co);
          
          /* Get the first stratum which Legion filled */
          int st = 0;
          while (ayTemp >= bkStratAges[st]) { st++; }
          if (st == m_GSP->getNoStrata()) { break; } // last stratum, get out of the loop
          
          /* only matures or only immature plants in this Legion */
          if (ayTemp >= this->getMatTime(fg) || aoTemp < this->getMatTime(fg))
          {
            int survivePercent; /* are the plants able to survive or not */
            /* check if plants are able to survive in this strata */
            if (ayTemp >= this->getMatTime(fg))
            { // only mature plants
              survivePercent = FGparams->getLightTolerance(Mature , m_LightR.getResource(st));
            } else
            { // only germinant or immature plants
              if (st == 0 && aoTemp == 0) {
                survivePercent = FGparams->getLightTolerance(Germinant , m_LightR.getResource(st));
              } else {
                survivePercent = FGparams->getLightTolerance(Immature , m_LightR.getResource(st));
              }
            }
            if (survivePercent > 0)
            { /* If all or part of the plants survives */
              if (aoTemp < bkStratAges[st])
              { /* All Legion Plants are in the same stratum, The whole or part of the Legion survives */
                if (survivePercent < 100)
                {
                  FGlegion->reduceCohort(co, IntToDouble(survivePercent));
                }
                if (noCohort == m_Comm.getNoCohort(fg)) {
                  co++;
                } else {
                  noCohort = m_Comm.getNoCohort(fg);
                }
              } else
              {	/* Plants covered more than a lone stratum */
                /* We are just sure that some individuals in this stratum might survive */
                FGlegion->splitCohort(co, bkStratAges[st]-1);
                noCohort++;
              }
            } else
            {	/* If all plants die, individuals in this stratum die */
              FGlegion->removeCohort(ayTemp, min(aoTemp, bkStratAges[st]-1));
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
  
  double matAbund = 0.0;
  /* get mature Abundance */
  if (this->getMatTime(fg) < this->getLifeSpan(fg))
  {
    // matAbund = FuncG->totalNumAbund(this->getMatTime(fg), this->getLifeSpan(fg)) /
    //   (1.0 * m_GSP->AbundToInt(FGparams->getMaxAbund()));
    matAbund = FuncG->totalNumAbund(this->getMatTime(fg), this->getLifeSpan(fg));
  }
  // if (matAbund > 0)
  // {
  //   cout << "PFG : " << fg << ", MAT ABUND : " << matAbund << endl;
  //   cout << "PFG : " << fg << ", FECUND : " << min(matAbund, 1.0) * FGparams->getPotentialFecund() * this->getEnvFecund(fg) << endl;
  // }
  // return min(matAbund, 1.0) * FGparams->getPotentialFecund() * this->getEnvFecund(fg);
  return min(matAbund, static_cast<double>(m_GSP->AbundToInt(FGparams->getMaxAbund()))) * FGparams->getPotentialFecund() * this->getEnvFecund(fg);
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
    
    /* 6. Age the propagule pools */
    App_ptr->AgePool1(FGparams->getPoolLife(ActiveP));
    if (FGparams->getInnateDormancy())
    {
      Dpp_ptr->AgePool1(FGparams->getPoolLife(ActiveP));
    }
    
    /* 3. Calculate the seed rain */
    int SeedInput, AvailSeeds;
    if (FGparams->getDispersed() == 1)
    {
      SeedInput = max(getSeedInput(fg), static_cast<int>(getSeedRain(fg)));
      //SeedInput = getSeedInput(fg);
    } else
    {
      SeedInput = getSeedRain(fg);
    }
    
    if (FGparams->getInnateDormancy())
    {
      AvailSeeds = Dpp_ptr->getSize();
    } else
    {
      AvailSeeds = static_cast<int>(App_ptr->getSize()) + static_cast<int>(SeedInput) ;
    }
    
    /* 5. Zero the active seed pool */
    App_ptr->EmptyPool();
    
    
    /* 8. Establishment depends upon the germinants being able to withstand the environment in stratum 0 */
    bool doRecruit = true;
    if (doLight && m_GSP->getLightRecruitment() && doSoil && m_GSP->getSoilRecruitment())
    {
      doRecruit = ( FGparams->getLightTolerance()[ Germinant ][ m_LightR.getResource(0) ] > 0 && (soilRes == RMedium) );
    } else if (doLight && m_GSP->getLightRecruitment())
    {
      doRecruit = (FGparams->getLightTolerance()[ Germinant ][ m_LightR.getResource(0) ] > 0);
    } else if (doSoil && m_GSP->getSoilRecruitment())
    {
      doRecruit = (soilRes == RMedium);
    }
    
    if (doRecruit)
    {
      /* 4. Germination is a function of the degree of enforced dormancy and of the size of the pool of available seeds */
      double GerminRate = static_cast<double>(min( m_GSP->AbundToInt(FGparams->getMaxAbund()), AvailSeeds )) ;
      if (doLight && doSoil)
      {
        int maxRecruitLight0 = FGparams->getMaxRecruitLight( m_LightR.getResource(0) );
        int maxRecruitSoil0 = FGparams->getMaxRecruitSoil( soilRes );
        
        GerminRate *= min(IntToDouble(maxRecruitLight0), IntToDouble(maxRecruitSoil0));
        // GerminRate *= (IntToDouble(maxRecruitLight0) * IntToDouble(maxRecruitSoil0));
      } else if (doLight)
      {
        int maxRecruitLight0 = FGparams->getMaxRecruitLight( m_LightR.getResource(0) );
        GerminRate *= IntToDouble(maxRecruitLight0);
      } else if (doSoil)
      {
        int maxRecruitSoil0 = FGparams->getMaxRecruitSoil( soilRes );
        GerminRate *= IntToDouble(maxRecruitSoil0);
      }
      
      // do recruitment only if abundance is < to max abund * (1 + ImmSize)
      double totAbund = FuncG->totalNumAbund( 1, this->getLifeSpan(fg) );
      double totMaxAbund = static_cast<double>(m_GSP->AbundToInt(FGparams->getMaxAbund())) * (1.0 + IntToDouble(FGparams->getImmSize()));
      if (totAbund < totMaxAbund)
      {
        double envRecruit = getEnvRecrRate(fg);
        if (isDrought[fg]){ envRecruit = 0.0; }
        int recrrate = ceil(GerminRate * envRecruit);   /* Recruitment is ponderated by environmental suitabilities */
        // recrrate = min( static_cast<int>(totMaxAbund - totAbund), recrrate ) ;
        // recrrate = min( m_GSP->AbundToInt(FGparams->getMaxAbund()), recrrate ) ;
        if (recrrate > 0)
        {
          FuncG->getLList_()->addCohort( static_cast<int>(recrrate), 0, 0);
          AvailSeeds -= recrrate ;
        }
      }
    }
    
    /* 7. Place the seeds in the appropriate pool */
    if (FGparams->getInnateDormancy())
    {
      Dpp_ptr->PutSeedInPool( AvailSeeds );
    } else
    {
      App_ptr->PutSeedInPool( AvailSeeds );
    }
    
    /* 9. Update Fecundity */
    if (isDrought[fg])
    {
      setSeedProd(fg, 0.0);
    } else
    {
      setSeedProd(fg, static_cast<int>(max(0.0, this->calcFecund(fg))));
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
    int pcUnaff = FGresp.getFates(Dstb, range, Unaff ); /* Unaffected plant percentage */
    
    if (m_Comm.getNoCohort(fg) > 0)
    {
      /* remove cohorts if all plants are affected */
      if (pcUnaff <= 0)
      {
        FGlegion->removeCohort(ageStart, ageStop);
      } else if (pcUnaff < 100)
      { /* reduce cohorts abundances if some plants are not unaffected */
        FGlegion->reduceCohort(ageStart, ageStop, IntToDouble(pcUnaff));
      }
    }
  } // end loop over ranges
} // end of DoUnaffected()

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SuFate::DoDisturbance(int fg, int Dstb, double Dstb_val, FGresponse FGresp)
{
  if (m_Comm.getNoCohort(fg) > 0)
  {
    FuncGroupPtr FuncG = m_Comm.getFuncGroup_(fg);
    LegionPtr FGlegion = FuncG->getLList_();
    
    /* Get number of resprouting plants */
    /* Resprouting plants counter reinitialisation */
    int noRange = FGresp.getFates()[Dstb].size();
    vector< int > ResprC(noRange, 0);
    //fill(ResprC.begin(), ResprC.end(), 0);
    
    /* Get disturbance value : if < 1, reduce Kill and Respr fates accordingly */
    if (Dstb_val < 1.0)
    {
      for (int range=0; range<noRange; range++)
      {
        double newKill = IntToDouble(FGresp.getFates(Dstb, range, Kill)) * Dstb_val;
        double newRespr = IntToDouble(FGresp.getFates(Dstb, range, Respr)) * Dstb_val;
        FGresp.setFates(DoubleToInt(newKill), Dstb, range, Kill);
        FGresp.setFates(DoubleToInt(newRespr), Dstb, range, Respr);
        FGresp.setFates(getLeavingFract(DoubleToInt(newKill), DoubleToInt(newRespr)), Dstb, range, Unaff);
      }
    }
    
    for (int co=0; co<m_Comm.getNoCohort(fg); co++)
    {
      for (int range=0; range<noRange; range++)
      {
        ResprC[range] += ceil( IntToDouble( FGresp.getFates(Dstb, range, Respr) ) * m_Comm.getCSize(fg, co) *
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
    
    /* Pick up legions having same size and following ages */
    FGlegion->pickupCohorts();
    
    /* Seeds pool perturbation part */
    PropPool* App_ptr = FuncG->getPools_(ActiveP);
    PropPool* Dpp_ptr = FuncG->getPools_(DormantP);
    
    /* Kill active seeds */
    App_ptr->setSize( ceil(App_ptr->getSize() - App_ptr->getSize() * IntToDouble(FGresp.getPropKilled(Dstb)) * Dstb_val) );
    
    /* Transfer Dormant seeds to active seed pool */
    if (FuncG->getFGparams_()->getInnateDormancy())
    {
      int dormbreaks = FGresp.getDormBreaks(Dstb);
      App_ptr->setSize( fmin(App_ptr->getSize() + Dpp_ptr->getSize() * IntToDouble(dormbreaks) * Dstb_val, 100) ) ;
      if (dormbreaks == 100)
      {
        Dpp_ptr->EmptyPool();
      } else
      {
        Dpp_ptr->setSize( ceil( Dpp_ptr->getSize() * ( 1.0 - IntToDouble(dormbreaks) * Dstb_val) ) );
      }
    }
  }
} // end of DoDisturbance(...)

void SuFate::DoDisturbance(int Dstb, double Dstb_val)
{
  unsigned noFG = m_Comm.getFuncGroupList().size();
  for (unsigned fg = 0; fg < noFG; fg++)
  {
    
    if (m_Comm.getNoCohort(fg) > 0)
    {
      FuncGroupPtr FuncG = m_Comm.getFuncGroup_(fg);
      LegionPtr FGlegion = FuncG->getLList_();
      FGresponse FGresp = FuncG->getFGparams_()->getDistResponse();
      
      /* Get number of resprouting plants */
      /* Resprouting plants counter reinitialisation */
      int noRange = FGresp.getFates()[Dstb].size();
      vector< int > ResprC(noRange, 0);
      //fill(ResprC.begin(), ResprC.end(), 0);
      
      /* Get disturbance value : if < 1, reduce Kill and Respr fates accordingly */
      if (Dstb_val < 1.0)
      {
        for (int range=0; range<noRange; range++)
        {
          double newKill = IntToDouble(FGresp.getFates(Dstb, range, Kill)) * Dstb_val;
          double newRespr = IntToDouble(FGresp.getFates(Dstb, range, Respr)) * Dstb_val;
          FGresp.setFates(DoubleToInt(newKill), Dstb, range, Kill);
          FGresp.setFates(DoubleToInt(newRespr), Dstb, range, Respr);
          FGresp.setFates(getLeavingFract(DoubleToInt(newKill), DoubleToInt(newRespr)), Dstb, range, Unaff);
        }
      }
      
      for (int co=0; co<m_Comm.getNoCohort(fg); co++)
      {
        for (int range=0; range<noRange; range++)
        {
          ResprC[range] += ceil( IntToDouble( FGresp.getFates(Dstb, range, Respr) ) * m_Comm.getCSize(fg, co) *
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
      
      /* Pick up legions having same size and following ages */
      FGlegion->pickupCohorts();
      
      /* Seeds pool perturbation part */
      PropPool* App_ptr = FuncG->getPools_(ActiveP);
      PropPool* Dpp_ptr = FuncG->getPools_(DormantP);
      
      /* Kill active seeds */
      App_ptr->setSize( ceil(App_ptr->getSize() - App_ptr->getSize() * IntToDouble(FGresp.getPropKilled(Dstb)) * Dstb_val) );
      
      /* Transfer Dormant seeds to active seed pool */
      if (FuncG->getFGparams_()->getInnateDormancy())
      {
        int dormbreaks = FGresp.getDormBreaks(Dstb);
        App_ptr->setSize( fmin(App_ptr->getSize() + Dpp_ptr->getSize() * IntToDouble(dormbreaks) * Dstb_val, 100) ) ;
        if (dormbreaks == 100)
        {
          Dpp_ptr->EmptyPool();
        } else
        {
          Dpp_ptr->setSize( ceil( Dpp_ptr->getSize() * ( 1.0 - IntToDouble(dormbreaks) * Dstb_val) ) );
        }
      }
    }
  } // end loop PFG
} // end of DoDisturbance(...)
