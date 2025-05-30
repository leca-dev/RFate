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

/*============================================================================*/
/*                           Functional Group Class                           */
/*============================================================================*/

/*!
 * \file FG.h
 * \brief Plant Functional Group definition
 * \author Damien Georges, Maya Gueguen
 * \version 1.0
 * \date 2020
 */

#ifndef FG_H
#define FG_H

#include "FGresponse.h"
#include "FGUtils.h"
#include "FilesOfParamsList.h"
#include "GlobalSimulParameters.h"
#include "Logger.h"

using namespace std;


/*!
 * \class FG
 * \brief Plant Functional Group definition
 *
 * This object stores all the parameters characterizing a Plant Functional
 * Group. Depending on which modules are activated or not, more or less
 * parameters are required.
 * Basic and required parameters concern FG identification, life history and
 * propagule biology.
 * Additional parameters need to be specified if the following modules are on :
 *  - light interaction
 *  - dispersal
 *  - soil interaction
 *  - disturbance(s)
 *  - fire(s)
 *  - drought(s)
 *  - invasive (or alien) species
 */

class FG
{
private:
  
  /* FG Identification */
  string m_Name; /*!< FG name */
  
  /* Life history*/
  int m_M; /*!< Maturation time */
  int m_L; /*!< Life span */
  int m_MaxAbund; /*!< Maximum Abundance */
  int m_ImmSize; /*!< Proportion of immature plants relative to mature abundance */
  int m_MaxStratum; /*!< Maximum stratum reached */
  vector<int> m_Strata; /*!< Strata change age */
  
  /* Propagule biology */
  vector<int> m_PoolL; /*!< Seed Pool Life Span [PTcount] */
  bool m_InnateDorm; /*!< Do FG seeds have Innate dormancy properties */
  int m_PotentialFecundity; /*!< Potential Fecundity of mature plants */
  
  /* Light interaction module */
  double m_LightShadeFactor; /*!< Index of shade quantity to weight PFG abundance and transform it into shade resources */
  vector<int> m_LightActiveGerm; /*!< Proportion of Active seeds able to germinate considering light resources [Rcount] */
  vector< vector<int> > m_LightTolerance; /*!< Is FG survived considering available light resources [LScount][Rcount] */
  
  /* Dispersal module */
  bool m_IsSeeded; /*!< Is seeding applied to this FG ? */
  double m_disp50; /*!< Distance where 50% of seeds are dispersed */
  double m_disp99; /*!< Distance where 99% of seeds are dispersed */
  double m_dispLD; /*!< Long distance dispersal */
  
  /* Soil interaction module */
  double m_SoilContrib; /*!< Contribution of PFG to refill soil nutriment resources (kind of litter index) */
  double m_SoilLow; /*!< Contribution of PFG to refill soil nutriment resources (kind of litter index) */
  double m_SoilHigh; /*!< Contribution of PFG to refill soil nutriment resources (kind of litter index) */
  vector<int> m_SoilActiveGerm; /*!< Proportion of Active seeds able to germinate considering soil nutriment resources [Rcount] */
  vector< vector<int> > m_SoilTolerance; /*!< Is FG survived considering available soil nutriment resources [LScount][Rcount] */
  
  /* Disturbance response */
  FGresponse m_DistResponse; /*!< PFG response to disturbances */
  
  /* Fire response */
  FGresponse m_FireResponse; /*!< PFG response to fire disturbances */
  double m_Flamm; /*!< Flammability : how easily the FG will burn */
  
  /* Drought response */
  FGresponse m_DroughtResponse; /*!< PFG response to severe drought disturbance, with immediate or post-year effects */
  double m_DroughtThreshMod; /*!< PFG threshold to moderate drought */
  double m_DroughtThreshSev; /*!< PFG threshold to moderate drought */
  int m_CountRecovery; /*!< How many years of previous drought the PFG recovers during a year without drought */
  int m_CountSens; /*!< How many years of previous drought lead a moderate drought to a severe one */
  int m_CountCum; /*!< How many years of previous drought lead a severe drought to have mortality effects*/
  
  /* Alien introduction module */
  bool m_IsAlien; /*!< Is FG an alien plant introduced ? */
  
  
  /*-------------------------------------------*/
  /* Serialization function -------------------*/
  /*-------------------------------------------*/
  
  friend class boost::serialization::access;
  template<class Archive>
  void serialize(Archive & ar, const unsigned int /*version*/)
  {
    ar & m_Name;
    ar & m_M;
    ar & m_L;
    ar & m_MaxAbund;
    ar & m_ImmSize;
    ar & m_MaxStratum;
    ar & m_Strata;
    ar & m_PoolL;
    ar & m_InnateDorm;
    ar & m_PotentialFecundity;
    ar & m_LightShadeFactor;
    ar & m_LightActiveGerm;
    ar & m_LightTolerance;
    ar & m_IsSeeded;
    ar & m_disp50;
    ar & m_disp99;
    ar & m_dispLD;
    ar & m_SoilContrib;
    ar & m_SoilLow;
    ar & m_SoilHigh;
    ar & m_SoilActiveGerm;
    ar & m_SoilTolerance;
    ar & m_SoilTolerance;
    ar & m_DistResponse;
    ar & m_FireResponse;
    ar & m_Flamm;
    ar & m_DroughtResponse;
    ar & m_DroughtThreshMod;
    ar & m_DroughtThreshSev;
    ar & m_CountRecovery;
    ar & m_CountSens;
    ar & m_CountCum;
    ar & m_IsAlien;
  }
  
public:
  
  /*-------------------------------------------*/
  /* Constructors -----------------------------*/
  /*-------------------------------------------*/
  
  /*!
   *	\brief Default constructor
   *
   *	FG default constructor => All parameters are set to 0, False or None
   */
  FG();
  
  /*!
   *	\brief Full constructor
   *
   *	FG full constructor
   *
   *	\param PFG_LifeHistoryFile : path to text file containing well-formatted
   * life history related parameters
   *	\param PFG_LightFile : path to text file containing well-formatted light
   * interaction related parameters
   *	\param PFG_DispersalFile : path to text file containing well-formatted
   * dispersal related parameters
   *	\param PFG_DisturbancesFile : path to text file containing well-formatted
   * disturbances related parameters
   *	\param PFG_SoilFile : path to text file containing well-formatted soil
   * interaction related parameters
   *	\param PFG_FireFile : path to text file containing well-formatted fire
   * related parameters
   *	\param PFG_DroughtFile : path to text file containing well-formatted
   * drought related parameters
   * param glob_params : GSP class object containing global simulation
   * related parameters, and modules specific (e.g number of strata, number of
   * disturbances...)
   */
  FG(const string& PFG_LifeHistoryFile,
     const string& PFG_LightFile,
     const string& PFG_DispersalFile,
     const string& PFG_DisturbancesFile,
     const string& PFG_SoilFile,
     const string& PFG_FireFile,
     const string& PFG_DroughtFile,
     const GSP& glob_params );
  
  /*!
   *	\brief Full constructor
   *
   *	FG full constructor
   *
   * param glob_params : GSP class object containing global simulation
   * related parameters, and modules specific (e.g number of strata, number of
   * disturbances...)
   * param file_of_params : FOPL class object containing paths to parameters
   * text file (--PFG_PARAMS_LIFE_HISTORY--, --PFG_PARAMS_DISPERSAL--, ...)
   * param fg_id : functional group ID to create community later
   */
  FG(const GSP& glob_params, const FOPL& file_of_params, const unsigned& fg_id);
  
  /*!
   *	\brief Part of constructor (BASE : demographic model)
   *
   *	FG part of constructor (BASE : demographic model)
   *
   * param glob_params : GSP class object containing global simulation
   * related parameters, and modules specific (e.g number of strata, number of
   * disturbances...)
   *	\param PFG_LifeHistoryFile : path to text file containing well-formatted
   * life history related parameters
   */
  void getSuccParams(const GSP& glob_params, const string& PFG_LifeHistoryFile);
  
  /*!
   *	\brief Part of constructor (OPTIONAL : light interaction model)
   *
   *	FG part of constructor (OPTIONAL : light interaction model)
   *
   *	\param PFG_LightFile : path to text file containing well-formatted light
   * interaction related parameters
   */
  void getLightParams(const string& PFG_LightFile);
  
  /*!
   *	\brief Part of constructor (OPTIONAL : dispersal model)
   *
   *	FG part of constructor (OPTIONAL : dispersal model)
   *
   *	\param PFG_DispersalFile : path to text file containing well-formatted
   * dispersal related parameters
   */
  void getDispParams(const string& PFG_DispersalFile);
  
  /*!
   *	\brief Part of constructor (OPTIONAL : disturbances model)
   *
   *	FG part of constructor (OPTIONAL : disturbances model)
   *
   * param glob_params : GSP class object containing global simulation
   * related parameters, and modules specific (e.g number of strata, number of
   * disturbances...)
   *	\param PFG_DisturbancesFile : path to text file containing well-formatted
   * disturbances related parameters
   */
  void getDistParams(const GSP& glob_params, const string& PFG_DisturbancesFile);
  
  /*!
   *	\brief Part of constructor (OPTIONAL : soil interaction model)
   *
   *	FG part of constructor (OPTIONAL : soil interaction model)
   *
   *	\param PFG_SoilFile : path to text file containing well-formatted soil
   * interaction related parameters
   */
  void getSoilParams(const string& PFG_SoilFile);
  
  /*!
   *	\brief Part of constructor (OPTIONAL : fire model)
   *
   *	FG part of constructor (OPTIONAL : fire model)
   *
   * param glob_params : GSP class object containing global simulation
   * related parameters, and modules specific (e.g number of strata, number of
   * disturbances...)
   *	\param PFG_FireFile : path to text file containing well-formatted fire
   * related parameters
   */
  void getFireParams(const GSP& glob_params, const string& PFG_FireFile);
  
  /*!
   *	\brief Part of constructor (OPTIONAL : drought model)
   *
   *	FG part of constructor (OPTIONAL : drought model)
   *
   * param glob_params : GSP class object containing global simulation
   * related parameters, and modules specific (e.g number of strata, number of
   * disturbances...)
   *	\param PFG_DroughtFile : path to text file containing well-formatted
   * drought related parameters
   */
  void getDrouParams(const GSP& glob_params, const string& PFG_DroughtFile);
  
  /*-------------------------------------------*/
  /* Destructor -------------------------------*/
  /*-------------------------------------------*/
  
  /*!
   *	\brief Destructor
   *
   *	FG destructor
   */
  virtual ~FG();
  
  /*-------------------------------------------*/
  /* Operators --------------------------------*/
  /*-------------------------------------------*/
  
  bool operator==(const FG& o) const
  {
    return (m_Name == o.m_Name &&
            m_M == o.m_M &&
            m_L == o.m_L &&
            m_MaxAbund == o.m_MaxAbund &&
            m_ImmSize == o.m_ImmSize &&
            m_MaxStratum == o.m_MaxStratum &&
            m_Strata == o.m_Strata &&
            m_PoolL == o.m_PoolL &&
            m_InnateDorm == o.m_InnateDorm &&
            m_PotentialFecundity == o.m_PotentialFecundity &&
            m_LightShadeFactor == o.m_LightShadeFactor &&
            m_LightActiveGerm == o.m_LightActiveGerm &&
            m_LightTolerance == o.m_LightTolerance &&
            m_IsSeeded == o.m_IsSeeded &&
            m_disp50 == o.m_disp50 &&
            m_disp99 == o.m_disp99 &&
            m_dispLD == o.m_dispLD &&
            m_SoilContrib == o.m_SoilContrib &&
            m_SoilLow == o.m_SoilLow &&
            m_SoilHigh == o.m_SoilHigh &&
            m_SoilActiveGerm == o.m_SoilActiveGerm &&
            m_SoilTolerance == o.m_SoilTolerance &&
            m_DistResponse == o.m_DistResponse &&
            m_FireResponse == o.m_FireResponse &&
            m_Flamm == o.m_Flamm &&
            m_DroughtResponse == o.m_DroughtResponse &&
            m_DroughtThreshMod == o.m_DroughtThreshMod &&
            m_DroughtThreshSev == o.m_DroughtThreshSev &&
            m_CountRecovery == o.m_CountRecovery &&
            m_CountSens == o.m_CountSens &&
            m_CountCum == o.m_CountCum &&
            m_IsAlien == o.m_IsAlien);
    
    /* check fixed size tables equality */
    /*		for (unsigned i=0; i<PTcount; i++)
     {
     is_equal = ( is_equal && m_PoolL[i] == o.m_PoolL[i] );
     }
     for (unsigned i=0; i<Rcount; i++)
     {
     is_equal = ( is_equal && m_LightActiveGerm[i] == o.m_LightActiveGerm[i] );
     }
     for (unsigned i=0; i<LScount; i++)
     {
     for (unsigned j=0; j<Rcount; j++)
     {
     is_equal = ( is_equal && m_LightTolerance[i][j] == o.m_LightTolerance[i][j] );
     }
     }*/
  }
  
  /*-------------------------------------------*/
  /* Getters & Setters ------------------------*/
  /*-------------------------------------------*/
  
  const string& getName() const;
  int getMatTime() const;
  int getLifeSpan() const;
  int getMaxAbund() const;
  int getImmSize() const;
  int getMaxStratum() const;
  const vector<int> getStrata() const;
  int getStrata(const int& i) const;
  const vector<int> getPoolLife() const;
  int getPoolLife(const PoolType& pt ) const;
  bool getInnateDormancy() const;
  int getPotentialFecund() const;
  double getLightShadeFactor() const;
  const vector<int> getLightActiveGerm() const;
  const int& getLightActiveGerm(const Resource& r) const;
  const vector< vector<int> >& getLightTolerance() const;
  int getLightTolerance(LifeStage ls, Resource r) const;
  bool getIsSeeded() const;
  double getDisp50() const;
  double getDisp99() const;
  double getDispLD() const;
  double getSoilContrib() const;
  double getSoilLow() const;
  double getSoilHigh() const;
  const vector<int> getSoilActiveGerm() const;
  const int& getSoilActiveGerm(const Resource& r) const;
  const vector< vector<int> >& getSoilTolerance() const;
  int getSoilTolerance(LifeStage ls,  Resource r) const;
  FGresponse getDistResponse();
  const FGresponse& getFireResponse() const;
  double getFlamm() const;
  const FGresponse& getDroughtResponse() const;
  double getDroughtThreshMod() const;
  double getDroughtThreshSev() const;
  int getCountRecovery() const;
  int getCountSens() const;
  int getCountCum() const;
  bool getIsAlien() const;
  
  
  void setName(const string& name);
  void setMatTime(const int& matTime);
  void setLifeSpan(const int& lifeSpan);
  void setMaxAbund(const int& maxAbund);
  void setImmSize(const int& immSize);
  void setMaxStratum(const int& maxStratum);
  void setStrata(const vector<int>& strata);
  void setStrata(const int& strata, const int& i);
  void setPoolLife(const int (&poolLife)[ PTcount ]);
  void setPoolLife(const int& poolLife, const PoolType& pt);
  void setInnateDormancy(const bool& innateDormancy);
  void setPotentialFecund(const int& potentialFecund);
  void setLightShadeFactor(const double& lightShadeFactor);
  void setLightActiveGerm(const int (&activeGerm)[ Rcount ]);
  void setLightActiveGerm(const int& activeGerm, const Resource& r );
  void setTolerance(const int (&tolerance)[ LScount ][ Rcount ]);
  void setTolerance(const int& tolerance, const LifeStage& ls, const Resource& r);
  void setIsSeeded(const bool& isSeeded);
  void setDisp50(const double& disp50);
  void setDisp99(const double& disp99);
  void setDispLD(const double& dispLD);
  void setSoilContrib(const double& soilContrib);
  void setSoilLow(const double& soilLow);
  void setSoilHigh(const double& soilHigh);
  void setSoilActiveGerm(const int (&activeGerm)[ Rcount ]);
  void setSoilActiveGerm(const int& activeGerm, const Resource& r );
  void setSoilTolerance(const vector< vector<int> >& tolerance);
  void setSoilTolerance(const int& tolerance, const LifeStage& ls, const Resource& r);
  void setDistResponse(const FGresponse& distResponse);
  void setFireResponse(const FGresponse& fireResponse);
  void setFlamm(const double& flamm);
  void setDroughtResponse(const FGresponse& droughtResponse);
  void setDroughtThreshMod(const double& droughtThreshMod);
  void setDroughtThreshSev(const double& droughtThreshSev);
  void setCountRecovery(const int& countRecovery);
  void setCountSens(const int& countSens);
  void setCountCum(const int& countCum);
  
  void setIsAlien(const bool& isAlien);
  
  /*-------------------------------------------*/
  /* Others functions -------------------------*/
  /*-------------------------------------------*/
  
  void show();
  
};

BOOST_CLASS_VERSION(FG, 0)
#endif // FG_H