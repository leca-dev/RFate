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

#include "SimulMap.h"

#include <iostream>
#include <cstring>
#include <fstream>
#include <cstdio>
#include <numeric>
#include <chrono>
#include <random>
#include <string>
#include <filesystem>

#include "gdal_priv.h" // to read raster files
#include "gdal.h"
#include "cpl_conv.h"

#include <boost/iostreams/filtering_stream.hpp>    
#include <boost/iostreams/filtering_streambuf.hpp>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/filter/gzip.hpp>

namespace bo = boost::iostreams;
namespace fs = std::filesystem;
using namespace std;

// boost::mt19937 est un Mersenne twister generator, ou générateur de nombres pseudo-aléatoires

// typedef std::mt19937 RandomGenerator;
typedef std::default_random_engine RandomGenerator;
typedef std::uniform_real_distribution<double> UniReal;
typedef std::uniform_int_distribution<int> UniInt;
typedef std::normal_distribution<double> Normal;

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructor                                                                                     */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

SimulMap::SimulMap() : m_glob_params(GSP()),
m_FGparams(0,FG()),
m_Coord(Coordinates<double>()),
m_Mask(SpatialMap<double, int>()),
m_MaskCells(1,0),
m_SeedMapIn(SpatialStack<double, int>()),
m_SeedMapOut(SpatialStack<double, int>()),
m_EnvSuitMap(SpatialStack<double, double>()),
m_EnvSuitRefMap(SpatialStack<double, double>()),
m_DistMap(SpatialStack<double, double>()),
m_FireMap(SpatialStack<double, int>()),
m_TslfMap(SpatialMap<double, int>()),
m_DroughtMap(SpatialMap<double, double>()),
m_ElevationMap(SpatialMap<double, double>()),
m_SlopeMap(SpatialMap<double, double>()),
m_PostDroughtMap(SpatialStack<double, unsigned>()),
m_CountDroughtMap(SpatialStack<double, unsigned>()),
m_IsDroughtMap(SpatialStack<double, unsigned>()),
m_ApplyCurrDroughtMap(SpatialStack<double, unsigned>()),
m_ApplyPostDroughtMap(SpatialStack<double, unsigned>()),
m_CondInitMap(SpatialStack<double, double>()),
m_SuccModelMap(SpatialMap<double, SuFatePtr>()),
m_DispModel(Disp())
{
  /* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

SimulMap::SimulMap(FOPL file_of_params)
{
  logg.info("*** building Simulation Map...\n",
            "*** building Global simulation parameters...");
  
  /* read global parameters file */
  m_glob_params = GSP(file_of_params.getGlobSimulParams());
  m_glob_params.show();
  GSPPtr m_glob_params_ptr = &m_glob_params;
  int noFG = m_glob_params.getNoFG(); // number of functional groups
  
  m_RNG.seed(m_glob_params.getSeed());
  
  /* build functional groups entities */
  logg.info("*** building Functional groups...");
  if (noFG != static_cast<int>(file_of_params.getFGLifeHistory().size()))
  {
    logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_LIFE_HISTORY-- ",
               "do not match in term of number!");
  }
  if (m_glob_params.getDoLightInteraction() &&
      noFG != static_cast<int>(file_of_params.getFGLight().size()))
  {
    logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_LIGHT-- ",
               "do not match in term of number!");
  }
  if (m_glob_params.getDoHabSuitability() &&
      noFG != static_cast<int>(file_of_params.getFGMapsHabSuit().size()))
  {
    logg.error("!!! Parameters NO_PFG and --PFG_MASK_HABSUIT-- ",
               "do not match in term of number!");
  }
  if (m_glob_params.getDoDispersal() &&
      noFG != static_cast<int>(file_of_params.getFGDispersal().size()))
  {
    logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_DISPERSAL-- ",
               "do not match in term of number!");
  }
  if (m_glob_params.getDoDisturbances() &&
      noFG != static_cast<int>(file_of_params.getFGDisturbance().size()))
  {
    logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_DISTURBANCES-- ",
               "do not match in term of number!");
  }
  if (m_glob_params.getDoSoilInteraction() &&
      noFG != static_cast<int>(file_of_params.getFGSoil().size()))
  {
    logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_SOIL-- ",
               "do not match in term of number!");
  }
  if (m_glob_params.getDoFireDisturbances() &&
      noFG != static_cast<int>(file_of_params.getFGFire().size()))
  {
    logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_FIRE-- ",
               "do not match in term of number!");
  }
  if (m_glob_params.getDoDroughtDisturbances() &&
      noFG != static_cast<int>(file_of_params.getFGDrought().size()))
  {
    logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_DROUGHT-- ",
               "do not match in term of number!");
  }
  
  /* check for parameters file */
  file_of_params.checkCorrectParams(m_glob_params.getDoLightInteraction(),
                                    m_glob_params.getDoHabSuitability(),
                                    m_glob_params.getDoDispersal(),
                                    m_glob_params.getDoDisturbances(),
                                    m_glob_params.getDoSoilInteraction(),
                                    m_glob_params.getDoFireDisturbances(),
                                    m_glob_params.getDoDroughtDisturbances(),
                                    m_glob_params.getDoAliensIntroduction());
  
  /* build Plant Functional Groups */
  m_FGparams.reserve(noFG);
  for (int fg_id=0; fg_id<noFG; fg_id++)
  {
    m_FGparams.emplace_back(m_glob_params, file_of_params, fg_id);
  }
  
  /* build study area coordinates */
  logg.info("> build study area coordinates...");
  m_Coord = Coordinates<double>( ReadCoordinates(file_of_params.getMask()) );
  Coordinates<double>* m_Coord_ptr = &m_Coord; // ptr on study area coordinates
  
  /* build simulation mask (study area) */
  logg.info("> build simulation mask (study area)...");
  m_Mask = SpatialMap<double, int>(m_Coord_ptr, ReadMask<int>( file_of_params.getMask(), 0.0, 1.0, true ) );
  m_MaskCells.reserve(m_Mask.getTotncell());
  for (int cell_ID=0; cell_ID<m_Mask.getTotncell(); cell_ID++)
  {
    if (m_Mask(cell_ID) == 1)
    {
      m_MaskCells.emplace_back(cell_ID);
    }
  }
  m_MaskCells.shrink_to_fit();
  
  /* DECLARE EMPTY SPATIAL STACK*/
  vector< vector< double > > emptyMapDouble;
  vector< vector< int > > emptyMapInt;
  emptyMapInt.reserve(noFG);
  vector< vector< unsigned > > emptyMapUns;
  vector< double >  emptyValDouble( m_Mask.getTotncell(), 0.0 );
  vector< int >  emptyValInt( m_Mask.getTotncell(), 0 );
  vector< unsigned >  emptyValUns( m_Mask.getTotncell(), 0 );
  for (int fg_id=0; fg_id<noFG; fg_id++)
  {
    emptyMapInt.emplace_back( emptyValInt );
    if (fg_id==0)
    {
      emptyMapDouble.push_back( emptyValDouble );
      emptyMapUns.push_back( emptyValUns );
    }
  }
  
  /* declare empty seeds map */
  logg.info("> declare empty seeds map...");
  m_SeedMapIn = SpatialStack<double, int>(m_Coord_ptr, emptyMapInt);
  m_SeedMapOut = SpatialStack<double, int>(m_Coord_ptr, emptyMapInt);
  emptyMapInt.resize(1);
  IntMapPtr m_SeedMapIn_ptr = &m_SeedMapIn; //  ptr on maps of produced seeds
  IntMapPtr m_SeedMapOut_ptr = &m_SeedMapOut; // ptr on maps of dispersed seeds (seed rain)
  
  /* dispersal model creation */
  if (m_glob_params.getDoDispersal())
  {
    logg.info("> dispersal model creation...");
    m_DispModel = Disp( &m_FGparams, &m_SeedMapIn, &m_SeedMapOut );
  } else
  {
    m_DispModel = Disp(&m_FGparams, &m_SeedMapIn, &m_SeedMapOut, false);
  }
  
  /* build environmental condition maps */
  logg.info("> build environmental condition maps...");
  if (m_glob_params.getDoHabSuitability())
  {
    vector< vector< double > > envSuitMap; // environmental suitability of fgs
    envSuitMap.reserve(noFG);
    for (int fg_id=0; fg_id<noFG; fg_id++)
    {
      envSuitMap.emplace_back( ReadMask<double>( file_of_params.getFGMapsHabSuit()[fg_id], 0.0, 1.0 ) );
    }
    m_EnvSuitMap = SpatialStack<double, double>(m_Coord_ptr, envSuitMap);
    
    /* define the coming year environmental reference */
    logg.info("> define the coming year environmental reference...");
    vector< double >  envSuitRefVal( m_Mask.getTotncell(), 0.5 );
    vector< vector< double > > envSuitRefMap; // environmental suitability year reference of fgs
    envSuitRefMap.reserve(noFG);
    for (int fg_id=0; fg_id<noFG; fg_id++)
    {
      envSuitRefMap.emplace_back( envSuitRefVal );
    }
    m_EnvSuitRefMap = SpatialStack<double, double>(m_Coord_ptr, envSuitRefMap);
  } else
  {
    m_EnvSuitMap = SpatialStack<double, double>(m_Coord_ptr, emptyMapDouble);
    m_EnvSuitRefMap = SpatialStack<double, double>(m_Coord_ptr, emptyMapDouble);
  }
  
  /* build simulation disturbances masks */
  if (m_glob_params.getDoDisturbances())
  {
    logg.info("> build simulation disturbances masks...");
    if (m_glob_params.getNoDist() == static_cast<int>(file_of_params.getMaskDist().size()))
    {
      vector< vector< double > > distMap; // disturbances masks
      distMap.reserve(noFG);
      for (int dist_id=0; dist_id<m_glob_params.getNoDist(); dist_id++)
      {
        distMap.emplace_back( ReadMask<double>( file_of_params.getMaskDist()[dist_id], 0.0, 1.0 ) );
      }
      m_DistMap = SpatialStack<double, double>(m_Coord_ptr, distMap);
    } else
    {
      logg.error("!!! Parameters DIST_NO and --DIST_MASK-- ",
                 "do not match in term of number!");
    }
  } else
  {
    m_DistMap = SpatialStack<double, double>(m_Coord_ptr, emptyMapDouble);
  }
  
  /* build simulation fire disturbances masks */
  if (m_glob_params.getDoFireDisturbances())
  {
    if (m_glob_params.getFireIgnitMode()==5)
    {
      logg.info("> build simulation fire disturbances masks...");
      if (m_glob_params.getNoFireDist() == static_cast<int>(file_of_params.getMaskFire().size()))
      {
        vector< vector< int > > fireMap; // fire disturbances masks
        fireMap.reserve(m_glob_params.getNoFireDist());
        for (int dist_id=0; dist_id<m_glob_params.getNoFireDist(); dist_id++)
        {
          fireMap.emplace_back( ReadMask<int>( file_of_params.getMaskFire()[dist_id], 0.0, 1.0 ) );
        }
        m_FireMap = SpatialStack<double, int>(m_Coord_ptr, fireMap);
      } else
      {
        logg.error("!!! Parameters FIRE_NO and --FIRE_MASK-- ",
                   "do not match in term of number!");
      }
    } else
    {
      m_FireMap = SpatialStack<double, int>(m_Coord_ptr, emptyMapInt);
    }
    
    /* build fire masks */
    logg.info("> build fire masks...");
    if (m_glob_params.getFirePropMode()==4)
    {
      m_DroughtMap = SpatialMap<double, double>(m_Coord_ptr, ReadMask<double>( file_of_params.getMaskDrought(), 0.0, 1.0 ) );
      m_ElevationMap = SpatialMap<double, double>(m_Coord_ptr, ReadMask<double>( file_of_params.getMaskElevation()) );
      m_SlopeMap = SpatialMap<double, double>(m_Coord_ptr, ReadMask<double>( file_of_params.getMaskSlope()) );
    } else
    {
      m_DroughtMap = SpatialMap<double, double>(m_Coord_ptr, emptyValDouble);
      m_ElevationMap = SpatialMap<double, double>(m_Coord_ptr, emptyValDouble);
      m_SlopeMap = SpatialMap<double, double>(m_Coord_ptr, emptyValDouble);
    }
    
    /* build TSLF mask (study area) */
    logg.info("> build TSLF mask (study area)...");
    m_TslfMap = SpatialMap<double, int>(m_Coord_ptr, ReadMask<int>( file_of_params.getMask(), 0.0, 1.0 ) );
  } else
  {
    m_FireMap = SpatialStack<double, int>(m_Coord_ptr, emptyMapInt);
    m_TslfMap = SpatialMap<double, int>(m_Coord_ptr, emptyValInt);
    m_DroughtMap = SpatialMap<double, double>(m_Coord_ptr, emptyValDouble);
    m_ElevationMap = SpatialMap<double, double>(m_Coord_ptr, emptyValDouble);
    m_SlopeMap = SpatialMap<double, double>(m_Coord_ptr, emptyValDouble);
  }
  
  /* build drought index mask */
  if (m_glob_params.getDoDroughtDisturbances())
  {
    logg.info("> build drought index mask...");
    m_DroughtMap = SpatialMap<double, double>(m_Coord_ptr, ReadMask<double>( file_of_params.getMaskDrought(), -5000.0, 1000.0 ) );
    vector< vector< unsigned > > droughtMap; // drought disturbances masks
    droughtMap.reserve(noFG);
    for (int fg_id=0; fg_id<noFG; fg_id++)
    {
      droughtMap.emplace_back( ReadMask<unsigned>( file_of_params.getMask(), 0.0, 1.0 ) );
    }
    m_PostDroughtMap = SpatialStack<double, unsigned>(m_Coord_ptr, droughtMap);
    m_CountDroughtMap = SpatialStack<double, unsigned>(m_Coord_ptr, droughtMap);
    m_IsDroughtMap = SpatialStack<double, unsigned>(m_Coord_ptr, droughtMap);
    m_ApplyCurrDroughtMap = SpatialStack<double, unsigned>(m_Coord_ptr, droughtMap);
    m_ApplyPostDroughtMap = SpatialStack<double, unsigned>(m_Coord_ptr, droughtMap);
    
    for (vector<int>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
    { // loop on pixels
      for (int fg=0; fg<noFG; fg++)
      { // loop on PFG
        m_PostDroughtMap(*cell_ID, fg) = 0;
        m_CountDroughtMap(*cell_ID, fg) = 0;
      }
    }
  } else
  {
    m_DroughtMap = SpatialMap<double, double>(m_Coord_ptr, emptyValDouble );
    m_PostDroughtMap = SpatialStack<double, unsigned>(m_Coord_ptr, emptyMapUns);
    m_CountDroughtMap = SpatialStack<double, unsigned>(m_Coord_ptr, emptyMapUns);
    m_IsDroughtMap = SpatialStack<double, unsigned>(m_Coord_ptr, emptyMapUns);
    m_ApplyCurrDroughtMap = SpatialStack<double, unsigned>(m_Coord_ptr, emptyMapUns);
    m_ApplyPostDroughtMap = SpatialStack<double, unsigned>(m_Coord_ptr, emptyMapUns);
  }
  
  /* build aliens introduction masks */
  if (m_glob_params.getDoAliensIntroduction())
  {
    logg.info("> build aliens introduction masks...");
    if (noFG == static_cast<int>(file_of_params.getFGMapsAliens().size()))
    {
      vector< vector< double > > condInitMap; // aliens introduction masks
      condInitMap.reserve(noFG);
      for (int fg_id=0; fg_id<noFG; fg_id++)
      {
        condInitMap.emplace_back( ReadMask<double>( file_of_params.getFGMapsAliens()[fg_id], 0.0, 1.0 ) );
      }
      m_CondInitMap = SpatialStack<double, double>(m_Coord_ptr, condInitMap);
    } else
    {
      logg.error("!!! Parameters NO_PFG and --ALIENS_MASK-- ",
                 "do not match in term of number!");
    }
  } else
  {
    m_CondInitMap = SpatialStack<double, double>(m_Coord_ptr, emptyMapDouble);
  }
  
  /* standard community creation */
  logg.info("> standard community creation...");
  vector< FuncGroup > comm_std;
  comm_std.reserve(noFG);
  for (int fg_id=0; fg_id<noFG; fg_id++)
  {
    comm_std.emplace_back(FuncGroup( &(m_FGparams[fg_id]) ));
  }
  Community Comm_std(comm_std);
  
  /* standard light resource creation */
  logg.info("> standard light resource creation...");
  LightResources lr_std(m_glob_params.getNoStrata());
  
  /* standard soil resource creation */
  logg.info("> standard soil resource creation...");
  SpatialMap<double, double> SoilMap;
  if (m_glob_params.getDoSoilInteraction())
  {
    if (m_glob_params.getSoilFillMap())
    {
      vector< double >  valSoil( m_Mask.getTotncell(), m_glob_params.getSoilInit() );
      SoilMap = SpatialMap<double, double>(m_Coord_ptr, valSoil);
    } else
    {
      testFileExist("--SOIL_MASK--",file_of_params.getMaskSoil(), false);
      SoilMap = SpatialMap<double, double>(m_Coord_ptr, ReadMask<double>( file_of_params.getMaskSoil()) );
    }
  } else
  {
    SoilMap = SpatialMap<double, double>(m_Coord_ptr, emptyValDouble);
  }
  
  /* create a succession model within each pixel */
  logg.info("> create a succession model within each pixel...");
  vector< SuFatePtr > succModel_ptr_list; // vector of ptr on a succession model
  succModel_ptr_list.reserve(m_Mask.getTotncell());
  for (int i=0; i<m_Mask.getTotncell(); i++)
  {
    SuFatePtr succModel_ptr; // ptr on succession model
    if (m_glob_params.getDoHabSuitability() == false)
    { // FATE succession model
      succModel_ptr = new SuFate(i, Comm_std, lr_std, SoilMap(i)
                                   , m_SeedMapOut_ptr, m_SeedMapIn_ptr, m_glob_params_ptr);
    } else if (m_glob_params.getDoHabSuitability() == true)
    { // FATEH succession model
      succModel_ptr = new SuFateH(i, Comm_std, lr_std, SoilMap(i)
                                    , m_SeedMapOut_ptr, m_SeedMapIn_ptr, m_glob_params_ptr
                                    , &m_EnvSuitMap, &m_EnvSuitRefMap );
    }
    succModel_ptr_list.emplace_back(succModel_ptr);
  }
  
  /* build the succession models map */
  logg.info("> build the succession models map...");
  m_SuccModelMap = SpatialMap<double, SuFatePtr>(m_Coord_ptr, succModel_ptr_list);
  
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

SimulMap::~SimulMap()
{
  /* delete all Succession Models */
  for (int i=0; i<m_Mask.getTotncell(); i++)
  {
    delete m_SuccModelMap(i);
  }
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

GSP& SimulMap::getGlobalParameters() { return m_glob_params; }
vector<FG>&  SimulMap::getFGparams() { return m_FGparams; }
Coordinates<double>& SimulMap::getCoord() { return m_Coord; }
SpatialMap<double, int>& SimulMap::getMask() { return m_Mask; }
vector<int>& SimulMap::getMaskCells() { return m_MaskCells; }
SpatialStack<double, int>& SimulMap::getSeedMapIn() { return m_SeedMapIn; }
SpatialStack<double, int>& SimulMap::getSeedMapOut() { return m_SeedMapOut; }
SpatialStack<double, double>&  SimulMap::getEnvSuitMap() { return m_EnvSuitMap ; }
SpatialStack<double, double>&  SimulMap::getEnvSuitRefMap() { return m_EnvSuitRefMap; }
SpatialStack<double, double>&  SimulMap::getDistMap() { return m_DistMap; }
SpatialStack<double, int>&  SimulMap::getFireMap() { return m_FireMap; }
SpatialMap<double, int>& SimulMap::getTslfMap() { return m_TslfMap; }
SpatialMap<double, double>& SimulMap::getElevationMap() { return m_ElevationMap; }
SpatialMap<double, double>& SimulMap::getSlopeMap() { return m_SlopeMap; }
SpatialMap<double, double>& SimulMap::getDroughtMap() { return m_DroughtMap; }
SpatialStack<double, unsigned>& SimulMap::getPostDroughtMap() { return m_PostDroughtMap; }
SpatialStack<double, unsigned>& SimulMap::getCountDroughtMap() { return m_CountDroughtMap; }
SpatialStack<double, unsigned>& SimulMap::getIsDroughtMap() { return m_IsDroughtMap; }
SpatialStack<double, unsigned>& SimulMap::getApplyCurrDroughtMap() { return m_ApplyCurrDroughtMap; }
SpatialStack<double, unsigned>& SimulMap::getApplyPostDroughtMap() { return m_ApplyPostDroughtMap; }
SpatialStack<double, double>& SimulMap::getCondInitMap() { return m_CondInitMap; }
SpatialMap<double, SuFatePtr>& SimulMap::getSuccModelMap() { return m_SuccModelMap; }
Disp& SimulMap::getDispModel() { return m_DispModel; }

void SimulMap::setGlobalParameters(GSP globalParameters) { m_glob_params = globalParameters; }
void SimulMap::setFGparams(vector<FG> FGparams) { m_FGparams = FGparams; }
void SimulMap::setCoord(Coordinates<double> coord) { m_Coord = coord; }
void SimulMap::setMask(SpatialMap<double, int> mask) { m_Mask = mask; }
void SimulMap::setMaskCells(vector<int> maskCells) { m_MaskCells = maskCells; }
void SimulMap::setSeedMapIn(SpatialStack<double, int> seedMapIn) { m_SeedMapIn = seedMapIn; }
void SimulMap::setSeedMapOut(SpatialStack<double, int> seedMapOut) { m_SeedMapOut = seedMapOut; }
void SimulMap::setEnvSuitMap(SpatialStack<double, double> envSuitMap) { m_EnvSuitMap = envSuitMap; }
void SimulMap::setEnvSuitRefMap(SpatialStack<double, double> envSuitRefMap) { m_EnvSuitRefMap = envSuitRefMap; }
void SimulMap::setDistMap(SpatialStack<double, double> distMap) { m_DistMap = distMap; }
void SimulMap::setFireMap(SpatialStack<double, int> fireMap) { m_FireMap = fireMap; }
void SimulMap::setTslfMap(SpatialMap<double, int> tslfMap) { m_TslfMap = tslfMap; }
void SimulMap::setElevationMap(SpatialMap<double, double> elevationMap) { m_ElevationMap = elevationMap; }
void SimulMap::setSlopeMap(SpatialMap<double, double> slopeMap) { m_SlopeMap = slopeMap; }
void SimulMap::setDroughtMap(SpatialMap<double, double> droughtMap) { m_DroughtMap = droughtMap; }
void SimulMap::setPostDroughtMap(SpatialStack<double, unsigned> postDroughtMap) { m_PostDroughtMap = postDroughtMap; }
void SimulMap::setCountDroughtMap(SpatialStack<double, unsigned> countDroughtMap) { m_CountDroughtMap = countDroughtMap; }
void SimulMap::setIsDroughtMap(SpatialStack<double, unsigned> isDroughtMap) { m_IsDroughtMap = isDroughtMap; }
void SimulMap::setApplyCurrDroughtMap(SpatialStack<double, unsigned> applyCurrDroughtMap) { m_ApplyCurrDroughtMap = applyCurrDroughtMap; }
void SimulMap::setApplyPostDroughtMap(SpatialStack<double, unsigned> applyPostDroughtMap) { m_ApplyPostDroughtMap = applyPostDroughtMap; }
void SimulMap::setCondInitMap(SpatialStack<double, double> condInitMap) { m_CondInitMap = condInitMap; }
void SimulMap::setSuccModelMap(SpatialMap<double, SuFatePtr> succModelMap) { m_SuccModelMap = succModelMap; }
void SimulMap::setDispModel(Disp dispModel) { m_DispModel = dispModel; }

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

// TODO (damien#1#): consider case where some plants always have wide dispersal caracteristics

void SimulMap::StartSeeding()
{
  for (unsigned fg=0; fg<m_FGparams.size(); fg++)
  {
    m_FGparams[fg].setIsSeeded(true);
  }
} // end of StartSeeding()

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::StopSeeding()
{
  for (unsigned fg=0; fg<m_FGparams.size(); fg++)
  {
    m_FGparams[fg].setIsSeeded(false);
  }
} // end of StopSeeding()

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::DoFileChange(string newChangeFile, string typeFile)
{
  logg.info("Try to get change parameters :\nFrom file ", newChangeFile, " (", typeFile, ")");
  
  /* open newChangeFile */
  ifstream file(newChangeFile.c_str(), ios::in);
  if (file)
  {
    /* Read file line by line */
    string strTmp; // tmp string to keep change filenames
    vector< string > newNameFiles; // vector of change filenames
    while (file >> strTmp)
    {
      if (strTmp != "")
      {
        /* store new files */
        newNameFiles.push_back(strTmp);
        logg.info("*** ", strTmp);
      }
    }
    /* Close file */
    file.close();
    
    /* Check if the number of given files is compatible with number of files required */
    unsigned noFiles = 0;
    if (strcmp(typeFile.c_str(),"habSuit")==0){ noFiles = m_FGparams.size();
    } else if (strcmp(typeFile.c_str(),"mask")==0){ noFiles = 1;
    } else if (strcmp(typeFile.c_str(),"dist")==0){ noFiles = m_glob_params.getNoDist();
    } else if (strcmp(typeFile.c_str(),"fire")==0){ noFiles = m_glob_params.getNoFireDist();
    } else if (strcmp(typeFile.c_str(),"drought")==0){ noFiles = 1;
    } else if (strcmp(typeFile.c_str(),"aliens")==0){ noFiles = m_FGparams.size();
    }
    
    if (newNameFiles.size()!=noFiles)
    {
      logg.error("Number of new files is incompatible with the number of files required (",
                 newNameFiles.size(), " vs ", noFiles, ").");
    }
    
    /* Updating Maps */
    if ((strcmp(typeFile.c_str(),"habSuit")==0) | (strcmp(typeFile.c_str(),"dist")==0) | 
    (strcmp(typeFile.c_str(),"drought")==0) | (strcmp(typeFile.c_str(),"aliens")==0))
    {
      vector< vector<double> > newMaps; // DOUBLE VALUES
      newMaps.reserve(noFiles);
      if (strcmp(typeFile.c_str(),"habSuit")==0 || strcmp(typeFile.c_str(),"dist")==0 | strcmp(typeFile.c_str(),"aliens")==0){
        for (string newName : newNameFiles)
        {
          newMaps.emplace_back( ReadMask<double>( newName, 0.0, 1.0 ) );
        }
      } else if (strcmp(typeFile.c_str(),"drought")==0){
        for (string newName : newNameFiles)
        {
          newMaps.emplace_back( ReadMask<double>( newName, -5000.0, 1000.0 ) );
        }
      }
      if (strcmp(typeFile.c_str(),"habSuit")==0){ setEnvSuitMap(SpatialStack<double, double>( &m_Coord, newMaps));
      } else if (strcmp(typeFile.c_str(),"dist")==0){ setDistMap(SpatialStack<double, double>( &m_Coord, newMaps));
      } else if (strcmp(typeFile.c_str(),"drought")==0){ setDroughtMap(SpatialMap<double, double>( &m_Coord, newMaps[0]));
      } else if (strcmp(typeFile.c_str(),"aliens")==0){ setCondInitMap(SpatialStack<double, double>( &m_Coord, newMaps));
      }
    } else if ((strcmp(typeFile.c_str(),"mask")==0) | (strcmp(typeFile.c_str(),"fire")==0))
    {
      vector< vector<int> > newMaps; // INTEGER VALUES
      newMaps.reserve(noFiles);
      for (string newName : newNameFiles)
      {
        newMaps.emplace_back( ReadMask<int>( newName, 0.0, 1.0 ) );
      }
      if (strcmp(typeFile.c_str(),"mask")==0)
      {
        setMask(SpatialMap<double, int>( &m_Coord, newMaps[0]));
        
        /* If studied area changed, change also the ids of used cells */
        vector<int> newMaskCells;
        newMaskCells.reserve(m_Mask.getTotncell());
        for (int cell_ID=0; cell_ID<m_Mask.getTotncell(); cell_ID++)
        {
          if (m_Mask(cell_ID) == 1)
          {
            newMaskCells.emplace_back(cell_ID);
          }
        }
        newMaskCells.shrink_to_fit();
        setMaskCells(newMaskCells);
      } else if(strcmp(typeFile.c_str(),"fire")==0){ setFireMap(SpatialStack<double, int>( &m_Coord, newMaps));
      }
    }
    /* Indicate that operation succeeded */
    logg.info("Done");
  } else
  {
    logg.error("Impossible to open ", newChangeFile, " file!");
  }
} // end of DoFileChange(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::DoFreqChange(string newChangeFile, string typeFile)
{
  logg.info("Try to get change parameters :");
  
  /* Updating Maps : open newChangeFile */
  ifstream file(newChangeFile.c_str(), ios::in);
  if (file)
  {
    /* Read file line by line */
    string strTmp; // tmp string to keep change filenames
    vector< int > newfreq;
    while (file >> strTmp)
    {
      if (strTmp != "")
      {
        /* store new freq values */
        newfreq.push_back(atoi(strTmp.c_str()));
        logg.info("*** ", strTmp);
      }
    }
    /* Close file */
    file.close();
    
    if (strcmp(typeFile.c_str(),"dist")==0){ m_glob_params.setFreqDist(newfreq);
    } else if (strcmp(typeFile.c_str(),"fire")==0){ m_glob_params.setFreqFireDist(newfreq);
    } else if(strcmp(typeFile.c_str(),"aliens")==0){ m_glob_params.setFreqAliens(newfreq);
    }
    
    /* Indicate that operation succeeded */
    logg.info("Done");
  } else
  {
    logg.error("Impossible to open ", newChangeFile, " file!");
  }
} // end of DoFreqChange(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::DoSuccession(bool doLog)
{
  /*	time_t start,end;
   time(&start);*/
  
  if (m_glob_params.getDoHabSuitability())
  {
    /* Defined the new environmental reference value for this year */
    this->UpdateEnvSuitRefMap(m_glob_params.getHabSuitMode());
  }
  
  vector <vector<int> > isDrought(m_Mask.getTotncell(),vector<int>(m_glob_params.getNoFG(),0));
  if (m_glob_params.getDoDroughtDisturbances())
  {
    for (vector<int>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
    {
      for (int fg=0; fg<m_glob_params.getNoFG(); fg++)
      {
        isDrought[*cell_ID][fg] = (m_IsDroughtMap(*cell_ID, fg));
      }
    }
  }
  
  /* Do succession only on points within mask */
  omp_set_num_threads(m_glob_params.getNoCPU());
  #pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
  
  for (int cell_ID : m_MaskCells)
  {
    m_SuccModelMap(cell_ID)->DoSuccessionPart1(isDrought[cell_ID]);
    // if (doLog && logg.getVerbosity() == 0) {
    //   m_SuccModelMap(cell_ID)->show();
    // }
  }
  
  /*  time(&end);
   logg.info("> Dosuccession took ", difftime (end,start), " s");	*/
}


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::DoAliensIntroduction(int yr)
{
  /* Do aliens introduction depending on their frequency */
  vector<bool> applyIntro;
  for (vector<int>::const_iterator it=m_glob_params.getFreqAliens().begin(); it!=m_glob_params.getFreqAliens().end(); ++it)
  {
    if (*it==1)
    {
      applyIntro.push_back(true);
      logg.info("Do aliens introduction...");
    } else if (*it==0)
    {
      applyIntro.push_back(false);
    } else if (yr%(*it)==0)
    {
      applyIntro.push_back(true);
      logg.info("Do aliens introduction...");
    } else
    {
      applyIntro.push_back(false);
    }
  }
  
  /* Do succession only on points within mask */
  omp_set_num_threads( m_glob_params.getNoCPU() );
  #pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
  
  for (int cell_ID : m_MaskCells)
  {
    for (int fg = 0; fg<m_glob_params.getNoFG(); fg++)
    {
      if (applyIntro[fg] && m_CondInitMap(cell_ID, fg)>0.0)
      {
        m_SuccModelMap(cell_ID)->setSeedRain(fg, m_SuccModelMap(cell_ID)->getSeedRain(fg) +
          static_cast<int>(m_glob_params.getSeedingInput() * m_CondInitMap(cell_ID, fg)));
      }
    } //end loop on PFGs
  } // end loop on cells
} // end of DoAliensIntroduction(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::DoDispersal()
{
  /* IF OPTION 1 : draw two sets of integer random numbers for d2 distance */
  logg.info("Step 1...");
  vector< vector< vector<int> > > randInt_1, randInt_2;
  randInt_1.resize(m_FGparams.size(), vector< vector<int> >(m_SeedMapIn.getTotncell(), vector<int>(0)));
  randInt_2.resize(m_FGparams.size(), vector< vector<int> >(m_SeedMapIn.getTotncell(), vector<int>(0)));
  if (m_glob_params.getDispersalMode() == 1)
  {
    for (unsigned fg=0; fg<m_FGparams.size(); fg++)
    {
      unsigned noDrawMax = max(1, static_cast<int>(ceil(m_DispModel.getFGdistCircle(fg, 0).size()/2.0)));
      
      for (int cell_id=0; cell_id<m_SeedMapIn.getTotncell(); cell_id++)
      { // loop on pixels
        randInt_1[fg][cell_id].clear();
        randInt_2[fg][cell_id].clear();
        randInt_1[fg][cell_id].reserve(noDrawMax);
        randInt_2[fg][cell_id].reserve(m_DispModel.getFGdistCircle(fg, 1).size());
        
        UniInt distrib1(0, m_DispModel.getFGdistCircle(fg, 1).size() - 1);
        for (unsigned noDraw = 0; noDraw < noDrawMax; noDraw++)
        {
          randInt_1[fg][cell_id].emplace_back(distrib1(m_RNG));
        }
        UniInt distrib2(0, 3);
        for (unsigned id = 0; id < m_DispModel.getFGdistCircle(fg, 1).size(); id++)
        {
          randInt_2[fg][cell_id].emplace_back(distrib2(m_RNG));
        }
      } // end loop over pixels
    } // end loop over PFG
  }
  
  
  /* IF OPTION 3 : draw two sets of real random numbers */
  logg.info("Step 2...");
  vector< vector<double> > rand01_1, rand01_2;
  rand01_1.resize(m_FGparams.size(), vector<double>(0.0));
  rand01_2.resize(m_FGparams.size(), vector<double>(0.0));
  if (m_glob_params.getDispersalMode() == 3)
  {
    for (unsigned fg=0; fg<m_FGparams.size(); fg++)
    {
      rand01_1[fg].clear();
      rand01_2[fg].clear();
      rand01_1[fg].reserve(m_DispModel.getFGdistCircle(fg, 0).size());
      rand01_2[fg].reserve(m_DispModel.getFGdistCircle(fg, 1).size());
      
      UniReal random_01(0.0, 1.0);
      for (unsigned id = 0; id < m_DispModel.getFGdistCircle(fg, 0).size(); id++)
      {
        rand01_1[fg].emplace_back(random_01(m_RNG));
      }
      for (unsigned id = 0; id < m_DispModel.getFGdistCircle(fg, 1).size(); id++)
      {
        rand01_2[fg].emplace_back(random_01(m_RNG));
      }
    } // end loop over PFG
  }
  
  
  /* IN ALL CASES : draw one set of real random numbers for dld distance */
  logg.info("Step 3...");
  vector< vector<int> > LD_draw;
  LD_draw.resize(m_FGparams.size(), vector<int>(0));
  for (unsigned fg=0; fg<m_FGparams.size(); fg++)
  {
    if (m_DispModel.getFGdistCircle(fg, 2).size() > 0)
    {
      LD_draw[fg].clear();
      LD_draw[fg].reserve(m_SeedMapIn.getTotncell());
      
      UniInt distrib3(0, m_DispModel.getFGdistCircle(fg, 2).size() - 1);
      for (int cell_id=0; cell_id<m_SeedMapIn.getTotncell(); cell_id++)
      {
        LD_draw[fg].emplace_back(distrib3(m_RNG)); //rand()% vSize;
      } // end loop over pixels
    }
  } // end loop over PFG
  
  
  logg.info("Go for dispersal...");
  m_SeedMapOut.emptyStack();
  m_DispModel.DoDispersalPacket(m_glob_params.getDispersalMode(), m_glob_params.getNoCPU(), m_MaskCells
                                  , randInt_1, randInt_2, rand01_1, rand01_2, LD_draw);
  m_SeedMapIn.emptyStack();
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

vector<int> SimulMap::DoIgnition(int dist, vector<int> availCells)
{
  UniReal random_01(0.0, 1.0);
  
  int noFires = m_glob_params.getFireIgnitNo()[dist];
  if (m_glob_params.getFireIgnitMode()==2)
  { /* No of starting fires : normal distribution */
Normal distrib(noFires,noFires/10+1);
    noFires = distrib(m_RNG);
  } else if (m_glob_params.getFireIgnitMode()==3)
  {  /* No of starting fires : previous data distribution */
UniInt distrib(0,m_glob_params.getFireIgnitNoHist().size()-1);
    noFires = m_glob_params.getFireIgnitNoHist()[distrib(m_RNG)];
  }
  
  vector<int> startCell;
  
  /* Randomly distributed over the landscape */
  if (m_glob_params.getFireIgnitMode()==1 || m_glob_params.getFireIgnitMode()==2 || m_glob_params.getFireIgnitMode()==3)
  {
    UniInt distrib(0,availCells.size()-1);
    for (int n=0; n<noFires; n++)
    {
      startCell.push_back(availCells[distrib(m_RNG)]); //rand() % availCells.size();
    }
  } else if (m_glob_params.getFireIgnitMode()==4) /* ChaoLi probability adaptation */
  {
    for (vector<int>::iterator cell_ID=availCells.begin(); cell_ID!=availCells.end(); ++cell_ID)
    {
      /* Baseline proba */
      double probBL = m_glob_params.getFireIgnitLogis()[0] / (1+exp(m_glob_params.getFireIgnitLogis()[1]-m_glob_params.getFireIgnitLogis()[2]*m_TslfMap(*cell_ID)));
      /* Fuel proba */
      // TODO: change unsigned to int !!!!!
      unsigned abundTmpTot=0;
      vector<unsigned> abundTmpFG;
      for (unsigned fg=0; fg<m_FGparams.size(); fg++)
      {
        unsigned abundTmp = m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund();
        abundTmpTot += abundTmp;
        abundTmpFG.push_back(abundTmp);
      }
      double probFuel = 0;
      if (abundTmpTot>0)
      {
        for (unsigned fg=0; fg<m_FGparams.size(); fg++)
        {
          probFuel += (m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getFlamm() / m_glob_params.getFireIgnitFlammMax())* (abundTmpFG[fg]/abundTmpTot);
        }
      }
      /* Drought proba */
      double probDrought = (-1.0) * m_DroughtMap(*cell_ID);
      if (random_01(m_RNG) < probBL*probFuel*probDrought)
      { //(rand()/(double)RAND_MAX)
        startCell.push_back(*cell_ID);
      }
    }
  }
  return(startCell);
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

vector<int> SimulMap::DoPropagation(int dist, vector<int> start, vector<int> availCells)
{
  UniReal random_01(0.0, 1.0);
  
  vector<int> preCell, currCell, postCell, neighCell;
  currCell = start;
  double prob = 0.0, lim = 100.0 /* maxStep */, stepCount = 0.0 /* maxStep, maxConsume */;
  lim = m_glob_params.getFireQuotaMax();
  if (m_glob_params.getFireQuotaMode()==2)
  { // maxConsume
    //lim = 1000.0;
    lim = m_glob_params.getFireQuotaMax();
  } else if (m_glob_params.getFireQuotaMode()==3)
  { // maxCell
    //lim = 0.3*m_MaskCells.size();
    lim = m_glob_params.getFireQuotaMax();
    stepCount = currCell.size();
  } else if (m_glob_params.getFireQuotaMode()==4)
  { // keepGoing
    lim = 0.0;
    stepCount = (-1.0)*currCell.size();
  }
  
  while (currCell.size())
  {
    /* FIRST CASES : fire spread depends on a probability of the current burning cell */
    if (m_glob_params.getFirePropMode()==1)
    { // -------------------------------------------------------------------------------------
      for (vector<int>::iterator it1=currCell.begin(); it1!=currCell.end(); ++it1)
      {
        /* Get the IDs of the 8 neighbour cells */
        for (int xx=-1; xx<=1; xx++)
        {
          for (int yy=-1; yy<=1; yy++)
          {
            int id = *it1+xx*m_Mask.getYncell()+yy;
            if (id>=0 && // border precaution
                id<m_Mask.getTotncell() && // border precaution
                m_Mask(id)==1 && // studied area
                find(availCells.begin(),availCells.end(),id)!=availCells.end() && // not already burnt
                id!=*it1 && // current cell
                find(postCell.begin(),postCell.end(),id)==postCell.end())
            { // not already burnt
              neighCell.push_back(id);
            }
          }
        }
        
        /* fireIntensity */
        prob = m_glob_params.getFirePropIntensity()[dist];
        
        /* For each neighbour cell : does the fire propagate ? */
        for (vector<int>::iterator it2=neighCell.begin(); it2!=neighCell.end(); ++it2)
        {
          if (find(postCell.begin(),postCell.end(),*it2)==postCell.end() && 
              find(preCell.begin(),preCell.end(),*it2)==preCell.end() && random_01(m_RNG) < prob)
          { //(rand()/(double)RAND_MAX)
            preCell.push_back(*it2);
            if (m_glob_params.getFireQuotaMode()==2 /* "maxConsume" */){ stepCount += prob; }
          }
        } // end loop on neighCell
        neighCell.clear();
      } // end loop on currCell
    } else if (m_glob_params.getFirePropMode()==2)
    { // -------------------------------------------------------------------------------------
      for (vector<int>::iterator it1=currCell.begin(); it1!=currCell.end(); ++it1)
      {
        /* Get the IDs of the 8 neighbour cells */
        for (int xx=-1; xx<=1; xx++)
        {
          for (int yy=-1; yy<=1; yy++)
          {
            int id = *it1+xx*m_Mask.getYncell()+yy;
            if (id>=0 && // border precaution
                id<m_Mask.getTotncell() && // border precaution
                m_Mask(id)==1 && // studied area
                find(availCells.begin(),availCells.end(),id)!=availCells.end() && // not already burnt
                id!=*it1 && // current cell
                find(postCell.begin(),postCell.end(),id)==postCell.end())
            { // not already burnt
              neighCell.push_back(id);
            }
          }
        }
        
        /* percentConsumed : How much stuff was consumed in the current cell ? */
        unsigned abundTmpTot = 0;
        vector<unsigned> abundTmpFG;
        vector<double> propKillFG;
        
        for (unsigned fg=0; fg<m_FGparams.size(); fg++)
        { // loop on FG
          unsigned abundTmp = m_SuccModelMap(*it1)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund();
          abundTmpTot += abundTmp;
          abundTmpFG.push_back(abundTmp);
          
          double propKill = 0.0;
          vector<vector< vector<int> > > fates = m_SuccModelMap(*it1)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getFireResponse().getFates();
          for (unsigned sub=0; sub<fates[dist].size(); sub++)
          {
            propKill += IntToDouble(fates[dist][sub][Kill]);
          }
          propKillFG.push_back(propKill);
        } // end loop on FG
        
        prob = 0.0;
        if (abundTmpTot>0)
        {
          for (unsigned fg=0; fg<m_FGparams.size(); fg++)
          {
            prob += 1.0*propKillFG[fg]*abundTmpFG[fg]/abundTmpTot;
          }
        }
        
        /* For each neighbour cell : does the fire propagate ? */
        for (vector<int>::iterator it2=neighCell.begin(); it2!=neighCell.end(); ++it2)
        {
          if (find(postCell.begin(),postCell.end(),*it2)==postCell.end() && 
              find(preCell.begin(),preCell.end(),*it2)==preCell.end() && random_01(m_RNG) < prob)
          { //(rand()/(double)RAND_MAX)
            preCell.push_back(*it2);
            if (m_glob_params.getFireQuotaMode()==2 /* "maxConsume" */){ stepCount += prob; }
          }
        } // end loop on neighCell
        neighCell.clear();
      } // end loop on currCell
    }
    
    /* SECOND CASE : fire spread depends on a probability of the 8 neighboring cells of the current burning cell */
    else if (m_glob_params.getFirePropMode()==3 /* "maxAmountFuel" */)
    { // -------------------------------------------------------------------------------------
      for (vector<int>::iterator it1=currCell.begin(); it1!=currCell.end(); ++it1)
      {
        /* Get the IDs of the 8 neighbour cells */
        for (int xx=-1; xx<=1; xx++)
        {
          for (int yy=-1; yy<=1; yy++)
          {
            int id = *it1+xx*m_Mask.getYncell()+yy;
            if (id>=0 && // border precaution
                id<m_Mask.getTotncell() && // border precaution
                m_Mask(id)==1 && // studied area
                find(availCells.begin(),availCells.end(),id)!=availCells.end() && // not already burnt
                id!=*it1 && // current cell
                find(postCell.begin(),postCell.end(),id)==postCell.end())
            { // not already burnt
              neighCell.push_back(id);
            }
          }
        }
        
        vector<unsigned> abundTmp;
        for (vector<int>::iterator it2=neighCell.begin(); it2!=neighCell.end(); ++it2)
        {
          unsigned abund = 0;
          for (unsigned fg=0; fg<m_FGparams.size(); fg++)
          {
            abund += m_SuccModelMap(*it2)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund() * m_SuccModelMap(*it2)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getFlamm();
          }
          abundTmp.push_back(abund);
        } // end loop on neighCell
        if (accumulate(abundTmp.begin(),abundTmp.end(),0)>0)
        {
          unsigned posMaxCell = distance(abundTmp.begin(),max_element(abundTmp.begin(),abundTmp.end()));
          unsigned maxCell = neighCell[posMaxCell];
          if (find(postCell.begin(),postCell.end(),maxCell)==postCell.end() && find(preCell.begin(),preCell.end(),maxCell)==preCell.end())
          {
            preCell.push_back(maxCell);
          }
          
          /* if you want to burn all the cells with the max amount of fuel (and not only one) */
          while (count(abundTmp.begin(),abundTmp.end(),*max_element(abundTmp.begin(),abundTmp.end()))>1)
          {
            abundTmp[posMaxCell] = 0;
            posMaxCell = distance(abundTmp.begin(),max_element(abundTmp.begin(),abundTmp.end()));
            maxCell = neighCell[posMaxCell];
            if (find(postCell.begin(),postCell.end(),maxCell)==postCell.end() && find(preCell.begin(),preCell.end(),maxCell)==preCell.end())
            {
              preCell.push_back(maxCell);
            }
          }
        }
        neighCell.clear();
      } // end loop on currCell
    }  else if (m_glob_params.getFirePropMode()==4 /* "maxAmountSoil" */)
    { // -------------------------------------------------------------------------------------
      for (vector<int>::iterator it1=currCell.begin(); it1!=currCell.end(); ++it1)
      {
        /* Get the IDs of the 8 neighbour cells */
        for (int xx=-1; xx<=1; xx++)
        {
          for (int yy=-1; yy<=1; yy++)
          {
            int id = *it1+xx*m_Mask.getYncell()+yy;
            if (id>=0 && // border precaution
                id<m_Mask.getTotncell() && // border precaution
                m_Mask(id)==1 && // studied area
                find(availCells.begin(),availCells.end(),id)!=availCells.end() && // not already burnt
                id!=*it1 && // current cell
                find(postCell.begin(),postCell.end(),id)==postCell.end())
            { // not already burnt
              neighCell.push_back(id);
            }
          }
        }
        
        vector<double> soilTmp;
        for (vector<int>::iterator it2=neighCell.begin(); it2!=neighCell.end(); ++it2)
        {
          soilTmp.push_back(m_SuccModelMap(*it2)->getSoilResources());
        } // end loop on neighCell
        if (accumulate(soilTmp.begin(),soilTmp.end(),0)>0)
        {
          int maxCell = neighCell[distance(soilTmp.begin(),max_element(soilTmp.begin(),soilTmp.end()))];
          if (find(postCell.begin(),postCell.end(),maxCell)==postCell.end() && find(preCell.begin(),preCell.end(),maxCell)==preCell.end())
          {
            preCell.push_back(maxCell);
          }
        }
        neighCell.clear();
      } // end loop on currCell
    }  else if (m_glob_params.getFirePropMode()==5 /* "probLandClim" */)
    { // -------------------------------------------------------------------------------------
      for (vector<int>::iterator it1=currCell.begin(); it1!=currCell.end(); ++it1)
      {
        /* Get the IDs of the 8 neighbour cells */
        for (int xx=-1; xx<=1; xx++)
        {
          for (int yy=-1; yy<=1; yy++)
          {
            int id = *it1+xx*m_Mask.getYncell()+yy;
            if (id>=0 && // border precaution
                id<m_Mask.getTotncell() && // border precaution
                m_Mask(id)==1 && // studied area
                find(availCells.begin(),availCells.end(),id)!=availCells.end() && // not already burnt
                id!=*it1 && // current cell
                find(postCell.begin(),postCell.end(),id)==postCell.end())
            { // not already burnt
              neighCell.push_back(id);
            }
          }
        }
        
        for (vector<int>::iterator it2=neighCell.begin(); it2!=neighCell.end(); ++it2)
        {
          if (find(postCell.begin(),postCell.end(),*it2)==postCell.end() && find(preCell.begin(),preCell.end(),*it2)==preCell.end())
          {
            /* Baseline proba */
            double probBL = m_glob_params.getFirePropLogis()[0] / (1+exp(m_glob_params.getFirePropLogis()[1]-m_glob_params.getFirePropLogis()[2]*m_TslfMap(*it2)));
            /* Fuel proba */
            unsigned abundTmp, abundTmpTot=0;
            vector<unsigned> abundTmpFG;
            for (unsigned fg=0; fg<m_FGparams.size(); fg++)
            {
              abundTmp = m_SuccModelMap(*it2)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund();
              abundTmpTot += abundTmp;
              abundTmpFG.push_back(abundTmp);
            }
            double probFuel = 0.0;
            if (abundTmpTot>0)
            {
              for (unsigned fg=0; fg<m_FGparams.size(); fg++)
              {
                probFuel += (m_SuccModelMap(*it2)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getFlamm() /*/ m_glob_params.getFireIgnitFlammMax()*/)* (abundTmpFG[fg]/abundTmpTot);
              }
            }
            /* Drought proba */
            double probDrought = (-1.0) * m_DroughtMap(*it2);
            
            /* Slope adjustement */
            double probSlope;
            if (m_ElevationMap(*it2)>m_ElevationMap(*it1))
            {
              probSlope = 1 + 0.001*m_SlopeMap(*it2);
            } else
            {
              probSlope = 1 + 0.001*max(-30.0,(-1.0)*m_SlopeMap(*it2));
            }
            
            if (random_01(m_RNG) < probBL*probFuel*probDrought*probSlope)
            { //(rand()/(double)RAND_MAX)
              preCell.push_back(*it2);
            }
          }
        } // end loop on neighCell
        neighCell.clear();
      } // end loop on currCell
    }
    
    /* IN ANY CASE : Update cells vectors : CURR->POST and PRE->CURR*/
    int newBurnt = postCell.size();
    postCell.insert(postCell.end(),currCell.begin(),currCell.end());
    currCell.clear();
    if (preCell.size()>0)
    {
      /* Remove the cells that have already been burnt */
      unsigned pos = 0, siz = preCell.size()-1;
      while (pos<siz)
      {
        if (find(postCell.begin(),postCell.end(),preCell[pos])!=postCell.end())
        {
          swap(preCell[pos],preCell[siz]);
          preCell.pop_back();
          siz = preCell.size()-1;
        } else { pos++; }
      }
      currCell.insert(currCell.end(),preCell.begin(),preCell.end());
      preCell.clear();
    }
    newBurnt = postCell.size()-newBurnt;
    
    /* Update the limit option */
    if (m_glob_params.getFireQuotaMode()==1 /* "maxStep" */){ stepCount++;
    } else if (m_glob_params.getFireQuotaMode()==3 /* "maxCell" */){ stepCount += newBurnt;
    } if (m_glob_params.getFireQuotaMode()==4 /* "keepGoing" */){ stepCount = (-1.0)*currCell.size();
    }
    
    if(stepCount>=lim) { break; }
    
  } // end while
  
  /* if you want to stop the fires only when you reach the quota (maxConsume & maxCell) */
  /*	if (stepCount<lim)
   {
   vector<int> newAvailCell = m_MaskCells;
   vector<unsigned>::iterator it;
   // erase cells that have already burnt
   for (vector<unsigned>::iterator cell_ID=postCell.begin(); cell_ID!=postCell.end(); ++cell_ID)
   {
   it = find(newAvailCell.begin(),newAvailCell.end(),*cell_ID);
   newAvailCell.erase(it);
   }
   vector<int> newStartCell = DoIgnition(dist,newAvailCell);
   vector<int> newBurntCell = DoPropagation(dist,newStartCell,newAvailCell);
   }
   for (vector<unsigned>::iterator cell_ID=newBurntCell.begin(); cell_ID!=newBurntCell.end(); ++cell_ID)
   {
   if (find(postCell.begin(),postCell.end(),*cell_ID)==postCell.end())
   {
   postCell.push_back(*cell_ID);
   }
   }*/
  
  return(postCell);
}

void SimulMap::DoUpdateTslf(vector<int> burnt)
{
  for (vector<int>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
  {
    if (find(burnt.begin(),burnt.end(),*cell_ID)!=burnt.end())
    {
      m_TslfMap.setValue(*cell_ID,0);
    } else {
      m_TslfMap.setValue(*cell_ID,m_TslfMap.getValue(*cell_ID)+1);
    }
  }
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::DoFireDisturbance(int yr)
{
  /* Do fire disturbances depending on their frequency */
  vector<bool> applyDist;
  for (vector<int>::const_iterator it=m_glob_params.getFreqFireDist().begin(); it!=m_glob_params.getFreqFireDist().end(); ++it)
  {
    if (*it==1) { applyDist.push_back(true);
    } else if (*it==0) { applyDist.push_back(false);
    } else if (yr%(*it)==0) { applyDist.push_back(true);
    } else { applyDist.push_back(false);
    }
  }
  
  /* If fire disturbances occur this year :
   apply ignition function
   apply propagation function
   update the fire disturbances masks */
  vector< vector<int> > ALLburntCell(m_glob_params.getNoFireDist());
  if (m_glob_params.getFireIgnitMode()!=5 /* map */ && m_glob_params.getFireNeighMode()==1)
  { // CASE 1 ---------------------------------------------------------------------------------------
    for (int dist=0; dist<m_glob_params.getNoFireDist(); dist++)
    {
      if (applyDist[dist])
      {
        vector<int> startCell = DoIgnition(dist,m_MaskCells);
        ALLburntCell[dist] = DoPropagation(dist,startCell,m_MaskCells);;
      }
    }
  } else if (m_glob_params.getFireIgnitMode()!=5 /* map */)
  { // CASE 2 ---------------------------------------------------------------------------------------
    int no = m_glob_params.getFireNeighCC()[0];
    int ea = m_glob_params.getFireNeighCC()[1];
    int so = m_glob_params.getFireNeighCC()[2];
    int we = m_glob_params.getFireNeighCC()[3];
    
    if (m_glob_params.getFireNeighMode()==3 /* "extentRand" */)
    { // CASE 2a --------------------------------------------------------------
      UniInt distrib_no(0,m_glob_params.getFireNeighCC()[0]);
      UniInt distrib_ea(0,m_glob_params.getFireNeighCC()[1]);
      UniInt distrib_so(0,m_glob_params.getFireNeighCC()[2]);
      UniInt distrib_we(0,m_glob_params.getFireNeighCC()[3]);
      
      for (int dist=0; dist<m_glob_params.getNoFireDist(); dist++)
      {
        if (applyDist[dist])
        {
          vector<int> startCell = DoIgnition(dist,m_MaskCells);
          vector<int> burntCell;
          for (vector<int>::iterator it1=startCell.begin(); it1!=startCell.end(); ++it1)
          {
            no = distrib_no(m_RNG); //rand() % no + 1;
            ea = distrib_ea(m_RNG); //rand() % ea + 1;
            we = distrib_we(m_RNG); //rand() % we + 1;
            so = distrib_so(m_RNG); //rand() % so + 1;
            for (int yy=(-no); yy<=so; yy++)
            {
              for (int xx=(-we); xx<=ea; xx++)
              {
                int id = *it1+yy+xx*m_Mask.getYncell();
                if ( id>=0 && /* border precaution */
  id<m_Mask.getTotncell() && /* border precaution */
  find(burntCell.begin(),burntCell.end(),id)==burntCell.end() && /* not already burnt */
  m_Mask(id)==1)
                { // studied area
                  burntCell.push_back(id);
                }
              }
            }
          }
          ALLburntCell[dist] = burntCell;
        }
      }
    } else
    { // CASE 2b --------------------------------------------------------------
      for (int dist=0; dist<m_glob_params.getNoFireDist(); dist++)
      {
        if (applyDist[dist])
        {
          vector<int> startCell = DoIgnition(dist,m_MaskCells);
          vector<int> burntCell;
          for (vector<int>::iterator it1=startCell.begin(); it1!=startCell.end(); ++it1)
          {
            for (int yy=(-no); yy<=so; yy++)
            {
              for (int xx=(-we); xx<=ea; xx++)
              {
                int id = *it1+yy+xx*m_Mask.getYncell();
                if ( id>=0 && /* border precaution */
  id<m_Mask.getTotncell() && /* border precaution */
  find(burntCell.begin(),burntCell.end(),id)==burntCell.end() && /* not already burnt */
  m_Mask(id)==1)
                { // studied area
                  burntCell.push_back(id);
                }
              }
            }
          }
          ALLburntCell[dist] = burntCell;
        }
      }
    }
  } else if (m_glob_params.getFireIgnitMode()==5 /* map */)
  { // CASE 3 ---------------------------------------------------------------------------------------
    for (int dist=0; dist<m_glob_params.getNoFireDist(); dist++)
    {
      if (applyDist[dist])
      {
        for (vector<int>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
        {
          if (m_FireMap(*cell_ID, dist) == 1 )
          {
            ALLburntCell[dist].push_back(*cell_ID);
          }
        }
      }
    }
  }
  
  /* If a cell has been burnt by several disturbances, only the most severe is applied */
  for (int dist2=m_glob_params.getNoFireDist()-1; dist2>0; dist2--)
  {
    for (int dist1=dist2-1; dist1>=0; dist1--)
    {
      if (ALLburntCell[dist2].size()>0 && ALLburntCell[dist1].size()>0)
      {
        for (unsigned pos=0; pos<ALLburntCell[dist2].size(); pos++)
        {
          vector<int>::iterator it = find(ALLburntCell[dist1].begin(),ALLburntCell[dist1].end(),ALLburntCell[dist2][pos]);
          if (it!=ALLburntCell[dist1].end())
          {
            ALLburntCell[dist1].erase(it);
          }
        }
      }
    }
  }
  
  /* Do fire disturbances only on points within mask */
  omp_set_num_threads( m_glob_params.getNoCPU() );
#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
  
  for (int dist=0; dist<m_glob_params.getNoFireDist(); dist++)
  { // loop on disturbances
    DoUpdateTslf(ALLburntCell[dist]);
    if (applyDist[dist] & (ALLburntCell[dist].size()>0))
    {
      logg.info("Fire this year !");
      for (vector<int>::iterator cell_ID=ALLburntCell[dist].begin(); cell_ID!=ALLburntCell[dist].end(); ++cell_ID)
      {
        for (unsigned fg=0; fg<m_FGparams.size(); fg++)
        { // loop on PFG
          m_SuccModelMap(*cell_ID)->DoDisturbance(fg, dist, 1.0, m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getFireResponse());
        }
      } // end loop over cells
    }
  } // end loop over fire disturbances
} // end of DoFireDisturbance(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::DoDroughtDisturbance_part1()
{
  /* Calculation of abundance per strata for each pixel */
  unsigned noStrata = m_glob_params.getNoStrata();
  SpatialMap<double, double> moistValues = getDroughtMap();
  
  omp_set_num_threads(m_glob_params.getNoCPU());
#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
  
  for (int cell_ID : m_MaskCells)
  { // loop on pixels
    vector<int> tmpAbund(noStrata+1, 0);
    double maxVal = 0.0;
    for (unsigned fg=0; fg<m_FGparams.size(); fg++)
    { // loop on PFG
      /* create a copy of FG parameters to simplify and speed up the code */
      FuncGroupPtr FuncG = m_SuccModelMap(cell_ID)->getCommunity_()->getFuncGroup_(fg);
      FGPtr FGparams = FuncG->getFGparams_();
      
      double totMaxAbund = static_cast<double>(FGparams->getMaxAbund());
      if (totMaxAbund)
      {
        totMaxAbund *= (1.0 + IntToDouble(FGparams->getImmSize()));
      }
      maxVal += totMaxAbund;
      
      if (m_SuccModelMap(cell_ID)->getCommunity_()->getNoCohort(fg) > 0)
      {
        /* Create a vector with stratum break ages */
        vector<int> bkStratAges = FGparams->getStrata();
        
        /* add PFG strata abundances */
        for (unsigned st=1; st<noStrata; st++)
        {
          tmpAbund[st] = static_cast<int>(FuncG->totalNumAbund( bkStratAges[st-1] , bkStratAges[st] - 1 ));
        } // end loop on Stratum
      }
    } // end loop on PFG
    
    /* Calculation of canopy closure : 0 = no canopy, 1 = full closure */
    double pixAbund = *max_element(tmpAbund.begin()+1, tmpAbund.end()); // SHOULD be only the upper stratum ?
    // accumulate(tmpAbund.begin(), tmpAbund.end(),0); ?
    // double maxVal = m_glob_params.getMaxAbundHigh() * m_FGparams.size(); // SHOULD by MaxAbundHigh * noPFG * (1 + ImmSizes) ?
    if (pixAbund>maxVal) pixAbund = maxVal;
    pixAbund = pixAbund/maxVal;
    if (pixAbund>0.5){ moistValues(cell_ID) = m_DroughtMap(cell_ID) + abs(m_DroughtMap(cell_ID))/2.0; } // SHOULD be adjustable both parameters (0.5 and 2) ?
  } // end loop on pixels
  
  
  
  /* Do disturbances only on points within mask */
  omp_set_num_threads( m_glob_params.getNoCPU() );
#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
  
  for (int cell_ID : m_MaskCells)
  {
    for (unsigned fg=0; fg<m_FGparams.size(); fg++)
    { // loop on PFG
      m_IsDroughtMap(cell_ID, fg) = 0;
      m_ApplyPostDroughtMap(cell_ID, fg) = 0;
      m_ApplyCurrDroughtMap(cell_ID, fg) = 0; // SHOULD be 0 ??
      
      if (m_SuccModelMap(cell_ID)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund() > 0)
      {
        /* Check Post Drought Mortality */
        if (m_PostDroughtMap(cell_ID, fg) == 1)
        {
          /* Set recruitment and fecundity to 0 */
          m_IsDroughtMap(cell_ID, fg) = 1;
          m_ApplyPostDroughtMap(cell_ID, fg) = 1;
        }
        
        /* Check Moisture Index */
        double moistIndex = moistValues(cell_ID);
        if (moistIndex > m_FGparams[fg].getDroughtThreshMod())
        {
          /* If NO drought : apply Drought Recovery */
          m_CountDroughtMap(cell_ID, fg) = max(static_cast<unsigned>(0), m_CountDroughtMap(cell_ID, fg) - m_FGparams[fg].getCountRecovery());
        } else
        {
          /* If drought : immediate effects */
          m_ApplyCurrDroughtMap(cell_ID, fg) = 1;
          
          /* Set recruitment and fecundity to 0 */
          m_IsDroughtMap(cell_ID, fg) = 1;
          
          /* Increase cumulated effect counter */
          m_CountDroughtMap(cell_ID, fg) = min(static_cast<unsigned>(m_FGparams[fg].getCountCum()), m_CountDroughtMap(cell_ID, fg)++);
          
          /* Check Post Drought Mortality */
          bool currSevDrought = (moistIndex < m_FGparams[fg].getDroughtThreshSev());
          bool modToSev = (!currSevDrought && (static_cast<int>(m_CountDroughtMap(cell_ID, fg)) >= m_FGparams[fg].getCountCum()));
          bool SevMort = (currSevDrought && (static_cast<int>(m_CountDroughtMap(cell_ID, fg)) >= m_FGparams[fg].getCountSens()));
          if (modToSev || SevMort)
          {
            m_PostDroughtMap(cell_ID, fg) = 1;
          }
        }
      } else
      {
        /* If NO PFG anymore : reset count */
        m_CountDroughtMap(cell_ID, fg) = 0;
      }
    }
  }
} // end of DoDroughtDisturbance(...)


void SimulMap::DoDroughtDisturbance_part2(string chrono)
{
  bool cond1 = (strcmp(chrono.c_str(), m_glob_params.getChronoPost().c_str()) == 0);
  bool cond2 = (strcmp(chrono.c_str(), m_glob_params.getChronoCurr().c_str()) == 0);
  
  if (cond1)
  {
    omp_set_num_threads( m_glob_params.getNoCPU() );
#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
    
    for (int cell_ID : m_MaskCells)
    { // loop on pixels
      for (unsigned fg=0; fg<m_FGparams.size(); fg++)
      { // loop on PFG
        /* create a copy of FG parameters to simplify and speed up the code */
        FGPtr FGparams = m_SuccModelMap(cell_ID)->getCommunity_()->getFuncGroup_(fg)->getFGparams_();
        
        if ((m_ApplyPostDroughtMap(cell_ID, fg)==1) && (m_ApplyCurrDroughtMap(cell_ID, fg)==0))
        { /* Apply post drought effects */
        //logg.info(">> Post drought effect this year !");
        m_SuccModelMap(cell_ID)->DoDisturbance(fg, 1, 1.0, FGparams->getDroughtResponse());
        }
      }
    }
  } else if (cond2)
  {
    omp_set_num_threads( m_glob_params.getNoCPU() );
#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
    
    for (int cell_ID : m_MaskCells)
    { // loop on pixels
      for (unsigned fg=0; fg<m_FGparams.size(); fg++)
      { // loop on PFG
        /* create a copy of FG parameters to simplify and speed up the code */
        FGPtr FGparams = m_SuccModelMap(cell_ID)->getCommunity_()->getFuncGroup_(fg)->getFGparams_();
        
        if ((m_ApplyCurrDroughtMap(cell_ID, fg) == 1) && (m_ApplyPostDroughtMap(cell_ID, fg) == 0))
        { /* Apply current drought effects */
        //logg.info(">> Current drought effect this year !");
        m_SuccModelMap(cell_ID)->DoDisturbance(fg, 0, 1.0, FGparams->getDroughtResponse());
        } else if ((m_ApplyCurrDroughtMap(cell_ID, fg) == 1) && (m_ApplyPostDroughtMap(cell_ID, fg) == 1))
        { /* Apply cumulated post-current drought effects */
        //logg.info(">> Current+Post drought effect this year !");
        FGresponse CurrPostResp = FGparams->getDroughtResponse();
          vector<vector<int> > tmpBreakAge = CurrPostResp.getBreakAge(), tmpResprAge = CurrPostResp.getResprAge();
          vector<int> tmpActiveSeeds = CurrPostResp.getActiveSeeds();
          vector<int> tmpPropKilled = CurrPostResp.getPropKilled();
          vector<vector<vector<int> > > tmpFates = CurrPostResp.getFates();
          
          tmpBreakAge.push_back(tmpBreakAge[0]);
          tmpResprAge.push_back(tmpResprAge[0]);
          tmpActiveSeeds.push_back(tmpActiveSeeds[0]);
          if (tmpPropKilled[0] == 0)
          {
            tmpPropKilled.push_back(DoubleToInt(0.1));
          } else
          {
            tmpPropKilled.push_back(DoubleToInt(IntToDouble(tmpPropKilled[0]) * 1.1));
          }
          vector<vector<int> > tmpNewFates(tmpFates[0].size());
          for (unsigned sub=0; sub<tmpFates[0].size(); sub++)
          { // loop on subdist
            vector<int> tmpKiUnRe = tmpFates[0][sub];
            double tmpKiUnRe0 = IntToDouble(tmpFates[0][sub][0]); // Killed
            double tmpKiUnRe1 = IntToDouble(tmpFates[0][sub][1]); // Unaffected
            double tmpKiUnRe2 = IntToDouble(tmpFates[0][sub][2]); // Resprouting
            if (tmpKiUnRe0 != 1.0)
            { // not already 100% killed
              double mortSup = 0.0;
              if (tmpKiUnRe0 == 0.0)
              { // no killed
                mortSup = 0.1 * m_CountDroughtMap(cell_ID, fg);
              } else
              {
                mortSup = tmpKiUnRe0 * 0.1 * m_CountDroughtMap(cell_ID, fg);
              }
              tmpKiUnRe[0] = DoubleToInt(tmpKiUnRe0 + mortSup);
              if (tmpKiUnRe1 == 0.0)
              { // no unaffected
                tmpKiUnRe[1] = DoubleToInt(0.5 * (IntToDouble(tmpKiUnRe[0]) - tmpKiUnRe0));
              } else if (tmpKiUnRe2 == 0.0)
              { // no resprouting
                tmpKiUnRe[1] = DoubleToInt(tmpKiUnRe1 - 1.5 * (IntToDouble(tmpKiUnRe[0]) - tmpKiUnRe0));
              } else
              { // resprouting and unaffected
                tmpKiUnRe[1] = DoubleToInt(tmpKiUnRe1 - 0.5 * (IntToDouble(tmpKiUnRe[0]) - tmpKiUnRe0));
              }
              tmpKiUnRe[2] = getLeavingFract( tmpKiUnRe[0], tmpKiUnRe[1] );
            }
            tmpNewFates[sub] = tmpKiUnRe;
          }
          tmpFates.push_back(tmpNewFates);
          CurrPostResp.setBreakAge(tmpBreakAge);
          CurrPostResp.setResprAge(tmpResprAge);
          CurrPostResp.setActiveSeeds(tmpActiveSeeds);
          CurrPostResp.setPropKilled(tmpPropKilled);
          CurrPostResp.setFates(tmpFates);
          m_SuccModelMap(cell_ID)->DoDisturbance(fg, 0, 1.0, FGparams->getDroughtResponse());
        }
      }
    }
  }
} // end of DoDroughtDisturbance(...)


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::DoDisturbance(int yr)
{
  /* Do disturbances depending on their frequency */
  vector<int> applyDist;
  applyDist.reserve(m_glob_params.getNoDist());
  vector<bool> applyRand(m_glob_params.getNoDist(), false);
  for (int dist=0; dist<m_glob_params.getNoDist(); dist++)
  { // loop on disturbances
    if (m_glob_params.getFreqDist()[dist] != 0 && 
        (m_glob_params.getFreqDist()[dist] == 1 || yr%(m_glob_params.getFreqDist()[dist]) == 0))
    {
      applyDist.emplace_back(dist);
      if (m_glob_params.getProbDist()[dist] < 1.0)
      {
        applyRand[dist] = true;
      }
    }
  }
  applyDist.shrink_to_fit();
  logg.info("Disturbances to be applied (FREQ) :", applyDist);
  
  /* Do disturbances only if some need to */
  if (applyDist.size() > 0)
  {
    logg.info("Disturbances not applied everywhere (PROB) :", applyRand);

    vector< vector< double > > vecRandi(m_glob_params.getNoDist(), vector<double>(m_Mask.getTotncell(), 0.0));
    // for (int cell_ID : m_MaskCells)
    // {
    //   UniReal random_01(0.0, 1.0);
    //   double randi = random_01(m_RNG);
    //   for (int dist=0; dist<m_glob_params.getNoDist(); dist++)
    //   { // loop on disturbances
    //     if (dist > 0 && m_glob_params.getPairDist()[dist] != m_glob_params.getPairDist()[dist-1]) {
    //       randi = random_01(m_RNG);
    //     }
    //     vecRandi[dist][cell_ID] = randi;
    //   }
    // }
    
    vector <double> sumtotmapdist(m_glob_params.getNoDist(), 0.0);
    
    /* Do disturbances only on points within mask */
    omp_set_num_threads( m_glob_params.getNoCPU() );
    #pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
    
    for (int cell_ID : m_MaskCells)
    {
      for (int dist : applyDist)
      { // loop on disturbances
        sumtotmapdist[dist] += m_DistMap(cell_ID, dist);
        if (m_DistMap(cell_ID, dist) > 0.0)
        { // within mask
          // logg.info("Disturbance happening in cell (point A) :", dist, cell_ID);
          if (!applyRand[dist] ||
              (applyRand[dist] && vecRandi[dist][cell_ID] < m_glob_params.getProbDist()[dist]))
          { // & disturbance occurs in this cell
            // logg.info("Disturbance happening in cell (point B) :", dist, cell_ID);
            m_SuccModelMap(cell_ID)->DoDisturbance(dist, m_DistMap(cell_ID, dist));
          }
        }
      } // end loop over disturbances
    } // end loop over cells
    
    logg.info("IF NODIST applied, this should all be to 0 :", sumtotmapdist);
  }
} // end of DoDisturbance(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::UpdateEnvSuitRefMap(unsigned option)
{
  vector< double > envSuitRefVal(m_Mask.getTotncell(), 0.5);
  UniReal random_01(0.0, 1.0);
  
  if (option==1)
  {
    /* draw a random number for each pixel*/
    for (vector<int>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
    {
      envSuitRefVal[*cell_ID] = random_01(m_RNG); //( rand()/(double)RAND_MAX );
    }
    /* assign these same numbers for each pfg */
    m_EnvSuitRefMap.emptyStack();
    for (unsigned fg_id=0; fg_id<m_FGparams.size(); fg_id++)
    {
      m_EnvSuitRefMap.setValues(fg_id, envSuitRefVal);
    }
  } else if (option==2)
  {
    m_EnvSuitRefMap.emptyStack();
    for (unsigned fg_id=0; fg_id<m_FGparams.size(); fg_id++)
    {
      /* to each pfg assign a mean and a standard deviation */
      double meanFG = random_01(m_RNG); //( rand()/(double)RAND_MAX );
      double sdFG = random_01(m_RNG); //( rand()/(double)RAND_MAX );
      logg.info("NEW Env Suit Ref distrib for FG : ", fg_id, "  with mean=", meanFG, " and sd=", sdFG);
                
      /* build the distribution corresponding to these mean and sd */
      static Normal distrib(meanFG,sdFG);
      
      /* draw a random number from this distribution for each pixel*/
      envSuitRefVal.resize(m_Mask.getTotncell(),0.5);
      for (vector<int>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
      {
        envSuitRefVal[*cell_ID] = distrib(m_RNG);
      }
      m_EnvSuitRefMap.setValues(fg_id, envSuitRefVal);
    }
  } else
  {
    logg.error("Chosen option to update Environmental Suitability reference does not exist.\n",
               "Please select either 1 (one random number per pixel) or 2 (one distribution per PFG).");
  }
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::UpdateSimulationParameters(FOPL file_of_params)
{
  /* keep a copy of parameters that will be replaced */
  GSP old_glob_params(m_glob_params);
  
  /* read new global parameters file */
  logg.info("*** REBUILDING Global simulation parameters...");
  m_glob_params  = GSP(file_of_params.getGlobSimulParams());
  
  /* check some parameters compatibility (between SAVED_STATE and "new" global parameter file) */
  if (old_glob_params.getNoFG() != m_glob_params.getNoFG())
  {
    logg.error("!!! Number of functional groups involved in saved object (", old_glob_params.getNoFG(),
               ") is incompatible with new parameters (", m_glob_params.getNoFG(), ")!");
  }
  if (old_glob_params.getNoStrata() != m_glob_params.getNoStrata())
  {
    logg.error("!!! Number of strata involved in saved object (", old_glob_params.getNoStrata(),
               ") is incompatible with new parameters (", m_glob_params.getNoStrata(), ")!");
  }
  if (m_glob_params.getDoDisturbances())
  {
    if (old_glob_params.getNoDist() != m_glob_params.getNoDist())
    {
      logg.error("!!! Number of disturbances involved in saved object (", old_glob_params.getNoDist(),
                 ") is incompatible with new parameters (", m_glob_params.getNoDist(), ")!");
    }
    if (old_glob_params.getNoDistSub() != m_glob_params.getNoDistSub())
    {
      logg.error("!!! Number of way to be influenced by disturbances involved in saved object (", old_glob_params.getNoDistSub(),
                 ") is incompatible with new parameters (", m_glob_params.getNoDistSub(), ")!");
    }
  }
  if (m_glob_params.getDoDroughtDisturbances())
  {
    if (old_glob_params.getNoDroughtSub() != m_glob_params.getNoDroughtSub())
    {
      logg.error("!!! Number of way to be influenced by drought disturbances involved in saved object (", old_glob_params.getNoDroughtSub(),
                 ") is incompatible with new parameters (", m_glob_params.getNoDroughtSub(), ")!");
    }
  }
  if (m_glob_params.getDoFireDisturbances())
  {
    if (old_glob_params.getNoFireDist() != m_glob_params.getNoFireDist())
    {
      logg.error("!!! Number of fire disturbances involved in saved object (", old_glob_params.getNoFireDist(),
                 ") is incompatible with new parameters (", m_glob_params.getNoFireDist(), ")!");
    }
    if (old_glob_params.getNoFireDistSub() != m_glob_params.getNoFireDistSub())
    {
      logg.error("!!! Number of way to be influenced by fire disturbances involved in saved object (", old_glob_params.getNoFireDistSub(),
                 ") is incompatible with new parameters (", m_glob_params.getNoFireDistSub(), ")!");
    }
  }
  /* end of check parameters compatibility */
  
  /* check some parameters compatibility (between "new" global and simul parameter files) */
  int noFG = m_glob_params.getNoFG();
  if (noFG != static_cast<int>(file_of_params.getFGLifeHistory().size()))
  {
    logg.error("!!! Parameters NO_PFG (", noFG, ") and --PFG_PARAMS_LIFE_HISTORY-- (",
               file_of_params.getFGLifeHistory().size(), ") do not match in term of number!");
  }
  if (m_glob_params.getDoLightInteraction() && noFG != static_cast<int>(file_of_params.getFGLight().size()))
  {
    logg.error("!!! Parameters NO_PFG (", noFG, ") and --PFG_PARAMS_LIGHT-- (",
               file_of_params.getFGLight().size(), ") do not match in term of number!");
  }
  if (m_glob_params.getDoSoilInteraction() && noFG != static_cast<int>(file_of_params.getFGSoil().size()))
  {
    logg.error("!!! Parameters NO_PFG (", noFG, ") and --PFG_PARAMS_SOIL-- (",
               file_of_params.getFGSoil().size(), ") do not match in term of number!");
  }
  if (m_glob_params.getDoDispersal() && noFG != static_cast<int>(file_of_params.getFGDispersal().size()))
  {
    logg.error("!!! Parameters NO_PFG (", noFG, ") and --PFG_PARAMS_DISPERSAL-- (",
               file_of_params.getFGDispersal().size(), ") do not match in term of number!");
  }
  if (m_glob_params.getDoDisturbances() && noFG != static_cast<int>(file_of_params.getFGDisturbance().size()))
  {
    logg.error("!!! Parameters NO_PFG (", noFG, ") and --PFG_PARAMS_DISTURBANCES-- (",
               file_of_params.getFGDisturbance().size(), ") do not match in term of number!");
  }
  if (m_glob_params.getDoDroughtDisturbances() && noFG != static_cast<int>(file_of_params.getFGDrought().size()))
  {
    logg.error("!!! Parameters NO_PFG (", noFG, ") and --PFG_PARAMS_DROUGHT-- (",
               file_of_params.getFGDrought().size(), ") do not match in term of number!");
  }
  if (m_glob_params.getDoFireDisturbances() && noFG != static_cast<int>(file_of_params.getFGFire().size()))
  {
    logg.error("!!! Parameters NO_PFG (", noFG, ") and --PFG_PARAMS_FIRE-- (",
               file_of_params.getFGFire().size(), ") do not match in term of number!");
  }
  if (m_glob_params.getDoHabSuitability() && noFG != static_cast<int>(file_of_params.getFGMapsHabSuit().size()))
  {
    logg.error("!!! Parameters NO_PFG (", noFG, ") and --PFG_MASK_HABSUIT-- (",
               file_of_params.getFGMapsHabSuit().size(), ") do not match in term of number!");
  }
  if (m_glob_params.getDoAliensIntroduction() && noFG != static_cast<int>(file_of_params.getFGMapsAliens().size()))
  {
    logg.error("!!! Parameters NO_PFG (", noFG, ") and --PFG_MASK_ALIENS-- (",
               file_of_params.getFGMapsAliens().size(), ") do not match in term of number!");
  }
  /* end of check parameters compatibility */
  
  /* check for parameters file */
  file_of_params.checkCorrectParams(m_glob_params.getDoLightInteraction(),
                                    m_glob_params.getDoHabSuitability(),
                                    m_glob_params.getDoDispersal(),
                                    m_glob_params.getDoDisturbances(),
                                    m_glob_params.getDoSoilInteraction(),
                                    m_glob_params.getDoFireDisturbances(),
                                    m_glob_params.getDoDroughtDisturbances(),
                                    m_glob_params.getDoAliensIntroduction());
  
  /* update fg environmental suitability conditions if needed */
  if (file_of_params.getFGMapsHabSuit()[0] != "0")
  { // some new initial envsuit conditions given
    logg.info("***** Update habitat suitability maps...");
    vector< vector< double > > envSuitMap; // environmental suitability of fgs
    envSuitMap.reserve(m_FGparams.size());
    for (unsigned fg_id=0; fg_id<m_FGparams.size(); fg_id++)
    {
      envSuitMap.emplace_back( ReadMask<double>( file_of_params.getFGMapsHabSuit()[fg_id], 0.0, 1.0 ) );
    }
    m_EnvSuitMap = SpatialStack<double, double>(&m_Coord, envSuitMap);
    
    
    logg.info("***** Update habitat suitability reference maps...");
    vector< double >  envSuitRefVal( m_Mask.getTotncell(), 0.5 );
    vector< vector< double > > envSuitRefMap; // environmental suitability year reference of fgs
    envSuitRefMap.reserve(m_FGparams.size());
    for (unsigned fg_id=0; fg_id<m_FGparams.size(); fg_id++)
    {
      envSuitRefMap.emplace_back( envSuitRefVal );
    }
    m_EnvSuitRefMap = SpatialStack<double, double>(&m_Coord, envSuitRefMap);
  }
  
  /* update seed dispersal maps and model if needed */
  if (m_glob_params.getDoDispersal())
  {
    logg.info("***** Update out seed map...");
    vector< vector< int > > emptyMapInt;
    emptyMapInt.reserve(m_FGparams.size());
    vector< int >  emptyValInt( m_Mask.getTotncell(), 0 );
    for (unsigned fg_id=0; fg_id<m_FGparams.size(); fg_id++)
    {
      emptyMapInt.emplace_back( emptyValInt );
    }
    
    // m_SeedMapIn = SpatialStack<double, int>(m_Coord_ptr, emptyMapInt);
    m_SeedMapOut = SpatialStack<double, int>(&m_Coord, emptyMapInt);
    m_DispModel = Disp( &m_FGparams, &m_SeedMapIn, &m_SeedMapOut );
    // } else
    // {
    //   m_DispModel = Disp(&m_FGparams, &m_SeedMapIn, &m_SeedMapOut, false);
    // }
  }
  
  /* update disturbances mask if needed */
  if (file_of_params.getMaskDist()[0] != "0")
  {
    logg.info("***** Update disturbances maps...");
    vector< vector< double > > distMap; // disturbances map
    distMap.reserve(m_glob_params.getNoDist());
    for (int dist_id=0; dist_id<m_glob_params.getNoDist(); dist_id++)
    {
      distMap.emplace_back( ReadMask<double>( file_of_params.getMaskDist()[dist_id], 0.0, 1.0 ) );
    }
    m_DistMap = SpatialStack<double, double>(&m_Coord, distMap);
  }
  
  /* update drought disturbances mask if needed */
  if (file_of_params.getMaskDrought() != "0")
  {
    logg.info("***** Update drought disturbances maps...");
    m_DroughtMap = SpatialMap<double, double>(&m_Coord, ReadMask<double>( file_of_params.getMaskDrought(), -5000.0, 1000.0 ) );
    vector< vector< unsigned > > droughtMap; // drought disturbances map
    droughtMap.reserve(m_FGparams.size());
    for (unsigned fg_id=0; fg_id<m_FGparams.size(); fg_id++)
    {
      droughtMap.emplace_back( ReadMask<unsigned>( file_of_params.getMask(), 0.0, 1.0 ) );
    }
    m_PostDroughtMap = SpatialStack<double, unsigned>(&m_Coord, droughtMap);
    m_CountDroughtMap = SpatialStack<double, unsigned>(&m_Coord, droughtMap);
    m_IsDroughtMap = SpatialStack<double, unsigned>(&m_Coord, droughtMap);
    m_ApplyCurrDroughtMap = SpatialStack<double, unsigned>(&m_Coord, droughtMap);
    m_ApplyPostDroughtMap = SpatialStack<double, unsigned>(&m_Coord, droughtMap);
    
    for (vector<int>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
    { // loop on pixels
      for (unsigned fg=0; fg<m_FGparams.size(); fg++)
      { // loop on PFG
        m_PostDroughtMap(*cell_ID, fg) = 0;
        m_CountDroughtMap(*cell_ID, fg) = 0;
      }
    }
  }
  
  /* build simulation fire disturbances masks */
  // if (m_glob_params.getDoFireDisturbances())
  // {
  //   if (m_glob_params.getFireIgnitMode()==5)
  //   {
  //     logg.info("> build simulation fire disturbances masks...");
  //     if (m_glob_params.getNoFireDist() == file_of_params.getMaskFire().size())
  //     {
  //       vector< vector< int > > fireMap; // fire disturbances masks
  //       fireMap.reserve(m_glob_params.getNoFireDist());
  //       for (int dist_id=0; dist_id<m_glob_params.getNoFireDist(); dist_id++)
  //       {
  //         fireMap.emplace_back( ReadMask<int>( file_of_params.getMaskFire()[dist_id], 0.0, 1.0 ) );
  //       }
  //       m_FireMap = SpatialStack<double, int>(m_Coord_ptr, fireMap);
  //     } else
  //     {
  //       logg.error("!!! Parameters FIRE_NO and --FIRE_MASK-- ",
  //                  "do not match in term of number!");
  //     }
  //   } else
  //   {
  //     m_FireMap = SpatialStack<double, int>(m_Coord_ptr, emptyMapInt);
  //   }
  // 
  //   /* build fire masks */
  //   logg.info("> build fire masks...");
  //   if (m_glob_params.getFirePropMode()==4)
  //   {
  //     m_DroughtMap = SpatialMap<double, double>(m_Coord_ptr, ReadMask<double>( file_of_params.getMaskDrought(), 0.0, 1.0 ) );
  //     m_ElevationMap = SpatialMap<double, double>(m_Coord_ptr, ReadMask<double>( file_of_params.getMaskElevation()) );
  //     m_SlopeMap = SpatialMap<double, double>(m_Coord_ptr, ReadMask<double>( file_of_params.getMaskSlope()) );
  //   }
  // 
  //   /* build TSLF mask (study area) */
  //   logg.info("> build TSLF mask (study area)...");
  //   m_TslfMap = SpatialMap<double, int>(m_Coord_ptr, ReadMask<int>( file_of_params.getMask(), 0.0, 1.0 ) );
  // }
  
  /* build aliens introduction masks */
  if (file_of_params.getFGMapsAliens()[0] != "0")
  {
    logg.info("***** Update aliens introduction maps...");
    vector< vector< double > > condInitMap; // aliens introduction masks
    condInitMap.reserve(m_FGparams.size());
    for (unsigned fg=0; fg<m_FGparams.size(); fg++)
    {
      condInitMap.emplace_back( ReadMask<double>( file_of_params.getFGMapsAliens()[fg], 0.0, 1.0 ) );
    }
    m_CondInitMap = SpatialStack<double, double>( &m_Coord, condInitMap );
  }
} // end of UpdateSimulationParameters(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void compressFile(string newFile)
{
  // Compress file
#if defined(__unix__) || defined(__linux__) || defined(linux) || defined(LINUX) || defined(__APPLE__)
  ostringstream ossCompressCommand;
  ossCompressCommand <<  "gzip -9 -f " << newFile;
  string strCompressCommand = ossCompressCommand.str();
  int compress_ok = system(strCompressCommand.c_str());
  if (compress_ok != 0)
  {
    logg.warning("Compression failed for ", newFile);
  }
#else
  std::ifstream ifile(newFile, std::ios_base::in | std::ios_base::binary);
  std::ofstream ofile(newFile+".gz", std::ios_base::out | std::ios_base::binary);
  
  bo::filtering_streambuf<bo::output> out;
  out.push(bo::gzip_compressor());
  out.push(ofile); 
  bo::copy(ifile, out);
  remove(newFile.c_str());
#endif
}

void SimulMap::SaveRasterAbund(string saveDir, int year, string prevFile)
{
  GDALAllRegister();
  
  // Start timer
  time_t start,end;
  time(&start);
  
  // Get output driver (GeoTIFF format).
  const char * driverInput = "GTiff";
  fs::path prevFile_path(prevFile.c_str());
  if (prevFile_path.extension()==".tif"){ driverInput = "GTiff";
  } else if (prevFile_path.extension()==".img"){ driverInput = "HFA";
  } else {
    logg.error("!!! The --MASK-- file extension (", prevFile_path.extension(),
               ") is not taking into account!\n",
               "!!! Please use either .img or .tif files!");
  }
  logg.info("Input & Ouput driver is : ", driverInput, " (extension ",
                                prevFile_path.extension(), ")");
  GDALDriverH outputDriver = GDALGetDriverByName( driverInput );
  CPLAssert( outputDriver != NULL );
  
  // Open the source file.
  GDALDatasetH rasInput = GDALOpen( prevFile.c_str(), GA_ReadOnly );
  CPLAssert( rasInput != NULL );
  
  // Get Source coordinate system.
  const char *inputProjection = GDALGetProjectionRef( rasInput );
  CPLAssert( inputProjection != NULL && strlen(inputProjection) > 0 );
  logg.info("Input & Ouput projection is : ", inputProjection);
  double outputGeoTransform[6];
  GDALGetGeoTransform( rasInput, outputGeoTransform );
  
  // Create output with same datatype as first input band.
  //GDALDataType inputDataType = GDALGetRasterDataType(GDALGetRasterBand(rasInput,1)); //GDT_Byte
  
  if (m_glob_params.getDoSavingPFGStratum() ||
      m_glob_params.getDoSavingPFG() ||
      m_glob_params.getDoSavingStratum())
  {
    logg.info(">>> Saving PFG abund outputs");
    
    if (m_glob_params.getDoSavingPFGStratum() || m_glob_params.getDoSavingPFG())
    {
      /* 1. ABUND PER PFG and PER STRATA */
      logg.info("> Saving abund per PFG & per strata");
      /* 2. ABUND PER PFG for ALL STRATA */
      logg.info("> Saving abund per PFG for all strata");
      // bool positiveVal12 = false;
      
      omp_set_num_threads( m_glob_params.getNoCPU() );
#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
      for (unsigned fg=0; fg<m_FGparams.size(); fg++)
      { // loop on PFG
        //logg.info(">>>>> PFG ", fg);
        vector<int> bkStratAges = m_FGparams[fg].getStrata(); // get strat ages change
        GUInt32 *abunValues2 = new GUInt32[m_Mask.getXncell()*m_Mask.getYncell()];
        for (int pixId=0; pixId<m_Mask.getTotncell(); pixId++)
        {
          abunValues2[pixId] = 0;
        }
        bool positiveVal2 = false;
        for (int strat=1; strat<m_glob_params.getNoStrata()+1; strat++)
        { // loop on Stratum
          //logg.info(">>>>> Stratum ", strat);
          // Calculate abundance values.
          GUInt32 *abunValues1 = new GUInt32[m_Mask.getXncell()*m_Mask.getYncell()];
          for (int pixId=0; pixId<m_Mask.getTotncell(); pixId++)
          {
            abunValues1[pixId] = 0;
          }
          bool positiveVal1 = false;
#pragma omp parallel for ordered
          for (int cell_ID : m_MaskCells)
          { // loop on pixels
            int abundTmp = static_cast<int>(m_SuccModelMap(cell_ID)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund( bkStratAges[strat-1] , bkStratAges[strat] - 1 ));
            abunValues1[cell_ID] = abundTmp;
            abunValues2[cell_ID] += abundTmp;
            if (abundTmp>0)
            {
              positiveVal1 = true;
              positiveVal2 = true;
            }
          } // end loop on pixels
          
          // if (positiveVal1) { positiveVal12 = true; }
          
          if (m_glob_params.getDoSavingPFGStratum() && positiveVal1)
          {
            // Create the output file only if the PFG is present somewhere.
            string newFile = saveDir+"/ABUND_perPFG_perStrata/Abund_YEAR_"+to_string(year)+"_"+m_FGparams[fg].getName()+
              "_STRATA_"+to_string(strat)+prevFile_path.extension().string();
            //GDALDriver * outputDriver = GetGDALDriverManager()->GetDriverByName(driverInput);
            //GDALDataset * rasOutput = outputDriver->Create( newFile.c_str(), m_Mask.getXncell(), m_Mask.getYncell(), m_glob_params.getNoStrata(), GDT_UInt32, NULL );
            GDALDatasetH rasOutput = GDALCreate( outputDriver, newFile.c_str(), m_Mask.getXncell(), m_Mask.getYncell(), 1, GDT_UInt32, NULL );
            CPLAssert( rasOutput != NULL );
            GDALSetProjection( rasOutput, inputProjection ); // Write out the projection definition.
            GDALSetGeoTransform( rasOutput, outputGeoTransform ); // Write out the GeoTransform.
            
            //GDALRasterBand * hBand = rasOutput->GetRasterBand( strat );
            GDALRasterBandH hBand = GDALGetRasterBand( rasOutput, 1 );
            CPLErr rasterAccess = GDALRasterIO(
              hBand, GF_Write, 0, 0, m_Mask.getXncell(), m_Mask.getYncell(),
              abunValues1, m_Mask.getXncell(), m_Mask.getYncell(), GDT_UInt32, 0, 0
            );
            if (rasterAccess > 0)
            {
              logg.warning("Writing ", newFile, " raster: acces status ",
                           rasterAccess);
            }
            GDALClose( rasOutput ); // Once we're done, close properly the dataset
            
            compressFile(newFile);
          }
          delete [] abunValues1;
        } // end loop on Stratum
        
        // if (positiveVal2) { positiveVal12 = true; }
        
        if (m_glob_params.getDoSavingPFG() && positiveVal2)
        {
          // Create the output file only if the PFG is present somewhere.
          string newFile = saveDir+"/ABUND_perPFG_allStrata/Abund_YEAR_"+to_string(year)+"_"+m_FGparams[fg].getName()+"_STRATA_all"+prevFile_path.extension().string();
          GDALDatasetH rasOutput = GDALCreate( outputDriver, newFile.c_str(), m_Mask.getXncell(), m_Mask.getYncell(), 1, GDT_UInt32, NULL );
          CPLAssert( rasOutput != NULL );
          GDALSetProjection( rasOutput, inputProjection ); // Write out the projection definition.
          GDALSetGeoTransform( rasOutput, outputGeoTransform ); // Write out the GeoTransform.
          
          GDALRasterBandH hBand = GDALGetRasterBand( rasOutput, 1 );
          CPLErr rasterAccess = GDALRasterIO(
            hBand, GF_Write, 0, 0, m_Mask.getXncell(), m_Mask.getYncell(),
            abunValues2, m_Mask.getXncell(), m_Mask.getYncell(), GDT_UInt32, 0, 0
          );
          if (rasterAccess > 0)
          {
            logg.warning("Writing ", newFile, " raster: acces status ",
                         rasterAccess);
          }
          GDALClose( rasOutput ); // Once we're done, close properly the dataset
          
          compressFile(newFile);
        }
        delete [] abunValues2;
      } // end loop on PFG
      
      // if (!positiveVal12 && year > m_glob_params.getSeedingDuration())
      // {
      //   logg.error("!!! ALL PFG DIED! Stopping simulation.");
      // }
    }
    
    if (m_glob_params.getDoSavingStratum())
    {
      /* 3. ABUND PER STRATA for ALL PFG */
      logg.info("> Saving abund per strata for all PFG");
      // bool positiveVal33 = false;
      
      for (int strat=1; strat<m_glob_params.getNoStrata()+1; strat++)
      { // loop on Stratum
        // Calculate abundance values.
        GUInt32 *abunValues3 = new GUInt32[m_Mask.getXncell()*m_Mask.getYncell()];
        for (int pixId=0; pixId<m_Mask.getTotncell(); pixId++)
        {
          abunValues3[pixId] = 0;
        }
        bool positiveVal3 = false;
#pragma omp parallel for ordered
        for (int cell_ID : m_MaskCells)
        { // loop on pixels
          int abundTmp = 0;
          for (unsigned fg=0; fg<m_FGparams.size(); fg++)
          { // loop on PFG
            vector<int> bkStratAges = m_FGparams[fg].getStrata(); // get strat ages change
            abundTmp += static_cast<int>(m_SuccModelMap(cell_ID)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund( bkStratAges[strat-1] , bkStratAges[strat] - 1 ));
          }
          abunValues3[cell_ID] = abundTmp;
          if (abundTmp>0)
          {
            positiveVal3 = true;
          }
        } // end loop on pixels
        
        if (positiveVal3)
        {
          // positiveVal33 = true;
          
          // Create the output file only if the PFG is present somewhere.
          string newFile = saveDir+"/ABUND_allPFG_perStrata/Abund_YEAR_"+to_string(year)+"_allPFG_STRATA_"+to_string(strat)+prevFile_path.extension().string();
          GDALDatasetH rasOutput = GDALCreate( outputDriver, newFile.c_str(), m_Mask.getXncell(), m_Mask.getYncell(), 1, GDT_UInt32, NULL );
          CPLAssert( rasOutput != NULL );
          GDALSetProjection( rasOutput, inputProjection ); // Write out the projection definition.
          GDALSetGeoTransform( rasOutput, outputGeoTransform ); // Write out the GeoTransform.
          
          GDALRasterBandH hBand = GDALGetRasterBand( rasOutput, 1 );
          CPLErr rasterAccess = GDALRasterIO(
            hBand, GF_Write, 0, 0, m_Mask.getXncell(), m_Mask.getYncell(),
            abunValues3, m_Mask.getXncell(), m_Mask.getYncell(), GDT_UInt32, 0, 0
          );
          if (rasterAccess > 0)
          {
            logg.warning("Writing ", newFile, " raster: acces status ",
                         rasterAccess);
          }
          GDALClose( rasOutput ); // Once we're done, close properly the dataset
          
          compressFile(newFile);
        }
        delete [] abunValues3;
      } // end loop on Stratum*/
      
      // if (!positiveVal33 && year > m_glob_params.getSeedingDuration())
      // {
      //   logg.error("!!! ALL PFG DIED! Stopping simulation.");
      // }
    }
  }
  
  
  
  if ((m_glob_params.getDoSoilInteraction() && m_glob_params.getSoilSaving()) ||
      (m_glob_params.getDoLightInteraction() && m_glob_params.getLightSaving()) ||
      (m_glob_params.getDoDispersal() && m_glob_params.getDispersalSaving()))
  {
    logg.info(">>> Saving pixel resources outputs");
    
    if (m_glob_params.getDoSoilInteraction() && m_glob_params.getSoilSaving())
    {
      logg.info("> Saving soil values");
      float *soilValues = new float[m_Mask.getXncell()*m_Mask.getYncell()];
      // fill our file pix by pix
      omp_set_num_threads( m_glob_params.getNoCPU() );
#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
      for (int pixId=0; pixId<m_Mask.getTotncell(); pixId++)
      {
        soilValues[pixId] = 0;
      }
      for (int cell_ID : m_MaskCells)
      {
        soilValues[cell_ID] = m_SuccModelMap(cell_ID)->getSoilResources();
      }
      // Create the output file.
      string newFile = saveDir+"/SOIL/Soil_Resources_YEAR_"+to_string(year)+prevFile_path.extension().string();
      GDALDatasetH rasOutput = GDALCreate( outputDriver, newFile.c_str(), m_Mask.getXncell(), m_Mask.getYncell(), 1, GDT_Float32, NULL );
      CPLAssert( rasOutput != NULL );
      
      GDALSetProjection( rasOutput, inputProjection ); // Write out the projection definition.
      double outputGeoTransform[6]; // Write out the GeoTransform.
      GDALGetGeoTransform( rasInput, outputGeoTransform );
      GDALSetGeoTransform( rasOutput, outputGeoTransform );
      
      GDALRasterBandH hBand = GDALGetRasterBand( rasOutput, 1 );
      CPLErr rasterAccess = GDALRasterIO(
        hBand, GF_Write, 0, 0, m_Mask.getXncell(), m_Mask.getYncell(),
        soilValues, m_Mask.getXncell(), m_Mask.getYncell(), GDT_Float32, 0, 0
      );
      if (rasterAccess > 0)
      {
        logg.warning("Writing ", newFile, " raster: acces status ", rasterAccess);
      }
      GDALClose( rasOutput ); // Once we're done, close properly the dataset
      
      delete [] soilValues;
      
      compressFile(newFile);
    }
    
    
    
    if (m_glob_params.getDoLightInteraction() && m_glob_params.getLightSaving())
    {
      logg.info("> Saving light values");
      
      for (int strat=0; strat<m_glob_params.getNoStrata(); strat++)
      { // loop on Stratum
        // Calculate light values.
        GUInt16 *lightValues = new GUInt16[m_Mask.getXncell()*m_Mask.getYncell()];
        // fill our file pix by pix
        omp_set_num_threads( m_glob_params.getNoCPU() );
#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
        for (int pixId=0; pixId<m_Mask.getTotncell(); pixId++)
        {
          lightValues[pixId] = 0;
        }
        for (int cell_ID : m_MaskCells)
        {
          lightValues[cell_ID] = ResourceToDouble(m_SuccModelMap(cell_ID)->getLightResources().getResource(strat));
        }
        
        // Create the output file.
        string newFile = saveDir+"/LIGHT/Light_Resources_YEAR_"+to_string(year)+
          "_STRATA_"+to_string(strat+1)+prevFile_path.extension().string();
        GDALDatasetH rasOutput = GDALCreate( outputDriver, newFile.c_str(), m_Mask.getXncell(), m_Mask.getYncell(), 1, GDT_UInt16, NULL );
        CPLAssert( rasOutput != NULL );
        
        GDALSetProjection( rasOutput, inputProjection ); // Write out the projection definition.
        double outputGeoTransform[6]; // Write out the GeoTransform.
        GDALGetGeoTransform( rasInput, outputGeoTransform );
        GDALSetGeoTransform( rasOutput, outputGeoTransform );
        
        GDALRasterBandH hBand = GDALGetRasterBand( rasOutput, 1 );
        CPLErr rasterAccess = GDALRasterIO(
          hBand, GF_Write, 0, 0, m_Mask.getXncell(), m_Mask.getYncell(),
          lightValues, m_Mask.getXncell(), m_Mask.getYncell(), GDT_UInt16, 0, 0
        );
        if (rasterAccess > 0)
        {
          logg.warning("Writing ", newFile, " raster: acces status ", rasterAccess);
        }
        GDALClose( rasOutput ); // Once we're done, close properly the dataset
        
        delete [] lightValues;
        
        compressFile(newFile);
      }
    }
    
    
    
    if (m_glob_params.getDoDispersal() && m_glob_params.getDispersalSaving())
    {
      logg.info(">>> Saving seeds after dispersal");
      for (unsigned fg=0; fg<m_FGparams.size(); fg++)
      { // loop on PFG
        float *seedValues = new float[m_Mask.getXncell()*m_Mask.getYncell()];
        
        // fill our file pix by pix
        omp_set_num_threads( m_glob_params.getNoCPU() );
#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
        for (int pixId=0; pixId<m_Mask.getTotncell(); pixId++)
        {
          seedValues[pixId] = 0;
        }
        for (int cell_ID : m_MaskCells)
        {
          seedValues[cell_ID] = m_SeedMapOut(cell_ID, fg);
        }
        
        // Create the output file only if the PFG is present somewhere.
        string newFile = saveDir+"/DISPERSAL/Dispersal_YEAR_"+to_string(year)+"_"+m_FGparams[fg].getName()+prevFile_path.extension().string();
        GDALDatasetH rasOutput = GDALCreate( outputDriver, newFile.c_str(), m_Mask.getXncell(), m_Mask.getYncell(), 1, GDT_Float32, NULL );
        CPLAssert( rasOutput != NULL );
        
        GDALSetProjection( rasOutput, inputProjection ); // Write out the projection definition.
        double outputGeoTransform[6]; // Write out the GeoTransform.
        GDALGetGeoTransform( rasInput, outputGeoTransform );
        GDALSetGeoTransform( rasOutput, outputGeoTransform );
        
        GDALRasterBandH hBand = GDALGetRasterBand( rasOutput, 1 );
        CPLErr rasterAccess = GDALRasterIO(
          hBand, GF_Write, 0, 0, m_Mask.getXncell(), m_Mask.getYncell(),
          seedValues, m_Mask.getXncell(), m_Mask.getYncell(), GDT_Float32, 0, 0
        );
        if (rasterAccess > 0)
        {
          logg.warning("Writing ", newFile, " raster: acces status ",
                       rasterAccess);
        }
        GDALClose( rasOutput ); // Once we're done, close properly the dataset
        
        delete [] seedValues;
        
        compressFile(newFile);
      } // end loop on PFG
    }
  }
  
  GDALClose( rasInput );
  
  // Print timer
  time(&end);
  logg.info("> Saving stuff took ", difftime (end,start), " s");
}
