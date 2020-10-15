
#include "SimulMap.h"

#include <iostream>
#include <cstring>
#include <fstream>
#include <cstdio>
#include <numeric>
#include <chrono>
#include <random>

#include <boost/math/distributions/normal.hpp>
#include <boost/random.hpp>

#include "gdal_priv.h" // to read raster files
#include "gdal.h"
#include "cpl_conv.h"

using namespace std;

// boost::normal_distribution<double> génère une distribution normale
// boost::mt19937 est un Mersenne twister generator, ou générateur de nombres pseudo-aléatoires
// boost::uniform_01<RandomGenerator> génère une distribution aléatoire uniforme
// boost::variate_generator<RandomGenerator&, Normal> est un bivariate generator (générateur MT19937+distribution standard uniforme)

typedef boost::normal_distribution<double> Normal;
typedef boost::mt19937 RandomGenerator;
typedef boost::uniform_01<RandomGenerator&> Uni01;
typedef boost::uniform_real<double> UniReal;
typedef boost::uniform_int<int> UniInt;
typedef boost::variate_generator<RandomGenerator&, Normal> GeneratorNorm;
typedef boost::variate_generator<RandomGenerator&, UniReal> GeneratorUniReal;
typedef boost::variate_generator<RandomGenerator&, UniInt> GeneratorUniInt;

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
											 m_DistMap(SpatialStack<double, int>()),
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


	/* build functional groups entities */
	logg.info("*** building Functional groups...");
	if (noFG!=(int)file_of_params.getFGLifeHistory().size())
	{
		logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_LIFE_HISTORY-- ",
							 "do not match in term of number!");
	}
	if (m_glob_params.getDoLightInteraction() &&
			noFG!=(int)file_of_params.getFGLight().size())
	{
		logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_LIGHT-- ",
							 "do not match in term of number!");
	}
	if (m_glob_params.getDoHabSuitability() &&
			noFG!=(int)file_of_params.getFGMapsHabSuit().size())
	{
		logg.error("!!! Parameters NO_PFG and --PFG_MASK_HABSUIT-- ",
							 "do not match in term of number!");
	}
	if (m_glob_params.getDoDispersal() &&
			noFG!=(int)file_of_params.getFGDispersal().size())
	{
		logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_DISPERSAL-- ",
							 "do not match in term of number!");
	}
	if (m_glob_params.getDoDisturbances() &&
			noFG!=(int)file_of_params.getFGDisturbance().size())
	{
		logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_DISTURBANCES-- ",
							 "do not match in term of number!");
	}
	if (m_glob_params.getDoSoilInteraction() &&
			noFG!=(int)file_of_params.getFGSoil().size())
	{
		logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_SOIL-- ",
							 "do not match in term of number!");
	}
	if (m_glob_params.getDoFireDisturbances() &&
			noFG!=(int)file_of_params.getFGFire().size())
	{
		logg.error("!!! Parameters NO_PFG and --PFG_PARAMS_FIRE-- ",
							 "do not match in term of number!");
	}
	if (m_glob_params.getDoDroughtDisturbances()
			&& noFG!=(int)file_of_params.getFGDrought().size())
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
	for (unsigned cell_ID=0; cell_ID<m_Mask.getTotncell(); cell_ID++)
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
		if (m_glob_params.getNoDist()==(int)file_of_params.getMaskDist().size())
		{
			vector< vector< int > > distMap; // disturbances masks
			distMap.reserve(noFG);
			for (int dist_id=0; dist_id<m_glob_params.getNoDist(); dist_id++)
			{
				distMap.emplace_back( ReadMask<int>( file_of_params.getMaskDist()[dist_id], 0.0, 1.0 ) );
			}
			m_DistMap = SpatialStack<double, int>(m_Coord_ptr, distMap);
		} else
		{
			logg.error("!!! Parameters DIST_NO and --DIST_MASK-- ",
								 "do not match in term of number!");
		}
	} else
	{
		m_DistMap = SpatialStack<double, int>(m_Coord_ptr, emptyMapInt);
	}

	/* build simulation fire disturbances masks */
	if (m_glob_params.getDoFireDisturbances())
	{
		if (m_glob_params.getFireIgnitMode()==5)
		{
			logg.info("> build simulation fire disturbances masks...");
			if (m_glob_params.getNoFireDist()==(int)file_of_params.getMaskFire().size())
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

		for (vector<unsigned>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
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
		if (noFG==(int)file_of_params.getFGMapsAliens().size())
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

	/* create a succession model within each pixel */
	logg.info("> create a succession model within each pixel...");
	vector< SuFatePtr > succModel_ptr_list; // vector of ptr on a succession model
	succModel_ptr_list.reserve(m_Mask.getTotncell());
	for (unsigned i=0; i<m_Mask.getTotncell(); i++)
	{
		SuFatePtr succModel_ptr; // ptr on succession model
		if (m_glob_params.getDoHabSuitability() == false)
		{ // FATE succession model
			succModel_ptr = new SuFate(i, Comm_std, lr_std, m_glob_params.getSoilInit(), m_SeedMapOut_ptr, m_SeedMapIn_ptr, m_glob_params_ptr);
		} else if (m_glob_params.getDoHabSuitability() == true)
		{ // FATEH succession model
			succModel_ptr = new SuFateH(i, Comm_std, lr_std, m_glob_params.getSoilInit(), m_SeedMapOut_ptr, m_SeedMapIn_ptr, m_glob_params_ptr, &m_EnvSuitMap, &m_EnvSuitRefMap );
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
	for (unsigned i=0; i<m_Mask.getTotncell(); i++)
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
vector<unsigned>& SimulMap::getMaskCells() { return m_MaskCells; }
SpatialStack<double, int>& SimulMap::getSeedMapIn() { return m_SeedMapIn; }
SpatialStack<double, int>& SimulMap::getSeedMapOut() { return m_SeedMapOut; }
SpatialStack<double, double>&  SimulMap::getEnvSuitMap() { return m_EnvSuitMap ; }
SpatialStack<double, double>&  SimulMap::getEnvSuitRefMap() { return m_EnvSuitRefMap; }
SpatialStack<double, int>&  SimulMap::getDistMap() { return m_DistMap; }
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
void SimulMap::setMaskCells(vector<unsigned> maskCells) { m_MaskCells = maskCells; }
void SimulMap::setSeedMapIn(SpatialStack<double, int> seedMapIn) { m_SeedMapIn = seedMapIn; }
void SimulMap::setSeedMapOut(SpatialStack<double, int> seedMapOut) { m_SeedMapOut = seedMapOut; }
void SimulMap::setEnvSuitMap(SpatialStack<double, double> envSuitMap) { m_EnvSuitMap = envSuitMap; }
void SimulMap::setEnvSuitRefMap(SpatialStack<double, double> envSuitRefMap) { m_EnvSuitRefMap = envSuitRefMap; }
void SimulMap::setDistMap(SpatialStack<double, int> distMap) { m_DistMap = distMap; }
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
		m_FGparams[fg].setDispersed(true);
	}
} // end of StartSeeding()

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::StopSeeding()
{
	for (unsigned fg=0; fg<m_FGparams.size(); fg++)
	{
		m_FGparams[fg].setDispersed(false);
	}
} // end of StopSeeding()

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::DoFileChange(string newChangeFile, string typeFile)
{
	logg.info("Try to get change parameters :\nFrom file ", newChangeFile, " (",
						typeFile, ")");

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
		if ((strcmp(typeFile.c_str(),"habSuit")==0) | (strcmp(typeFile.c_str(),"drought")==0) | (strcmp(typeFile.c_str(),"aliens")==0))
		{
			vector< vector<double> > newMaps; // DOUBLE VALUES
			newMaps.reserve(noFiles);
			for (unsigned file_id=0; file_id<noFiles; file_id++)
			{
				if (strcmp(typeFile.c_str(),"habSuit")==0 || strcmp(typeFile.c_str(),"aliens")==0){
					newMaps.emplace_back( ReadMask<double>( newNameFiles[file_id], 0.0, 1.0 ) );
				} else if (strcmp(typeFile.c_str(),"drought")==0){
					newMaps.emplace_back( ReadMask<double>( newNameFiles[file_id], -5000.0, 1000.0 ) );
				}
			}
			if (strcmp(typeFile.c_str(),"habSuit")==0){ setEnvSuitMap(SpatialStack<double, double>( &m_Coord, newMaps));
			} else if (strcmp(typeFile.c_str(),"drought")==0){ setDroughtMap(SpatialMap<double, double>( &m_Coord, newMaps[0]));
			} else if (strcmp(typeFile.c_str(),"aliens")==0){ setCondInitMap(SpatialStack<double, double>( &m_Coord, newMaps));
			}
		} else if ((strcmp(typeFile.c_str(),"mask")==0) | (strcmp(typeFile.c_str(),"dist")==0) | (strcmp(typeFile.c_str(),"fire")==0))
		{
			vector< vector<int> > newMaps; // INTEGER VALUES
			newMaps.reserve(noFiles);
			for (unsigned file_id=0; file_id<noFiles; file_id++)
			{
				newMaps.emplace_back( ReadMask<int>( newNameFiles[file_id], 0.0, 1.0 ) );
			}
			if (strcmp(typeFile.c_str(),"mask")==0)
			{
				setMask(SpatialMap<double, int>( &m_Coord, newMaps[0]));

				/* If studied area changed, change also the ids of used cells */
				vector<unsigned> newMaskCells;
				newMaskCells.reserve(m_Mask.getTotncell());
				for (unsigned cell_ID=0; cell_ID<m_Mask.getTotncell(); cell_ID++)
				{
					if (m_Mask(cell_ID) == 1)
					{
						newMaskCells.emplace_back(cell_ID);
					}
				}
				newMaskCells.shrink_to_fit();
				setMaskCells(newMaskCells);
			} else if(strcmp(typeFile.c_str(),"dist")==0){ setDistMap(SpatialStack<double, int>( &m_Coord, newMaps));
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

		if (strcmp(typeFile.c_str(),"fire")==0){ m_glob_params.setFreqFireDist(newfreq);
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

void SimulMap::DoSuccession()
{
/*	time_t start,end;
	time(&start);*/

	vector <vector<unsigned> > isDrought(m_Mask.getTotncell(),vector<unsigned>(m_glob_params.getNoFG(),0));
	if (m_glob_params.getDoDroughtDisturbances())
	{
		for (vector<unsigned>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
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
	for (unsigned ID=0; ID<m_MaskCells.size(); ID++)
	{
		unsigned cell_ID = m_MaskCells[ID];
		m_SuccModelMap(cell_ID)->DoSuccessionPart1(isDrought[cell_ID]);
	}
	if (m_glob_params.getDoHabSuitability())
	{
		/* Defined the new environmental reference value for next year */
		this->UpdateEnvSuitRefMap(m_glob_params.getHabSuitMode());
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

	for (unsigned ID=0; ID<m_MaskCells.size(); ID++)
	{
		unsigned cell_ID = m_MaskCells[ID];
		for (int fg = 0; fg<m_glob_params.getNoFG(); fg++)
		{
			if (applyIntro[fg] && m_CondInitMap(cell_ID, fg)>0.0)
			{
				m_SuccModelMap(cell_ID)->setSeedRain(fg, int(m_SuccModelMap(cell_ID)->getSeedRain(fg)+(int)(100*m_CondInitMap(cell_ID, fg))));
			}
		} //end loop on PFGs
	} // end loop on cells
} // end of DoAliensIntroduction(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::DoDispersal()
{
	m_SeedMapOut.emptyStack();
	m_DispModel.DoDispersalPacket(m_glob_params.getDispersalMode(), m_glob_params.getNoCPU(), m_MaskCells);
	m_SeedMapIn.emptyStack();
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

vector<unsigned int> SimulMap::DoIgnition(int dist, vector<unsigned int> availCells)
{
	unsigned seed = chrono::system_clock::now().time_since_epoch().count();
	RandomGenerator rng(seed);
	Uni01 random_01(rng);

	int noFires = m_glob_params.getFireIgnitNo()[dist];

	/* No of starting fires : normal distribution */
	if (m_glob_params.getFireIgnitMode()==2)
	{
		Normal distrib(noFires,noFires/10+1);
		GeneratorNorm draw_from_distrib(rng,distrib);
		noFires = draw_from_distrib();
	}
	/* No of starting fires : previous data distribution */
	if (m_glob_params.getFireIgnitMode()==3)
	{
		UniInt distrib(0,m_glob_params.getFireIgnitNoHist().size()-1);
		GeneratorUniInt draw_from_distrib(rng,distrib);
		noFires = m_glob_params.getFireIgnitNoHist()[draw_from_distrib()];
	}

	vector<unsigned> startCell;

	/* Randomly distributed over the landscape */
	if (m_glob_params.getFireIgnitMode()==1 || m_glob_params.getFireIgnitMode()==2 || m_glob_params.getFireIgnitMode()==3)
	{
		UniInt distrib(0,availCells.size()-1);
		GeneratorUniInt draw_from_distrib(rng,distrib);
		for (int n=0; n<noFires; n++)
		{
			startCell.push_back(availCells[draw_from_distrib()]); //rand() % availCells.size();
		}
	} else if (m_glob_params.getFireIgnitMode()==4) /* ChaoLi probability adaptation */
	{
		for (vector<unsigned>::iterator cell_ID=availCells.begin(); cell_ID!=availCells.end(); ++cell_ID)
		{
			/* Baseline proba */
			double probBL = m_glob_params.getFireIgnitLogis()[0] / (1+exp(m_glob_params.getFireIgnitLogis()[1]-m_glob_params.getFireIgnitLogis()[2]*m_TslfMap(*cell_ID)));
			/* Fuel proba */
			unsigned abundTmpTot=0;
			vector<unsigned> abundTmpFG;
			for (unsigned fg=0; fg<m_FGparams.size(); fg++)
			{
				unsigned abundTmp = m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund();
				abundTmpTot += abundTmp;
				abundTmpFG.push_back(abundTmp);
			}
			double probFuel = 0;
			for (unsigned fg=0; fg<m_FGparams.size(); fg++)
			{
				if (abundTmpTot>0)
				{
					probFuel += (m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getFlamm() / m_glob_params.getFireIgnitFlammMax())* (abundTmpFG[fg]/abundTmpTot);
				}
			}
			/* Drought proba */
			double probDrought = (-1.0) * m_DroughtMap(*cell_ID);

			if (random_01() < probBL*probFuel*probDrought)
			{ //(rand()/(double)RAND_MAX)
				startCell.push_back(*cell_ID);
			}
		}
	}
	return(startCell);
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

vector<unsigned int> SimulMap::DoPropagation(int dist, vector<unsigned int> start, vector<unsigned int> availCells)
{
	unsigned seed = chrono::system_clock::now().time_since_epoch().count();
	RandomGenerator rng(seed);
	Uni01 random_01(rng);

	vector<unsigned int> preCell, currCell, postCell, neighCell;
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
		for (vector<unsigned>::iterator it1=currCell.begin(); it1!=currCell.end(); ++it1)
		{
			/* Get the IDs of the 8 neighbour cells */
			for (int xx=-1; xx<=1; xx++)
			{
				for (int yy=-1; yy<=1; yy++)
				{
					unsigned id = *it1+xx*m_Mask.getYncell()+yy;
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

			/* FIRST CASE : fire spread depends on a probability of the current burning cell */
			/* fireIntensity */
			if (m_glob_params.getFirePropMode()==1)
			{
				prob = m_glob_params.getFirePropIntensity()[dist];
			}
			/* percentConsumed : How much stuff was consumed in the current cell ? */
			else if (m_glob_params.getFirePropMode()==2)
			{
				unsigned abundTmpTot = 0;
				vector<unsigned> abundTmpFG;
				vector<double> propKillFG;

				for (unsigned fg=0; fg<m_FGparams.size(); fg++)
				{ // loop on FG
					unsigned abundTmp = m_SuccModelMap(*it1)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund();
					abundTmpTot += abundTmp;
					abundTmpFG.push_back(abundTmp);

					double propKill = 0.0;
					vector<vector< vector<Fract> > > fates = m_SuccModelMap(*it1)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getFireResponse().getFates();
					for (unsigned sub=0; sub<fates[dist].size(); sub++)
					{
						propKill += FractToDouble(fates[dist][sub][Kill]);
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
			}

			/* For each neighbour cell : does the fire propagate ? */
			if (m_glob_params.getFirePropMode()==1 || m_glob_params.getFirePropMode()==2)
			{
				for (vector<unsigned>::iterator it2=neighCell.begin(); it2!=neighCell.end(); ++it2)
				{
					if (find(postCell.begin(),postCell.end(),*it2)==postCell.end() && find(preCell.begin(),preCell.end(),*it2)==preCell.end() && random_01() < prob)
					{ //(rand()/(double)RAND_MAX)
						preCell.push_back(*it2);
						if (m_glob_params.getFireQuotaMode()==2 /* "maxConsume" */){ stepCount += prob; }
					}
				}
			}

			/* SECOND CASE : fire spread depends on a probability of the 8 neighboring cells of the current burning cell */
			if (m_glob_params.getFirePropMode()==3 /* "maxAmountFuel" */)
			{
				vector<unsigned> abundTmp;
				for (vector<unsigned>::iterator it2=neighCell.begin(); it2!=neighCell.end(); ++it2)
				{
					unsigned abund = 0;
					for (unsigned fg=0; fg<m_FGparams.size(); fg++)
					{
						abund += m_SuccModelMap(*it2)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund() * m_SuccModelMap(*it2)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getFlamm();
					}
					abundTmp.push_back(abund);
				}
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

			} else if(m_glob_params.getFirePropMode()==4 /* "maxAmountSoil" */)
			{
				vector<double> soilTmp;
				for (vector<unsigned>::iterator it2=neighCell.begin(); it2!=neighCell.end(); ++it2)
				{
					soilTmp.push_back(m_SuccModelMap(*it2)->getSoilResources());
				}
				if (accumulate(soilTmp.begin(),soilTmp.end(),0)>0)
				{
					unsigned maxCell = neighCell[distance(soilTmp.begin(),max_element(soilTmp.begin(),soilTmp.end()))];
					if (find(postCell.begin(),postCell.end(),maxCell)==postCell.end() && find(preCell.begin(),preCell.end(),maxCell)==preCell.end())
					{
						preCell.push_back(maxCell);
					}
				}
			} else if(m_glob_params.getFirePropMode()==5 /* "probLandClim" */)
			{
				for (vector<unsigned>::iterator it2=neighCell.begin(); it2!=neighCell.end(); ++it2)
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
						for (unsigned fg=0; fg<m_FGparams.size(); fg++)
						{
							if (abundTmpTot>0)
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

						if ( random_01() < probBL*probFuel*probDrought*probSlope)
						{ //(rand()/(double)RAND_MAX)
							preCell.push_back(*it2);
						}
					}
				}
			}
			neighCell.clear();
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
	}

	/* if you want to stop the fires only when you reach the quota (maxConsume & maxCell) */
/*	if (stepCount<lim)
	{
		vector<unsigned int> newAvailCell = m_MaskCells;
		vector<unsigned>::iterator it;
		// erase cells that have already burnt
		for (vector<unsigned>::iterator cell_ID=postCell.begin(); cell_ID!=postCell.end(); ++cell_ID)
		{
			it = find(newAvailCell.begin(),newAvailCell.end(),*cell_ID);
			newAvailCell.erase(it);
		}
		vector<unsigned int> newStartCell = DoIgnition(dist,newAvailCell);
		vector<unsigned int> newBurntCell = DoPropagation(dist,newStartCell,newAvailCell);
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

void SimulMap::DoUpdateTslf(vector<unsigned int> burnt)
{
	for (vector<unsigned>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
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
	unsigned seed = chrono::system_clock::now().time_since_epoch().count();
	RandomGenerator rng(seed);

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
	vector< vector<unsigned int> > ALLburntCell(m_glob_params.getNoFireDist());
	for (int dist=0; dist<m_glob_params.getNoFireDist(); dist++)
	{
		if (applyDist[dist] && m_glob_params.getFireIgnitMode()!=5 /* map */)
		{
			vector<unsigned int> startCell = DoIgnition(dist,m_MaskCells);
			vector<unsigned int> burntCell;
			if (m_glob_params.getFireNeighMode()==1)
			{
				burntCell = DoPropagation(dist,startCell,m_MaskCells);
			} else
			{
				for (vector<unsigned>::iterator it1=startCell.begin(); it1!=startCell.end(); ++it1)
				{
					int no = m_glob_params.getFireNeighCC()[0];
					int ea = m_glob_params.getFireNeighCC()[1];
					int so = m_glob_params.getFireNeighCC()[2];
					int we = m_glob_params.getFireNeighCC()[3];
					if (m_glob_params.getFireNeighMode()==3 /* "extentRand" */)
					{
						UniInt distrib_no(0,no);
						UniInt distrib_ea(0,ea);
						UniInt distrib_we(0,we);
						UniInt distrib_so(0,so);
						GeneratorUniInt draw_from_distrib_no(rng,distrib_no);
						GeneratorUniInt draw_from_distrib_ea(rng,distrib_ea);
						GeneratorUniInt draw_from_distrib_we(rng,distrib_we);
						GeneratorUniInt draw_from_distrib_so(rng,distrib_so);
						no = draw_from_distrib_no(); //rand() % no + 1;
						ea = draw_from_distrib_ea(); //rand() % ea + 1;
						we = draw_from_distrib_we(); //rand() % we + 1;
						so = draw_from_distrib_so(); //rand() % so + 1;
					}
					for (int yy=(-no); yy<=so; yy++)
					{
						for (int xx=(-we); xx<=ea; xx++)
						{
							unsigned id = *it1+yy+xx*m_Mask.getYncell();
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
			}
			ALLburntCell[dist] = burntCell;
		} else if (applyDist[dist] && m_glob_params.getFireIgnitMode()==5 /* map */)
		{
			for (vector<unsigned>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
			{
				if (m_FireMap(*cell_ID, dist) == 1 )
				{
					ALLburntCell[dist].push_back(*cell_ID);
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
					vector<unsigned>::iterator it = find(ALLburntCell[dist1].begin(),ALLburntCell[dist1].end(),ALLburntCell[dist2][pos]);
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
			for (vector<unsigned>::iterator cell_ID=ALLburntCell[dist].begin(); cell_ID!=ALLburntCell[dist].end(); ++cell_ID)
			{
				for (unsigned fg=0; fg<m_FGparams.size(); fg++)
				{ // loop on PFG
					m_SuccModelMap(*cell_ID)->DoDisturbance(fg,dist,m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getFireResponse());
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

	// omp_set_num_threads( m_glob_params.getNoCPU() );
	// #pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
	for (vector<unsigned>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
	{ // loop on pixels
		vector<int> tmpAbund(noStrata,0);
		for (unsigned fg=0; fg<m_FGparams.size(); fg++)
		{ // loop on PFG
			vector<int> strAgeChange = m_FGparams[fg].getStrata(); // get stratum changing ages

			// #pragma omp parallel for ordered
			for (unsigned strat=1; strat<noStrata; strat++)
			{ // loop on Stratum
				tmpAbund[strat-1] += (int)(m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund( strAgeChange[strat-1] , strAgeChange[strat] - 1 ));
			} // end loop on Stratum
		} // end loop on PFG

		/* Calculation of canopy closure : 0 = no canopy, 1 = full closure */
		double pixAbund = *max_element(tmpAbund.begin()+1, tmpAbund.end());
		double maxVal = m_glob_params.getMaxAbundHigh() * m_FGparams.size(); //7000.0;
		if (pixAbund>maxVal) pixAbund = maxVal;
		pixAbund = pixAbund/maxVal;
		if (pixAbund>0.5){ moistValues(*cell_ID) = m_DroughtMap(*cell_ID) + abs(m_DroughtMap(*cell_ID))/2.0; }
	}

	/* Do disturbances only on points within mask */
	for (vector<unsigned>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
	{ // loop on pixels
		for (unsigned fg=0; fg<m_FGparams.size(); fg++)
		{ // loop on PFG
			m_IsDroughtMap(*cell_ID, fg) = 0;
			m_ApplyPostDroughtMap(*cell_ID, fg) = 0;
			m_ApplyCurrDroughtMap(*cell_ID, fg) = 1;

			if (m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund() > 0)
			{
				/* 0.Check Habitat Suitability */
				// set recruit and fecund to 0 ?
				// automatic at the beginning of DoSuccession

				/* 2.Check Post Drought Mortality */
				if (m_PostDroughtMap(*cell_ID, fg)==1)
				{
					/* Set recruitment and fecundity to 0 */
					m_IsDroughtMap(*cell_ID, fg) = 1;
					m_ApplyPostDroughtMap(*cell_ID, fg) = 1;
				}

				/* 1.Check Moisture Index */
				double moistIndex = moistValues(*cell_ID);
				if (moistIndex>m_FGparams[fg].getDroughtSD()[0])
				{
					/* 3.If NO drought : apply Drought Recovery */
					m_CountDroughtMap(*cell_ID, fg) -= m_FGparams[fg].getDroughtRecovery();
					if (m_CountDroughtMap(*cell_ID, fg)<0)
					{
						m_CountDroughtMap(*cell_ID, fg) = 0;
					}
				} else
				{
					/* Set recruitment and fecundity to 0 */
					m_IsDroughtMap(*cell_ID, fg) = 1;

					if (m_CountDroughtMap(*cell_ID, fg)<m_FGparams[fg].getCountModToSev()) { m_CountDroughtMap(*cell_ID, fg) ++; }
					bool currSevDrought = false, currModDrought = false;
					if (moistIndex<m_FGparams[fg].getDroughtSD()[1])
					{
						currSevDrought = true;
					} else
					{
						currModDrought = true;
					}
					if (currSevDrought && m_CountDroughtMap(*cell_ID, fg)==1)
					{
						m_PostDroughtMap(*cell_ID, fg) = 1;
					}
					bool modToSev = (currModDrought && (m_CountDroughtMap(*cell_ID, fg)==m_FGparams[fg].getCountModToSev()));
					bool SevMort = (currSevDrought && (m_CountDroughtMap(*cell_ID, fg)==m_FGparams[fg].getCountSevMort()));

					/* 4.If drought : check Current Drought Mortality */
					if (modToSev || SevMort )
					{
						m_PostDroughtMap(*cell_ID, fg) = 1;
						m_ApplyCurrDroughtMap(*cell_ID, fg) = 1;
					}
				}
			} else
			{
				/* If NO PFG anymore : reset count */
				m_CountDroughtMap(*cell_ID, fg) = 0;
			}
		}
	}
} // end of DoDroughtDisturbance(...)


void SimulMap::DoDroughtDisturbance_part2(string chrono)
{
	for (vector<unsigned>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
	{ // loop on pixels
		for (unsigned fg=0; fg<m_FGparams.size(); fg++)
		{ // loop on PFG
			if ((m_ApplyPostDroughtMap(*cell_ID, fg)==1) && (m_ApplyCurrDroughtMap(*cell_ID, fg)==0))
			{ /* Apply post drought effects */
				if (strcmp(chrono.c_str(),m_glob_params.getChronoPost().c_str())==0)
				{
					//logg.info(">> Post drought effect this year !");
					m_SuccModelMap(*cell_ID)->DoDisturbance(fg,1,m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getDroughtResponse());
					//m_SuccModelMap(*cell_ID)->DoDisturbance(1,"drought");
				}
			} else if ((m_ApplyCurrDroughtMap(*cell_ID, fg)==1) && (m_ApplyPostDroughtMap(*cell_ID, fg)==0))
			{ /* Apply current drought effects */
				if (strcmp(chrono.c_str(),m_glob_params.getChronoCurr().c_str())==0)
				{
					//logg.info(">> Current drought effect this year !");
					m_SuccModelMap(*cell_ID)->DoDisturbance(fg,0,m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getDroughtResponse());
					//m_SuccModelMap(*cell_ID)->DoDisturbance(0,"drought");
				}
			} else if ((m_ApplyCurrDroughtMap(*cell_ID, fg)==1) && (m_ApplyPostDroughtMap(*cell_ID, fg)==1))
			{ /* Apply cumulated post-current drought effects */
				if (strcmp(chrono.c_str(),m_glob_params.getChronoCurr().c_str())==0)
				{
					//logg.info(">> Current+Post drought effect this year !");
					FGresponse CurrPostResp = m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getDroughtResponse();
					vector<vector<int> > tmpBreakAge = CurrPostResp.getBreakAge(), tmpResprAge = CurrPostResp.getResprAge();
					vector<Fract> tmpDormBreaks = CurrPostResp.getDormBreaks();
					vector<Fract> tmpPropKilled = CurrPostResp.getPropKilled();
					vector<vector<vector<Fract> > > tmpFates = CurrPostResp.getFates();

					tmpBreakAge.push_back(tmpBreakAge[0]);
					tmpResprAge.push_back(tmpResprAge[0]);
					tmpDormBreaks.push_back(tmpDormBreaks[0]);
					if (FractToDouble(tmpPropKilled[0])==0.0)
					{
						tmpPropKilled.push_back(DoubleToFract(0.1));
					} else
					{
						tmpPropKilled.push_back(DoubleToFract(FractToDouble(tmpPropKilled[0])*1.1));
					}
					vector<vector<Fract> > tmpNewFates(tmpFates[0].size());
					for (unsigned sub=0; sub<tmpFates[0].size(); sub++)
					{ // loop on subdist
						vector<Fract> tmpKiUnRe = tmpFates[0][sub];
						if (FractToDouble(tmpFates[0][sub][0])!=1)
						{ // already 100% killed
							double mortSup = 0.0;
							if (FractToDouble(tmpFates[0][sub][0])==0)
							{ // no killed
								mortSup = 0.1*m_CountDroughtMap(*cell_ID, fg);
							} else
							{
								mortSup = FractToDouble(tmpFates[0][sub][0])*0.1*m_CountDroughtMap(*cell_ID, fg);
							}
							tmpKiUnRe[0] = DoubleToFract(FractToDouble(tmpFates[0][sub][0])+mortSup);
							if (FractToDouble(tmpFates[0][sub][1])==0)
							{ // no unaffected
								tmpKiUnRe[1] = DoubleToFract(0.5*(FractToDouble(tmpKiUnRe[0]) - FractToDouble(tmpFates[0][sub][0])));
							} else if (FractToDouble(tmpFates[0][sub][2])==0)
							{ // no resprouting
								tmpKiUnRe[1] = DoubleToFract(FractToDouble(tmpFates[0][sub][1]) - 1.5*(FractToDouble(tmpKiUnRe[0]) - FractToDouble(tmpFates[0][sub][0])));
							} else
							{ // resprouting and unaffected
								tmpKiUnRe[1] = DoubleToFract(FractToDouble(tmpFates[0][sub][1]) - 0.5*(FractToDouble(tmpKiUnRe[0]) - FractToDouble(tmpFates[0][sub][0])));
							}
							tmpKiUnRe[2] = getLeavingFract( tmpKiUnRe[0], tmpKiUnRe[1] );
						}
						tmpNewFates[sub] = tmpKiUnRe;
					}
					tmpFates.push_back(tmpNewFates);
					CurrPostResp.setBreakAge(tmpBreakAge);
					CurrPostResp.setResprAge(tmpResprAge);
					CurrPostResp.setDormBreaks(tmpDormBreaks);
					CurrPostResp.setPropKilled(tmpPropKilled);
					CurrPostResp.setFates(tmpFates);
					m_SuccModelMap(*cell_ID)->DoDisturbance(fg,0,m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getDroughtResponse());
				}
			}
		}
	}
} // end of DoDroughtDisturbance(...)


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::DoDisturbance(int yr)
{
	/* Do disturbances depending on their frequency */
	vector<bool> applyDist;
	for (vector<int>::const_iterator it=m_glob_params.getFreqDist().begin(); it!=m_glob_params.getFreqDist().end(); ++it)
	{
		if (*it==1) { applyDist.push_back(true);
		} else if (yr%(*it)==0) { applyDist.push_back(true);
		} else { applyDist.push_back(false);
		}
	}

	/* Do disturbances only on points within mask */
	omp_set_num_threads( m_glob_params.getNoCPU() );
	#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)

	for (unsigned ID=0; ID<m_MaskCells.size(); ID++)
	{
		unsigned cell_ID = m_MaskCells[ID];
		for (int dist=0; dist<m_glob_params.getNoDist(); dist++)
		{ // loop on disturbances
			if (applyDist[dist] && m_DistMap(cell_ID, dist) == 1)
			{ // within mask & disturbance occurs in this cell
				for (unsigned fg=0; fg<m_FGparams.size(); fg++)
				{ // loop on PFG
					m_SuccModelMap(cell_ID)->DoDisturbance(fg,dist,m_SuccModelMap(cell_ID)->getCommunity_()->getFuncGroup_(fg)->getFGparams_()->getDistResponse());
				}
			}
		} // end loop over disturbances
	} // end loop over cells
} // end of DoDisturbance(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void SimulMap::UpdateEnvSuitRefMap(unsigned option)
{
	vector< double > envSuitRefVal(m_Mask.getTotncell(),0.5);

	unsigned seed = chrono::system_clock::now().time_since_epoch().count();
	RandomGenerator rng(seed);
	Uni01 random_01(rng);

	if (option==1)
	{
		/* draw a random number for each pixel*/
		for (vector<unsigned>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
		{
			envSuitRefVal[*cell_ID] = random_01(); //( rand()/(double)RAND_MAX );
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
			double meanFG = random_01(); //( rand()/(double)RAND_MAX );
			double sdFG = random_01(); //( rand()/(double)RAND_MAX );
			logg.info("NEW Env Suit Ref distrib for FG : ", fg_id, "  with mean=",
								meanFG, " and sd=", sdFG);

			/* build the distribution corresponding to these mean and sd */
			Normal distrib(meanFG,sdFG);
			GeneratorNorm draw_from_distrib(rng,distrib);

			/* draw a random number from this distribution for each pixel*/
			envSuitRefVal.resize(m_Mask.getTotncell(),0.5);
			for (vector<unsigned>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
			{
				envSuitRefVal[*cell_ID] = draw_from_distrib();
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
	m_glob_params  = GSP(file_of_params.getGlobSimulParams());

	/* check some parameters compatibility */
	if (old_glob_params.getNoFG() != m_glob_params.getNoFG())
	{
		logg.error("!!! Number of functional groups involved in saved object (",
		 					 old_glob_params.getNoFG(),
							 ") is incompatible with new parameters (",
							 m_glob_params.getNoFG(), ")!");
	}
	if (old_glob_params.getDoHabSuitability() != m_glob_params.getDoHabSuitability())
	{
		logg.error("!!! Succession model involved in saved object (",
		 					 old_glob_params.getDoHabSuitability(),
							 ") is incompatible with new parameters (",
							  m_glob_params.getDoHabSuitability(), ")!");
	}
	if (old_glob_params.getNoStrata() != m_glob_params.getNoStrata())
	{
		logg.error("!!! Number of strata involved in saved object (",
		 					 old_glob_params.getNoStrata(),
							 ") is incompatible with new parameters (",
							  m_glob_params.getNoStrata(), ")!");
	}
	if (old_glob_params.getNoDist() != m_glob_params.getNoDist())
	{
		logg.error("!!! Number of disturbances involved in saved object (",
		 					 old_glob_params.getNoDist(),
							 ") is incompatible with new parameters (",
							 m_glob_params.getNoDist(), ")!");
	}
	if (old_glob_params.getNoDistSub() != m_glob_params.getNoDistSub())
	{
		logg.error("!!! Number of way to be influence by disturbances involved in saved object (",
		 					 old_glob_params.getNoDistSub(),
							 ") is incompatible with new parameters (",
							 m_glob_params.getNoDistSub(), ")");
	}
	/* end of check parameters compatibility */

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
	}

	/* update disturbances mask if needed */
	if (file_of_params.getMaskDist()[0] != "0")
	{
		logg.info("***** Update disturbances maps...");
		vector< vector< int > > distMap; // disturbances map
		distMap.reserve(m_glob_params.getNoDist());
		for (int dist_id=0; dist_id<m_glob_params.getNoDist(); dist_id++)
		{
			distMap.emplace_back( ReadMask<int>( file_of_params.getMaskDist()[dist_id], 0.0, 1.0 ) );
		}
		m_DistMap = SpatialStack<double, int>(&m_Coord, distMap);
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

		for (vector<unsigned>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
		{ // loop on pixels
			for (unsigned fg=0; fg<m_FGparams.size(); fg++)
			{ // loop on PFG
				m_PostDroughtMap(*cell_ID, fg) = 0;
				m_CountDroughtMap(*cell_ID, fg) = 0;
			}
		}
	}

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

void SimulMap::SaveRasterAbund(string saveDir, int year, string prevFile)
{
	GDALAllRegister();

	// Start timer
	time_t start,end;
	time(&start);

	// Get output driver (GeoTIFF format).
	const char * driverInput = "GTiff";
	boost::filesystem::path prevFile_path(prevFile.c_str());
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

	logg.info(">>> Saving PFG abund outputs");

	/* 1. ABUND PER PFG and PER STRATA */
	logg.info("> Saving abund per PFG & per strata");
	/* 2. ABUND PER PFG for ALL STRATA */
	logg.info("> Saving abund per PFG for all strata");
	omp_set_num_threads( m_glob_params.getNoCPU() );
	#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
	for (unsigned fg=0; fg<m_FGparams.size(); fg++)
	{ // loop on PFG
		//logg.info(">>>>> PFG ", fg);
		vector<int> strAgeChange = m_FGparams[fg].getStrata(); // get strat ages change
		GUInt16 *abunValues2 = new GUInt16[m_Mask.getXncell()*m_Mask.getYncell()];
		for (unsigned pixId=0; pixId<m_Mask.getTotncell(); pixId++)
		{
			abunValues2[pixId] = 0;
		}
		bool positiveVal2 = false;
		for (int strat=1; strat<m_glob_params.getNoStrata(); strat++)
		{ // loop on Stratum
			//logg.info(">>>>> Stratum ", strat);
			// Calculate abundance values.
			GUInt16 *abunValues1 = new GUInt16[m_Mask.getXncell()*m_Mask.getYncell()];
			for (unsigned pixId=0; pixId<m_Mask.getTotncell(); pixId++)
			{
				abunValues1[pixId] = 0;
			}
			bool positiveVal1 = false;
			#pragma omp parallel for ordered
			for (unsigned pixId=0; pixId<m_MaskCells.size(); pixId++)
			{ // loop on pixels
				unsigned cell_ID = m_MaskCells[pixId];
				int abundTmp = (int)(m_SuccModelMap(cell_ID)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund( strAgeChange[strat-1] , strAgeChange[strat] - 1 ));
				abunValues1[cell_ID] = abundTmp;
				abunValues2[cell_ID] += abundTmp;
				if (abundTmp>0)
				{
					positiveVal1 = true;
					positiveVal2 = true;
				}
			} // end loop on pixels

			if (positiveVal1)
			{
				// Create the output file only if the PFG is present somewhere.
				string newFile = saveDir+"/ABUND_perPFG_perStrata/Abund_YEAR_"+boost::lexical_cast<string>(year)+"_"+m_FGparams[fg].getName()+
				"_STRATA_"+boost::lexical_cast<string>(strat)+prevFile_path.extension().string();
				//GDALDriver * outputDriver = GetGDALDriverManager()->GetDriverByName(driverInput);
				//GDALDataset * rasOutput = outputDriver->Create( newFile.c_str(), m_Mask.getXncell(), m_Mask.getYncell(), m_glob_params.getNoStrata(), GDT_UInt16, NULL );
				GDALDatasetH rasOutput = GDALCreate( outputDriver, newFile.c_str(), m_Mask.getXncell(), m_Mask.getYncell(), 1, GDT_UInt16, NULL );
				CPLAssert( rasOutput != NULL );
				GDALSetProjection( rasOutput, inputProjection ); // Write out the projection definition.
				GDALSetGeoTransform( rasOutput, outputGeoTransform ); // Write out the GeoTransform.

				//GDALRasterBand * hBand = rasOutput->GetRasterBand( strat );
				GDALRasterBandH hBand = GDALGetRasterBand( rasOutput, 1 );
				CPLErr rasterAccess = GDALRasterIO(
					hBand, GF_Write, 0, 0, m_Mask.getXncell(), m_Mask.getYncell(),
					abunValues1, m_Mask.getXncell(), m_Mask.getYncell(), GDT_UInt16, 0, 0
				);
				if (rasterAccess > 0)
				{
					logg.warning("Writing ", newFile, " raster: acces status ",
											 rasterAccess);
				}
				GDALClose( rasOutput ); // Once we're done, close properly the dataset

				// Compress file
				ostringstream ossCompressCommand;
				ossCompressCommand <<  "gzip -9 -f " << newFile;
				string strCompressCommand = ossCompressCommand.str();
				int compress_ok = system(strCompressCommand.c_str());
				if (compress_ok != 0)
				{
					logg.warning("Compression failed for ", newFile);
				}
			}
			delete [] abunValues1;
		} // end loop on Stratum
		if (positiveVal2)
		{
			// Create the output file only if the PFG is present somewhere.
			string newFile = saveDir+"/ABUND_perPFG_allStrata/Abund_YEAR_"+boost::lexical_cast<string>(year)+"_"+m_FGparams[fg].getName()+"_STRATA_all"+prevFile_path.extension().string();
			GDALDatasetH rasOutput = GDALCreate( outputDriver, newFile.c_str(), m_Mask.getXncell(), m_Mask.getYncell(), 1, GDT_UInt16, NULL );
			CPLAssert( rasOutput != NULL );
			GDALSetProjection( rasOutput, inputProjection ); // Write out the projection definition.
			GDALSetGeoTransform( rasOutput, outputGeoTransform ); // Write out the GeoTransform.

			GDALRasterBandH hBand = GDALGetRasterBand( rasOutput, 1 );
			CPLErr rasterAccess = GDALRasterIO(
				hBand, GF_Write, 0, 0, m_Mask.getXncell(), m_Mask.getYncell(),
				abunValues2, m_Mask.getXncell(), m_Mask.getYncell(), GDT_UInt16, 0, 0
			);
			if (rasterAccess > 0)
			{
				logg.warning("Writing ", newFile, " raster: acces status ",
										 rasterAccess);
			}
			GDALClose( rasOutput ); // Once we're done, close properly the dataset

			// Compress file
			ostringstream ossCompressCommand;
			ossCompressCommand <<  "gzip -9 -f " << newFile;
			string strCompressCommand = ossCompressCommand.str();
			int compress_ok = system(strCompressCommand.c_str());
			if (compress_ok != 0)
			{
				logg.warning("Compression failed for ", newFile);
			}
		}
		delete [] abunValues2;
	} // end loop on PFG

	/* 3. ABUND PER STRATA for ALL PFG */
/*	logg.info("> Saving abund per strata for all PFG");
	for (int strat=1; strat<m_glob_params.getNoStrata(); strat++)
	{ // loop on Stratum
		// Calculate abundance values.
		GUInt16 *abunValues3 = new GUInt16[m_Mask.getXncell()*m_Mask.getYncell()];
		for (unsigned pixId=0; pixId<m_Mask.getTotncell(); pixId++)
		{
			abunValues3[pixId] = 0;
		}
		bool positiveVal = false;
		for (vector<unsigned>::iterator cell_ID=m_MaskCells.begin(); cell_ID!=m_MaskCells.end(); ++cell_ID)
		{ // loop on pixels
			int abundTmp = 0;
			for (unsigned fg=0; fg<m_FGparams.size(); fg++)
			{ // loop on PFG
				vector<int> strAgeChange = m_FGparams[fg].getStrata(); // get strat ages change
				double abundTmp_double = m_SuccModelMap(*cell_ID)->getCommunity_()->getFuncGroup_(fg)->totalNumAbund( strAgeChange[strat-1] , strAgeChange[strat] - 1 );
				abundTmp_double = 10000 * abundTmp_double / double(m_NamespaceCons.getGlobalHighAbund());
				abundTmp += int(min( abundTmp_double, 10000.0)); // have to be divided by 100 to have true percentage
			} // end loop on PFG
			abunValues3[*cell_ID] = abundTmp;
			if (abundTmp>0) positiveVal = true;
		} // end loop on pixels

		if (positiveVal)
		{
			// Create the output file only if the PFG is present somewhere.
			string newFile = saveDir+"/ABUND_allPFG_perStrata/Abund_YEAR_"+boost::lexical_cast<string>(year)+"_allPFG_STRATA_"+boost::lexical_cast<string>(strat)+prevFile_path.extension().string();
			GDALDatasetH rasOutput = GDALCreate( outputDriver, newFile.c_str(), m_Mask.getXncell(), m_Mask.getYncell(), 1, GDT_UInt16, NULL );
			CPLAssert( rasOutput != NULL );
			GDALSetProjection( rasOutput, inputProjection ); // Write out the projection definition.
			GDALSetGeoTransform( rasOutput, outputGeoTransform ); // Write out the GeoTransform.

			GDALRasterBandH hBand = GDALGetRasterBand( rasOutput, 1 );
			GDALRasterIO( hBand, GF_Write, 0, 0, m_Mask.getXncell(), m_Mask.getYncell(), abunValues3, m_Mask.getXncell(), m_Mask.getYncell(), GDT_UInt16, 0, 0 );
			GDALClose( rasOutput ); // Once we're done, close properly the dataset

			// Compress file
			ostringstream ossCompressCommand;
			ossCompressCommand <<  "gzip -9 -f " << newFile;
			string strCompressCommand = ossCompressCommand.str();
			int compress_ok = system(strCompressCommand.c_str());
			if (compress_ok != 0)
			{
				logg.warning("Compression failed for ", newFile);
			}
		}
		delete [] abunValues3;
	} // end loop on Stratum*/

	if (m_glob_params.getDoSoilInteraction())
	{
		logg.info("> Saving soil outputs");
		float *soilValues = new float[m_Mask.getXncell()*m_Mask.getYncell()];
		// fill our file pix by pix
		omp_set_num_threads( m_glob_params.getNoCPU() );
		#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
		for (unsigned pixId=0; pixId<m_Mask.getTotncell(); pixId++)
		{
			soilValues[pixId] = 0;
		}
		for (unsigned ID=0; ID<m_MaskCells.size(); ID++)
		{
			unsigned cell_ID = m_MaskCells[ID];
			soilValues[cell_ID] = m_SuccModelMap(cell_ID)->getSoilResources();
		}
		// Create the output file.
		string newFile = saveDir+"/SOIL/Soil_Resources_YEAR_"+boost::lexical_cast<string>(year)+prevFile_path.extension().string();
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
			logg.warning("Writing ", newFile, " raster: acces status ",
									 rasterAccess);
		}
		GDALClose( rasOutput ); // Once we're done, close properly the dataset

		delete [] soilValues;

		// Compress file
		ostringstream ossCompressCommand;
		ossCompressCommand <<  "gzip -9 -f " << newFile;
		string strCompressCommand = ossCompressCommand.str();
		int compress_ok = system(strCompressCommand.c_str());
		if (compress_ok != 0)
		{
			logg.warning("Compression failed for ", newFile);
		}
	}

	if (m_glob_params.getDoLightInteraction())
	{
		logg.info("> Saving light outputs");

		for (int strat=0; strat<m_glob_params.getNoStrata(); strat++)
		{ // loop on Stratum
			// Calculate light values.
			GUInt16 *lightValues = new GUInt16[m_Mask.getXncell()*m_Mask.getYncell()];
			// fill our file pix by pix
			omp_set_num_threads( m_glob_params.getNoCPU() );
			#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
			for (unsigned pixId=0; pixId<m_Mask.getTotncell(); pixId++)
			{
				lightValues[pixId] = 0;
			}
			for (unsigned ID=0; ID<m_MaskCells.size(); ID++)
			{
				unsigned cell_ID = m_MaskCells[ID];
				lightValues[cell_ID] = ResourceToDouble(m_SuccModelMap(cell_ID)->getLightResources().getResource(strat));
			}

			// Create the output file.
			string newFile = saveDir+"/LIGHT/Light_Resources_YEAR_"+boost::lexical_cast<string>(year)+
				"_STRATA_"+boost::lexical_cast<string>(strat)+prevFile_path.extension().string();
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
				logg.warning("Writing ", newFile, " raster: acces status ",
										 rasterAccess);
			}
			GDALClose( rasOutput ); // Once we're done, close properly the dataset

			delete [] lightValues;

			// Compress file
			ostringstream ossCompressCommand;
			ossCompressCommand <<  "gzip -9 -f " << newFile;
			string strCompressCommand = ossCompressCommand.str();
			int compress_ok = system(strCompressCommand.c_str());
			if (compress_ok != 0)
			{
				logg.warning("Compression failed for ", newFile);
			}
		}
	}

	/* BONUS. DISPERSAL MAPS PER PFG */
	/*if (m_glob_params.getDoDispersal())
	{
		logg.info(">>> Saving DISPERSAL SEED MAPs");
		for (unsigned fg=0; fg<m_FGparams.size(); fg++)
		{ // loop on PFG
			GUInt16 *seedValues = new GUInt16[m_Mask.getXncell()*m_Mask.getYncell()];

			// fill our file pix by pix
			omp_set_num_threads( m_glob_params.getNoCPU() );
			#pragma omp parallel for schedule(dynamic) if(m_glob_params.getNoCPU()>1)
			for (unsigned pixId=0; pixId<m_Mask.getTotncell(); pixId++)
			{
				seedValues[pixId] = 0;
			}
			for (unsigned ID=0; ID<m_MaskCells.size(); ID++)
			{
				unsigned cell_ID = m_MaskCells[ID];
				seedValues[cell_ID] = m_SeedMapOut(cell_ID,fg);
			}

			// Create the output file only if the PFG is present somewhere.
			string newFile = saveDir+"/DISPERSAL/Dispersal_YEAR_"+boost::lexical_cast<string>(year)+"_"+m_FGparams[fg].getName()+prevFile_path.extension().string();
			GDALDatasetH rasOutput = GDALCreate( outputDriver, newFile.c_str(), m_Mask.getXncell(), m_Mask.getYncell(), 1, GDT_Float32, NULL );
			CPLAssert( rasOutput != NULL );

			GDALSetProjection( rasOutput, inputProjection ); // Write out the projection definition.
			double outputGeoTransform[6]; // Write out the GeoTransform.
			GDALGetGeoTransform( rasInput, outputGeoTransform );
			GDALSetGeoTransform( rasOutput, outputGeoTransform );

			GDALRasterBandH hBand = GDALGetRasterBand( rasOutput, 1 );
			GDALRasterIO( hBand, GF_Write, 0, 0, m_Mask.getXncell(), m_Mask.getYncell(), seedValues, m_Mask.getXncell(), m_Mask.getYncell(), GDT_UInt16, 0, 0 );
			GDALClose( rasOutput ); // Once we're done, close properly the dataset

			delete [] seedValues;

			// Compress file
			ostringstream ossCompressCommand;
			ossCompressCommand <<  "gzip -9 -f " << newFile;
			string strCompressCommand = ossCompressCommand.str();
			int compress_ok = system(strCompressCommand.c_str());
			if (compress_ok != 0)
			{
				logg.warning("Compression failed for ", newFile);
			}
		} // end loop on PFG
	}*/

	GDALClose( rasInput );

	// Print timer
	time(&end);
	logg.info("> Saving stuff took ", difftime (end,start), " s");
}
