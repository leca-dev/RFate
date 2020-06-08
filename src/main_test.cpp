/*=============================================================================*/
/*               FUNCTIONAL ATTRIBUTES IN TERRESTRIAL ECOSYSTEMS               */
/*                                 Version 1.1                                 */
/*                                                                             */
/* Biological model, including succession, disturbance and environmental       */
/* response sections.                                                          */
/*                                                                             */
/*=============================================================================*/

#include <iostream>
#include <memory>
#include <cstring>
#include <sstream>
#include <fstream>
#include <cstdio>
#include <vector>
#include <cmath>
#include <ctime>

#include "stdlib.h"
#include "stdio.h"
#include "string.h"

#include <assert.h> // to check objects equivalences

#include "gdal_priv.h" // to read raster files
#include "cpl_conv.h"

/* header files */
#include "FilesOfParamsList.h"
#include "GlobalSimulParameters.h"
#include "FGUtils.h"
#include "FG.h"
#include "Cohort.h"
#include "Legion.h"
#include "FuncGroup.h"
#include "PropPool.h"
#include "SuFate.h"
#include "SuFateH.h"
#include "SimulMap.h"
#include "Disp.h"
#include "Spatial.h"

/* to save and load simulation objects */
#include <boost/archive/text_oarchive.hpp> // to create archive
#include <boost/archive/text_iarchive.hpp> // to read archive
#include <boost/lexical_cast.hpp> // to transform int into string
#include <boost/filesystem.hpp>   // for file manipulation
#include <boost/serialization/export.hpp> // for children class serialisation
#include <boost/serialization/vector.hpp>
#include <boost/serialization/nvp.hpp>
#include <boost/serialization/version.hpp>

BOOST_CLASS_EXPORT_GUID(SuFateH, "SuFateH")

/* to get virtual and physical memory information */
#include "sys/types.h" // Unix-Max
#include "sys/sysinfo.h" // Unix

struct sysinfo memInfo; // Unix
using namespace std;

// TODO (damien#1#): switch to xml serealisation

using namespace std;

int main(int argc, char* argv[])
{
	/* Time consuming measurement */
	time_t Start, End;
	time(&Start);

	/* Initializing a random generator seed */
	srand(time(NULL));

	/* Read global parameter for this simulation */
	cout << endl;
	cout << "*********************************************" << endl;
	cout << "   WELCOME TO FATE-HDD TEST RUNTIME" << endl;
	cout << "*********************************************" << endl;
	cout << endl;

	/*=============================================================================*/

		/* test FGUtils class */
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test FGUtils class : enum types" << endl;
		// for Fract enum type
		cout << " ===> Fract" << endl;
		vector<Fract> frac = {PC00, PC10, PC20, PC30, PC40, PC50, PC60, PC70, PC80, PC90, PC100};
		for (int i=0; i<Fcount; i++)
		{
			cout << "> FractToDouble(PC" << i * 10 << ") = " << FractToDouble(Fract(i)) << endl;
			cout << "> FractToDouble(PC" << i * 10 << ") = " << FractToDouble(frac[i]) << endl;
		}
		for (int i=0; i<Fcount; i++)
		{
			cout << "> DoubleToFract(FractToDouble(PC" << i * 10 << ")) = " << DoubleToFract(FractToDouble(Fract(i))) << endl;
			cout << "> DoubleToFract(FractToDouble(PC" << i * 10 << ")) = " << DoubleToFract(FractToDouble(frac[i])) << endl;
		}
		for (int i=0; i<Fcount; i++)
		{
			cout << "> DoubleToFract(" << i / 10.0 << ") = " << DoubleToFract(i / 10.0) << endl;
		}
		cout << "> DoubleToFract(0.3) = " << DoubleToFract(0.3) << endl;
		cout << "> DoubleToFract(1) = " << DoubleToFract(1) << endl;
		cout << "> DoubleToFract(10) = " << DoubleToFract(10.0) << endl;


		// for Fract2 enum type
		cout << " ===> Fract2" << endl;
		vector<Fract2> frac2 = {F2None, F2Low, F2Medium, F2High, F2All};
		for (int i=0; i<F2count; i++)
		{
			cout << "> FractToDouble(" << Fract2(i) << ") = " << FractToDouble(Fract2(i)) << endl;
			cout << "> FractToDouble(" << frac2[i] << ") = " << FractToDouble(frac2[i]) << endl;
		}

	/*=============================================================================*/

		/* test FGUtils class */
/*		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test FGUtils class : test functions" << endl;
		testDirExist("MAKEFILES", "MAKEFILES");
		testDirExist("FAKE_DIR", "FAKE_DIR");
		testFileExist("FateHD_compilation", "FateHD_compilation.sh");
		vector<string> check_files = {"FateHD_compilation.sh", "Fake_file"};
		testFileExist("check_files", check_files);*/

	/*=============================================================================*/

		/* Test of Reading parameters functions */
//		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
//		cout << "Test of Reading parameters functions" << endl;
//		vector<string> path = ReadParamsWithinFile("/home/damien/__CODE_SOURCE__/biomove-code/CPP_SOURCES/FATEHDD/code_blocks/FATEHDD/test/input/Simul_TestLongDisp/PARAM_SIMUL/paramSimul_1.txt", "MASK", "--");
//		cout << "path = " ;
//		copy(path.begin(), path.end(), ostream_iterator<string>(cout, " "));
//		cout << endl;

	/*=============================================================================*/

		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << "> working dir = " << boost::filesystem::current_path() << endl;

		/* test of input args */
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		// If the user didn't provide a filename command line argument, print an error and exit.
		if (argc <= 1)
	 	{
	 		cerr << "Warning! Usage: " << argv[0] << " <parameters_file>" << endl;
	 		cerr << "You can also ask for software version info with " << argv[0] << " -v" << endl;
	 		terminate();
	 	}
		string paramFile(argv[1]);
		cout << "> paramFile = " << paramFile << endl;

	/*=============================================================================*/

		/* test FOPL class */
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test FOPL class" << endl;
		FOPL file_of_params_DEFAULT;
		file_of_params_DEFAULT.show();
		FOPL file_of_params(paramFile);
		file_of_params.show();
		file_of_params.checkCorrectParams();
		//file_of_params.checkCorrectParams(false, true, true, false, false, false, false, false);
		file_of_params.checkCorrectMasks();

		/* test FGUtils class */
/*		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test FGUtils class : namespace constants" << endl;
		FGUtils name_const_DEFAULT;
		name_const_DEFAULT.show();
		cout << "> file_of_params.getNamespaceConstants() = " << file_of_params.getNamespaceConstants() << endl;
		FGUtils name_const(file_of_params.getNamespaceConstants());
		name_const.show();*/


		/* test GSP class */
		// Missing test of constructor with all attributes given
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test GSP class" << endl;
		GSP glob_params_DEFAULT;
		glob_params_DEFAULT.show();
		cout << "> file_of_params.getGlobSimulParams() = " << file_of_params.getGlobSimulParams() << endl;
		GSP glob_params(file_of_params.getGlobSimulParams());
		glob_params.show();

	/*=============================================================================*/

		/* test FG class */
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test FG class : filled by hand" << endl;
		FG fg;
		fg.show();

		fg.setName("spp_test");
		fg.setLifeSpan(10);
		int poolLm[2] = {3, 10};
		fg.setPoolLife(poolLm);
		fg.setMaxAbund(AHigh);
		FGresponse fg_resp = fg.getDistResponse();
		fg_resp.setFates(PC10,0,0,Unaff);
		fg_resp.setFates(PC40,0,1,Kill);
		fg.setDistResponse(fg_resp);
		fg.show();

		/*cout << "Try setPoolLife with more than PTcount values :" << endl;
		int poolL_wrong[3] = {3, 10, 2};
		fg.setPoolLife(poolL_wrong);*/

		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test FG class : filled with parameter files" << endl;
		cout << "> FG Life history file = " << file_of_params.getFGLifeHistory()[0] << endl;
		cout << "> FG Disturbance file = " << file_of_params.getFGDisturbance()[0] << endl;
		cout << "> FG Dispersal history file = " << file_of_params.getFGDispersal()[0] << endl;
		cout << endl;

		FG fg0(glob_params, file_of_params, 0);
		fg0.show();

		vector<string> new_disp_files = {"DISP_pfg1", "DISP_pfg2", "DISP_pfg3"};
		file_of_params.setFGDispersal(new_disp_files);
		FG fg1(glob_params, file_of_params, 1);
		fg1.show();

		vector<string> new_succ_files = {"SUCC_pfg1", "SUCC_pfg2", "SUCC_pfg3"};
		file_of_params.setFGLifeHistory(new_succ_files);
		vector<string> new_light_files = {"SUCC_pfg1", "SUCC_pfg2", "SUCC_pfg3"};
		file_of_params.setFGLight(new_light_files);

		glob_params.setDoDisturbances(true);
		glob_params.setNoDist(2);
		glob_params.setNoDistSub(4);
		FG fg2(glob_params, file_of_params, 2);
		fg2.show();

	/*=============================================================================*/

		/* Test of cohort class */
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test Cohort class" << endl;
		Cohort cohort;
		cohort.show();
		Cohort cohort0(10,1,2);
		Cohort cohort1(100,10,20);
		Cohort cohort2(1000,100,200);
		cohort1.setAy(2);
		cohort2.setAo(50);
		cohort0.show();
		cohort1.show();
		cohort2.show();

		/* Test of legion class */
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test Legion class" << endl;
		cout << "> add new cohorts" << endl;
		Legion legion;
		legion.show();
		legion.addCohort(1,1,3);
		legion.addCohort(1,3,5);
		legion.addCohort(1,2,7);
		legion.addCohort(1,10,11);
		legion.addCohort(1,6,10);
		legion.show();

		cout << "> delete nothing" << endl;
		legion.removeCohort(0,0);
		legion.show();

		cout << "> delete age 10" << endl;
		legion.removeCohort(10,10);
		legion.show();

		cout << "> delete all older than 10..." << endl;
		legion.removeCohort(10,30);
		legion.show();

		cout << "> delete age 9" << endl;
		legion.removeCohort(9,9);
		legion.show();

		cout << "> delete all younger than 2..." << endl;
		legion.removeCohort(0,2);
		legion.show();

		cout << "> delete from 4 to 7" << endl;
		legion.removeCohort(4,7);
		legion.show();

		cout << "> delete all" << endl;
		legion.removeCohort(0,30);
		legion.show();

		cout << "> add new cohort " << endl;
		legion.addCohort(1,1,10);
		legion.show();

		cout << "> delete from 3 to 7" << endl;
		legion.removeCohort(3,7);
		legion.show();

		cout << "> add new cohort" << endl;
		legion.addCohort(1000,1,50);
		legion.show();

		cout << "> reduce cohort by 0.5 between 1 and 20" << endl;
		legion.reduceCohort(1,20,0.5);

		cout << "> reduce cohort by 0.5 between 60 and 80" << endl;
		legion.reduceCohort(60,80,0.5);
		legion.show();

		cout << "> reduce cohort by 0 between 2 and 6" << endl;
		legion.reduceCohort(2,6,0);
		legion.show();

		cout << "> reduce cohort by 5 between 2 and 12" << endl;
		legion.reduceCohort(2,12,5);
		legion.show();

	/*=============================================================================*/

		/* Test of PropPool class */
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test PropPool class" << endl;
		PropPool proppool;
		proppool.show();

		proppool.setSize(102);
		proppool.setDeclining(true);
		proppool.show();

	/*=============================================================================*/

		/* Test of LightResources class */
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test LightResources class" << endl;
		LightResources LR0(glob_params.getNoStrata());
		LR0.show();

		LightResources LR1(4);
		LR1.show();

		vector<Resource> vec_resources = {RLow, RLow, RHigh, RMedium, RLow};
		LightResources LR2(vec_resources);
		LR2.show();

	/*=============================================================================*/

		/* Test of FuncGroup class */
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test FuncGroup class" << endl;
//		boost::shared_ptr<FG> fg_ptr (new FG(fg));
		FG* fg_ptr = new FG(fg);
		FuncGroup funcGroup(fg_ptr);
		funcGroup.show();
		funcGroup.summary();

		FG* fg_ptr1 = new FG(fg1);
		FuncGroup funcGroup1(fg_ptr1);
		funcGroup1.show();
		funcGroup1.summary();

		vector<PropPool> vec_proppool = {proppool};
		FuncGroup funcGroup2(vec_proppool, legion, fg_ptr1);
		funcGroup2.show();
		funcGroup2.summary();

	/*=============================================================================*/

		/* Test of SuFate class */
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test FuncGroup class: functions" << endl;
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;

		// pfg parameters obj creation
//		boost::shared_ptr<FG> fg0_ptr (new FG(fg0));
//		boost::shared_ptr<FG> fg1_ptr (new FG(fg1));
//		boost::shared_ptr<FG> fg2_ptr (new FG(fg2));
		FG* fg0_ptr = new FG(fg0);
		FG* fg1_ptr = new FG(fg1);
		FG* fg2_ptr = new FG(fg2);

		// Functional Groups obj creation
		FuncGroup FG0(fg0_ptr);
		FuncGroup FG1(fg1_ptr);
		FuncGroup FG2(fg2_ptr);

		// Add some individuals
		FG0.getLList_()->addCohort(1,1,3);
		FG0.getLList_()->addCohort(1,3,5);
		FG0.getLList_()->addCohort(1,2,7);
		FG0.getLList_()->addCohort(10,1,50);
		FG0.getLList_()->addCohort(100,0,100);
		FG1.getLList_()->addCohort(12,6,10);
		FG2.getLList_()-> addCohort(1000000,7,100);
		//FG2.getLList_()->removeCohort(64, 9);
		//FG2.getLList_()->reduceCohort(80,60,0.5);
		//FG2.getLList_()->addCohort(1000000,100, 7);
		//FG2.getLList_()->removeCohort(9, 64);

		FG0.show();
		FG1.show();
		FG2.show();

/*		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << "FG0.totalNumAbund(...) = " << endl;
		cout << "(1,4) = " << FG0.totalNumAbund(1,4) << endl;
		cout << "(1,5) = " << FG0.totalNumAbund(1,5) << endl;
		cout << "(5,10) = " << FG0.totalNumAbund(5,10) << endl;
		cout << "(6,10) = " << FG0.totalNumAbund(6,10) << endl;
		cout << "(1,50) = " << FG0.totalNumAbund(1,50) << endl;
		cout << "(1,100) = " << FG0.totalNumAbund(1,100) << endl;
		cout << "(1,200) = " << FG0.totalNumAbund(1,200) << endl;
		cout << "(150,200) = " << FG0.totalNumAbund(150,200) << endl;
		cout << "FG0.getLList_()->getNumAbund(...) = " << endl;
		cout << "(1,4) = " << FG0.getLList_()->getNumAbund(1,4) << endl;
		cout << "(1,5) = " << FG0.getLList_()->getNumAbund(1,5) << endl;
		cout << "(5,10) = " << FG0.getLList_()->getNumAbund(5,10) << endl;
		cout << "(6,10) = " << FG0.getLList_()->getNumAbund(6,10) << endl;
		cout << "(1,50) = " << FG0.getLList_()->getNumAbund(1,50) << endl;
		cout << "(1,100) = " << FG0.getLList_()->getNumAbund(1,100) << endl;
		cout << "(1,200) = " << FG0.getLList_()->getNumAbund(1,200) << endl;
		cout << "(150,200) = " << FG0.getLList_()->getNumAbund(150,200) << endl;
		cout << endl;

		FG0.getFGparams_()->setImmSize(0.5);
		cout << "FG0.totalNumAbund(...) = " << endl;
		cout << "(1,4) = " << FG0.totalNumAbund(1,4) << endl;
		cout << "(1,5) = " << FG0.totalNumAbund(1,5) << endl;
		cout << "(5,10) = " << FG0.totalNumAbund(5,10) << endl;
		cout << "(6,10) = " << FG0.totalNumAbund(6,10) << endl;
		cout << "(1,50) = " << FG0.totalNumAbund(1,50) << endl;
		cout << "(1,100) = " << FG0.totalNumAbund(1,100) << endl;
		cout << "(1,200) = " << FG0.totalNumAbund(1,200) << endl;
		cout << "(150,200) = " << FG0.totalNumAbund(150,200) << endl;
		cout << "FG0.getLList_()->getNumAbund(...) = " << endl;
		cout << "(1,4) = " << FG0.getLList_()->getNumAbund(1,4) << endl;
		cout << "(1,5) = " << FG0.getLList_()->getNumAbund(1,5) << endl;
		cout << "(5,10) = " << FG0.getLList_()->getNumAbund(5,10) << endl;
		cout << "(6,10) = " << FG0.getLList_()->getNumAbund(6,10) << endl;
		cout << "(1,50) = " << FG0.getLList_()->getNumAbund(1,50) << endl;
		cout << "(1,100) = " << FG0.getLList_()->getNumAbund(1,100) << endl;
		cout << "(1,200) = " << FG0.getLList_()->getNumAbund(1,200) << endl;
		cout << "(150,200) = " << FG0.getLList_()->getNumAbund(150,200) << endl;
		cout << endl;

		cout << "TEST getNumAbund : how it was" << endl;
		cout << FG0.getLList_()->getNumAbund( 1,FG0.getFGparams_()->getMatTime() ) * FG0.getFGparams_()->getImmSize() +
		FG0.getLList_()->getNumAbund( FG0.getFGparams_()->getMatTime()+1,FG0.getFGparams_()->getLifeSpan() ) << endl;
		cout << "TEST getNumAbund : how it should have been" << endl;
		cout << FG0.getLList_()->getNumAbund( 1,FG0.getFGparams_()->getMatTime() -1) * FG0.getFGparams_()->getImmSize() +
		FG0.getLList_()->getNumAbund( FG0.getFGparams_()->getMatTime(),FG0.getFGparams_()->getLifeSpan() ) << endl;
		cout << "TEST totalNumAbund : how it can be replaced" << endl;
		cout << FG0.totalNumAbund( 1,FG0.getFGparams_()->getLifeSpan() ) << endl;
		// difference comes from that weighting by ImmSize is done for each cohort with totalNumAbund,
		// while it's done only once at the end for getNumAbund

		cout << "TEST decomposition : " << endl;
		cout << FG0.getLList_()->getNumAbund( 1,FG0.getFGparams_()->getMatTime() ) << endl;
		cout << FG0.getLList_()->getNumAbund( 1,FG0.getFGparams_()->getMatTime() -1 ) << endl;
		cout << FG0.getLList_()->getNumAbund( 1,FG0.getFGparams_()->getMatTime() ) * FG0.getFGparams_()->getImmSize() << endl;
		cout << FG0.getLList_()->getNumAbund( 1,FG0.getFGparams_()->getMatTime() -1 ) * FG0.getFGparams_()->getImmSize() << endl;
		cout << FG0.totalNumAbund( 1,FG0.getFGparams_()->getMatTime() -1 ) << endl;

		cout << "(100,1) = " << FG0.totalNumAbund(100,1) << endl;
		cout << "(100,1) = " << FG0.getLList_()->getNumAbund(100,1) << endl;*/


		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << "> agelegion function" << endl;
		cout << "+1 year" << endl;
		FG2.ageLegions();
		FG2.show();
		cout << "+1 year" << endl;
		FG2.ageLegions();
		FG2.show();
		cout << "+1 year" << endl;
		FG2.ageLegions();
		FG2.show();
		cout << "+1 year" << endl;
		FG2.ageLegions();
		FG2.show();

	/*=============================================================================*/

		// GetPoolInput function test
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << "> getPools function" << endl;
		FG0.show();
		FG0.getPools_( (PoolType) 0 )->setSize(100);
		FG0.getFGparams_()->setPoolLife(20, DormantP);
		FG0.show();
		FG0.AgePool1();
		FG0.show();
		FG0.AgePool1();
		FG0.show();

	/*=============================================================================*/

		/* Test of Community class */
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test Community class" << endl;
		vector< FuncGroup > com0(3);
		com0[0] = FG0;
		com0[1] = FG1;
		com0[2] = FG2;
		Community COM0(com0);
		COM0.show();

	/*=============================================================================*/

		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test SuFate class" << endl;

		// Global parameters object creation
		GSPPtr GSP_ptr (new GSP(glob_params));

		// Community object creation
		//CommunityPtr com0_ptr (new Community(COM0));

		// Light resources creation
		//LightResourcesPtr lr_ptr( new LightResources(LR1) );

		/* creation of seed maps */
		Coordinates<double>* m_Coord_ptr = new Coordinates<double>( ReadCoordinates(file_of_params.getMask()) );
		SpatialStack<double, double>* m_SeedMapIn = new SpatialStack<double, double>(m_Coord_ptr, COM0.getFuncGroupList().size());
		SpatialStack<double, double>* m_SeedMapOut = new SpatialStack<double, double>(m_Coord_ptr, COM0.getFuncGroupList().size());

		/* creation of namespace constants file */
		//FGUtilsPtr name_const_ptr (new FGUtils(name_const));

		// Succession model creation
		SuFate succ(1, COM0, LR1, 0.0, m_SeedMapOut, m_SeedMapIn, GSP_ptr); //, name_const_ptr);
		succ.show();

	/*=============================================================================*/

		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test SuFate class : functions" << endl;

		// CalculateEnvironment function test
/*		cout << "> CalculateEnvironment function test" << endl;
		succ.CalculateEnvironment(false, 0.0);
		succ.show();*/

		// CheckSurvival function test
/*		cout << "> CheckSurvival function test" << endl;
		succ.CheckSurvival();
		FG0.show();
		succ.getCommunity_()->getFuncGroup_(0)->show();*/



	/*=============================================================================*/

		// Test do.succession function
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << "> DoSuccession function" << endl;
		succ.show();
		succ.DoSuccessionPart1({0});
		succ.DoSuccessionPart1({0});
		succ.DoSuccessionPart1({0});
		succ.show();
		cout << endl;

		// Test DoDisturbance
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << "> DoDisturbance function" << endl;
		succ.getCommunity_()->getFuncGroup_(2)->show();
		succ.getCommunity_()->getFuncGroup_(2)->getLList_()->addCohort(100, 1, 50);
		succ.getCommunity_()->getFuncGroup_(2)->show();
		succ.DoDisturbance(2, 0, succ.getCommunity_()->getFuncGroup_(2)->getFGparams_()->getDistResponse());
		succ.getCommunity_()->getFuncGroup_(2)->show();

	/*=============================================================================*/

		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test SuFateH class" << endl;

		// Envsuit and EnvsuitParams ptr creation
		SpatialStack<double, double>* m_EnvSuitMapPtr = new SpatialStack<double, double>(m_Coord_ptr, COM0.getFuncGroupList().size());
		SpatialStack<double, double>* m_EnvSuitRefMapPtr = new SpatialStack<double, double>(m_Coord_ptr, COM0.getFuncGroupList().size());

		// Succession model creation
		SuFateH succH(1, COM0, LR1, 0.0, m_SeedMapOut, m_SeedMapIn, GSP_ptr, m_EnvSuitMapPtr, m_EnvSuitRefMapPtr);
		succH.show();

		succH.DoSuccessionPart1({0});
		succH.show();

	/*=============================================================================*/

		// Test Dispersal functions
		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test Disp class" << endl;

		// Create a small frame : 10 x 10 pixels
		Coordinates<double>* coordD_ptr = new Coordinates<double>(0.0,1000.0,100.0, 0.0,1000.0,100.0);

		// Create map of input seeds
		vector< double > seedInFG0(100,10);
		vector< double > seedInFG1(100,100);
		//vector< double > seedInFG2(100,1000);
		vector< double > seedInFG2(100,0);
		seedInFG2[45] = 1000;
		vector< vector< double > > seedIn = {seedInFG0, seedInFG1, seedInFG2};
		SpatialStack<double, double>* seedMapIn_ptr = new SpatialStack<double, double>(coordD_ptr, seedIn);

		// Create map of output seeds
		vector< double > seedOutFG0(100,0);
		vector< double > seedOutFG1(100,0);
		vector< double > seedOutFG2(100,0);
		vector< vector< double > > seedOut = {seedOutFG0, seedOutFG1, seedOutFG2};
		SpatialStack<double, double>* seedMapOut_ptr = new SpatialStack<double, double>(coordD_ptr, seedOut);

		// Create vector of FG
		fg2.setDisp50(100);
		fg2.setDisp99(300);
		fg2.setDispLD(500);
		vector<FG> FGparams = {fg0, fg1, fg2};
		vector<FG>* FGparams_ptr = new vector<FG>(FGparams);

		cout << "> before dispersal (FG2) : input" << endl;
		for (unsigned i=0; i<10; i++){
			for (unsigned j=0; j<10; j++){
				cout << (*seedMapIn_ptr)(j,i,2) << "\t";
			}
			cout << endl;
		}

		cout << "> before dispersal (FG2) : output" << endl;
		for (unsigned i=0; i<10; i++){
			for (unsigned j=0; j<10; j++){
				cout << (*seedMapOut_ptr)(j,i,2) << "\t";
			}
			cout << endl;
		}

		// Create Disp object
		Disp disp_map(FGparams_ptr, seedMapIn_ptr, seedMapOut_ptr);
		vector<unsigned> pix_id(100, 0);
		for (unsigned i=0; i<100; i++){ pix_id[i] = i; }
		disp_map.DoDispersalPacket(1, 1, pix_id);


		cout << "> after dispersal (FG2) : input" << endl;
		for (unsigned i=0; i<10; i++){
			for (unsigned j=0; j<10; j++){
				cout << (*seedMapIn_ptr)(j,i,2) << "\t";
			}
			cout << endl;
		}

		cout << "> after dispersal (FG2) : output" << endl;
		for (unsigned i=0; i<10; i++){
			for (unsigned j=0; j<10; j++){
				cout << (*seedMapOut_ptr)(j,i,2) << "\t";
			}
			cout << endl;
		}
		//terminate();

	/*=============================================================================*/

		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test SimulMap class" << endl;

		file_of_params.show();
		file_of_params.setFGLifeHistory({"SUCC_pfg1","SUCC_pfg2"});
		file_of_params.setFGDispersal({"DISP_pfg1","DISP_pfg2"});
		file_of_params.setFGDisturbance({"DIST_pfg1","DIST_pfg2"});
		SimulMap simulMap(file_of_params);

	/*=============================================================================*/

		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << " ===> test SimulMap functions" << endl;

		cout << "> DistMap before change mask procedure" << endl;
		for(unsigned j=0; j<simulMap.getDistMap().getYncell(); j++)
		{
			for(unsigned i=0; i<simulMap.getDistMap().getXncell(); i++)
			{
				cout << simulMap.getDistMap()(i,j,0) << " ";
			}
			cout << endl;
		}

		simulMap.DoFileChange("DIST_change_mask", "dist");

		cout << "> DistMap after change mask procedure" << endl;
		for(unsigned j=0; j<simulMap.getDistMap().getYncell(); j++)
		{
			for(unsigned i=0; i<simulMap.getDistMap().getXncell(); i++)
			{
				cout << simulMap.getDistMap()(i,j,0) << " ";
			}
			cout << endl;
		}

	/*=============================================================================*/

		cout << "> envSuitRefMap before update : PFG 1" << endl;
		for(unsigned j=0; j<simulMap.getEnvSuitRefMap().getYncell(); j++)
		{
			for(unsigned i=0; i<simulMap.getEnvSuitRefMap().getXncell(); i++)
			{
				cout << simulMap.getEnvSuitRefMap()(i,j,0) << " ";
			}
			cout << endl;
		}
		cout << "> envSuitRefMap before update : PFG 2" << endl;
		//cout << " " << simulMap.getEnvSuitRefMap().getNoLayers() << endl;
		for(unsigned j=0; j<simulMap.getEnvSuitRefMap().getYncell(); j++)
		{
			for(unsigned i=0; i<simulMap.getEnvSuitRefMap().getXncell(); i++)
			{
				cout << simulMap.getEnvSuitRefMap()(i,j,1) << " ";
			}
			cout << endl;
		}

		simulMap.UpdateEnvSuitRefMap(1);

		cout << "> envSuitRefMap after update 1 : PFG 1" << endl;
		for(unsigned j=0; j<simulMap.getEnvSuitRefMap().getYncell(); j++)
		{
			for(unsigned i=0; i<simulMap.getEnvSuitRefMap().getXncell(); i++)
			{
				cout << simulMap.getEnvSuitRefMap()(i,j,0) << " ";
			}
			cout << endl;
		}
		cout << "> envSuitRefMap after update 1 : PFG 2" << endl;
		for(unsigned j=0; j<simulMap.getEnvSuitRefMap().getYncell(); j++)
		{
			for(unsigned i=0; i<simulMap.getEnvSuitRefMap().getXncell(); i++)
			{
				cout << simulMap.getEnvSuitRefMap()(i,j,1) << " ";
			}
			cout << endl;
		}

		simulMap.UpdateEnvSuitRefMap(2);

		cout << "> envSuitRefMap after update 2 : PFG 1" << endl;
		for(unsigned j=0; j<simulMap.getEnvSuitRefMap().getYncell(); j++)
		{
			for(unsigned i=0; i<simulMap.getEnvSuitRefMap().getXncell(); i++)
			{
				cout << simulMap.getEnvSuitRefMap()(i,j,0) << " ";
			}
			cout << endl;
		}
		cout << "> envSuitRefMap after update 2 : PFG 2" << endl;
		for(unsigned j=0; j<simulMap.getEnvSuitRefMap().getYncell(); j++)
		{
			for(unsigned i=0; i<simulMap.getEnvSuitRefMap().getXncell(); i++)
			{
				cout << simulMap.getEnvSuitRefMap()(i,j,1) << " ";
			}
			cout << endl;
		}

	/*=============================================================================*/

		unsigned pt_id = 2;
		unsigned fg_id = 0;
		for(unsigned fg=0; fg<simulMap.getFGparams().size(); fg++)
		{
			simulMap.getFGparams()[fg].setMaxRecruitLight({PC100, PC90, PC90});
			simulMap.getFGparams()[fg].setInnateDormancy(false);
			simulMap.getFGparams()[fg].setTolerance({{PC100,PC100,PC100},{PC100,PC100,PC100},{PC100,PC100,PC100}});
			simulMap.getFGparams()[fg].setImmSize(0.4);
			simulMap.getFGparams()[fg].show();
		}

		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << "> DoSuccession with seeding..." << endl;
		simulMap.getSuccModelMap()(pt_id)->show();
		simulMap.StartSeeding();
		for(unsigned year=0; year<30; year++)
		{
			cout << "year " << year << endl;
			// Does not change because fecundity does not change because total abund of mature is null
			//cout << "seedMapIn = " << simulMap.getSeedMapIn()(pt_id,fg_id) << " ";
			//cout << "seedMapOut = " << simulMap.getSeedMapOut()(pt_id,fg_id) << "\n";
			simulMap.DoSuccession();
		}
		cout << endl << endl;
		cout << "\nyear 30" << endl;
		simulMap.getSuccModelMap()(pt_id)->show();

		cout << endl << endl;
		for(unsigned fg=0; fg<simulMap.getFGparams().size(); fg++)
		{
			simulMap.getSuccModelMap()(pt_id)->getCommunity_()->getFuncGroup_(fg)->show();
		}

		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << "> DoSuccession without seeding..." << endl;
		simulMap.StopSeeding();
		for(unsigned year=0; year<20; year++)
		{
			cout << "year " << year << endl;
			// Does not change because fecundity does not change because total abund of mature is null
			//cout << "seedMapIn = " << simulMap.getSeedMapIn()(pt_id,fg_id) << " ";
			//cout << "seedMapOut = " << simulMap.getSeedMapOut()(pt_id,fg_id) << "\n";

			simulMap.DoDispersal();
			simulMap.DoSuccession();
		}
		cout << endl << endl;
		cout << "\nyear 50" << endl;
		simulMap.getSuccModelMap()(pt_id)->show();

		cout << endl << endl;
		for(unsigned fg=0; fg<simulMap.getFGparams().size(); fg++)
		{
			simulMap.getSuccModelMap()(pt_id)->getCommunity_()->getFuncGroup_(fg)->show();
		}

	/*=============================================================================*/

		cout << endl << endl << "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-" << endl;
		cout << "> Serialization tests : PropPool" << endl;

		// Create data
		PropPool pp(10, false, 100);

		// Save data
		const char* fileName = "saved.txt";
		{
			// Create an output archive
			ofstream ofs(fileName, fstream::binary | fstream::out);
			boost::archive::text_oarchive ar(ofs);

			// Write data
			ar << pp;
			ar << fg0;
			ar << succ;
			ofs.close();
		}
		cout << "simple objects saved" << endl;

		// Restore data
		PropPool restored_pp;
		FG restored_fg0;
		SuFate restored_succ;
		{
			// Create and input archive
			ifstream ifs(fileName, fstream::binary | fstream::in ); //ios::in | ios::binary );
			boost::archive::text_iarchive ar(ifs);

			// Load data
			ar & restored_pp & restored_fg0 & restored_succ;
			ifs.close();
		}
		cout << "simple objects restored" << endl;

		// Make sure we restored the data exactly as it was saved
		assert(restored_pp == pp);
		assert(restored_fg0 == fg0);
		assert(restored_succ == succ);


		cout << endl;
		cout << "> Serialization tests : SimulMap" << endl;

		// Save data
		const char* fileName_compl = "saved_compl.txt";
		{
			// Create an output archive
			ofstream ofs(fileName_compl, fstream::binary | fstream::out);
			boost::archive::text_oarchive ar(ofs);

			// Write data
			ar << simulMap;
			ofs.close();
		}
		cout << "complex objects saved" << endl;

		// Restore data
		SimulMap restored_simulMap;
		{
			// Create and input archive
			ifstream ifs(fileName_compl, fstream::binary | fstream::in ); //ios::in | ios::binary );
			boost::archive::text_iarchive ar(ifs);

			// Load data
			ar & restored_simulMap;
			ifs.close();
		}
		cout << "complex objects restored" << endl;

		// Make sure we restored the data exactly as it was saved
		assert(restored_simulMap == simulMap);


		cout << endl;
		cout << "> Serialization tests : inequality" << endl;
		/*for(int year=0; year<10; year++)
		{
			cout << "year" << year << endl;
			simulMap.DoSuccession();
		}
		assert(restored_simulMap == simulMap);*/


		cout << endl;
		cout << "> Serialization tests : update" << endl;

		cout << "\nINITIAL object characteristics" << endl;
		cout << "in SuFateH : " << simulMap.getEnvSuitRefMap()(pt_id, 0) << endl;
		cout << "in SuFateH : " << simulMap.getEnvSuitMap()(pt_id, 0) << endl;

		cout << "\nRESTORED object characteristics" << endl;
		cout << "in SuFateH : " << restored_simulMap.getEnvSuitRefMap()(pt_id, 0) << endl;
		cout << "in SuFateH : " << restored_simulMap.getEnvSuitMap()(pt_id, 0) << endl;

	/*=============================================================================*/

	 /* End of Run */
	 time(&End);
	 int TotTime = difftime(End,Start);

	 cout 	<< "Process executed normally! It took "
	 << TotTime/3600 << "h " << (TotTime%3600)/60
	 << "m " << (TotTime%3600)%60 << "s." << endl;

    return 0;
}
