/*============================================================================*/
/*               FUNCTIONAL ATTRIBUTES IN TERRESTRIAL ECOSYSTEMS              */
/*                                 Version 1.1                                */
/*                                                                            */
/* Biological model, including succession, disturbance and environmental      */
/* response sections.                                                         */
/*                                                                            */
/*============================================================================*/

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

#if defined(__unix__) || defined(__linux__) || defined(linux) || defined(LINUX)
	#include "gdal.h"
	#include <ogr_spatialref.h>
#endif

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
#include "Logger.h"

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

/*============================================================================*/

#if defined(__unix__) || defined(__linux__) || defined(linux) || defined(LINUX)
	/* to get virtual and physical memory information */
	#include "sys/types.h" // Unix-Mac
	#include "sys/sysinfo.h" // Unix

	struct sysinfo memInfo; // Unix

	/* MEMORY CURRENTLY USED BY CURRENT PROCESS : Unix */
	int parseLine(char* line)
	{
		int i = strlen(line);
		while (*line < '0' || *line > '9') line++;
		line[i-3] = '\0';
		i = atoi(line);
		return i;
	}

	int getMemUsed(string typeMEM)
	{ //Note: this value is in KB!
		FILE* file = fopen("/proc/self/status", "r");
		int result = -1;
		char line[128];
		while (fgets(line, 128, file) != NULL)
		{
			if (strcmp(typeMEM.c_str(),"virtual") == 0)
			{
				if (strncmp(line, "VmSize:", 7)==0)
				{ // FOR VIRTUAL MEMORY
					result = parseLine(line);
					break;
				}
			}
			if (strcmp(typeMEM.c_str(),"physical") == 0)
			{
				if (strncmp(line, "VmRSS:", 6) == 0)
				{ // FOR PHYSICAL MEMORY (RAM)
					result = parseLine(line);
					break;
				}
			}
		}
		fclose(file);
		return result;
	}
#elif defined(__APPLE__)
	/* to get virtual and physical memory information */
	#include <mach/mach.h>  // Mac
	#include <mach/vm_statistics.h> // Mac
	#include <mach/mach_types.h> // Mac
	#include <mach/mach_init.h> // Mac
	#include <mach/mach_host.h> // Mac

	struct task_basic_info t_info; // Mac
	mach_msg_type_number_t t_info_count = TASK_BASIC_INFO_COUNT; // Mac
#endif

/*============================================================================*/

using namespace std;

/* some global variables */
string FATEHDD_VERSION = "6.2-3";
SimulMap* simulMap(0);
Logger logg;

void saveFATE(string objFileName)
{
	// Create an output archive
	ofstream ofs(objFileName.c_str(), fstream::binary | fstream::out);
	boost::archive::text_oarchive ar(ofs);
	ar << simulMap; // Write data
	ofs.close();

	// Compress file
	ostringstream ossCompressCommand;
	ossCompressCommand <<  "gzip -9 -f " << objFileName;
	string strCompressCommand = ossCompressCommand.str();
	int compress_ok = system(strCompressCommand.c_str());
	if (compress_ok != 0)
	{
		cerr << "Compression failed for " << objFileName << endl;
	}
}
void loadFATE(string objFileName)
{
	// Create an input archive
	ifstream ifs( objFileName.c_str(), fstream::binary | fstream::in );
	if (ifs.good())
	{
		boost::archive::text_iarchive ar(ifs);
		ar >> simulMap; // load data
		ifs.close(); // close file
	} else
	{
		cerr << "!!! File of SAVED STATE does not exists or can't be opened!" << endl;
		terminate();
	}
}
void changeFile(int year, string change_type, vector<int>& change_times,
								vector<string>& change_files)
{
	if (change_times.size() > 0 && change_times.front() == year)
	{
		cout << "Doing " << change_type << " masks change..." << endl;
		simulMap->DoFileChange(change_files.front(), change_type);
		/* remove useless time and file from list */
		change_times.erase(change_times.begin());
		change_files.erase(change_files.begin());
	}
}

void changeFreq(int year, string change_type, vector<int>& freq_change_times,
								vector<string>& freq_change_files)
{
	if (freq_change_times.size() > 0 && freq_change_times.front() == year)
	{
		cout << "Doing " << change_type << " frequencies change..." << endl;
		simulMap->DoFreqChange(freq_change_files.front(), change_type);
		/* remove useless time and file from list */
		freq_change_times.erase(freq_change_times.begin());
		freq_change_files.erase(freq_change_files.begin());
	}
}


/*============================================================================*/
//' FATE-HD Wrapper
//'
//' This function runs a FATE-HD dynamical landscape vegetation simulation.
//'
//' @param paramFile path to the parameter file of the simulation.
//' @param nbCpus number of CPUs affected to the simulation (default 1).
//' @param verboseLevel logger verbose level.
//'
//' @return None
//'
//' @examples \dontrun{runFate()}
//'
//' @author Isabelle Boulangeat, \email{isabelle.boulangeat@inrae.fr}
//' @author Maya Gueguen, \email{maya.gueguen@univ-grenoble-alpes.fr}
//'
//' @references \url{http://www.will.chez-alice.fr/pdf/BoulangeatGCB2014.pdf}
//'
//' @export
// [[Rcpp::export]]
int runFate(std::string paramFile, int nbCpus = 1, int verboseLevel = 0)
{
	/*==========================================================================*/
	/* Initialization */

	// Logger initialization
	logg.configure(verboseLevel);  // Set logger verbose level
	// Number of CPUs used for computation
	if (nbCpus > 1)
	{
		logg.info("PARALLEL VERSION : programme will run on ", nbCpus, " CPU.");
		omp_set_num_threads(nbCpus);  // Set number of CPUs
	}
	// Timer initialization
	time_t Start, End;
	time(&Start);  // Start timer
	// Random generator seed initialization
	srand(time(NULL));

	logg.info("*********************************************\n",
						"   WELCOME TO FATE-HDD SIMULATION RUNTIME\n",
						"*********************************************\n");

	/*==========================================================================*/
	/* create the simulation parameter object that stores paths to all needed
	parameters files */

	logg.info("This simulation will be based on ", paramFile,
						" parameters file.");
	FOPL file_of_params(paramFile);
	logg.info("File of parameters created !");
	file_of_params.checkCorrectParams();
	file_of_params.checkCorrectMasks();
	file_of_params.show();

	/*==========================================================================*/
	/* FILE for saving COMPUTATION statistics */

	string strFileName = file_of_params.getSavingDir() + "/ComputationStatistics.txt";
	ofstream fileStats(strFileName.c_str(), ios::out | ios::trunc);

	#if defined(__unix__) || defined(__linux__) || defined(linux) || defined(LINUX)
		/* Memory consuming measurement */
		sysinfo (&memInfo); // Unix

		/* TOTAL VIRTUAL MEMORY */
		long long totalVirtualMem = memInfo.totalram;
		totalVirtualMem += memInfo.totalswap;
		totalVirtualMem *= memInfo.mem_unit;

		/* TOTAL PHYSICAL MEMORY (RAM) */
		long long totalPhysMem = memInfo.totalram;
		totalPhysMem *= memInfo.mem_unit;

		fileStats << "TOTAL VIRTUAL MEMORY : " << totalVirtualMem << endl;
		fileStats << "TOTAL PHYSICAL MEMORY : " << totalPhysMem << endl;
		fileStats << "Initial VIRTUAL MEMORY used : " << getMemUsed("virtual") << endl;
		fileStats << "Initial PHYSICAL MEMORY used : " << getMemUsed("physical") << endl;
	#elif defined(__APPLE__)
		/* Memory consuming measurement */
		vm_size_t page_size; // Mac
		mach_port_t mach_port; // Mac
		mach_msg_type_number_t count; // Mac
		vm_statistics64_data_t vm_stats; // Mac
	#endif

	fileStats.close();
	delete simulMap;
	return 0;

	/*=============================================================================*/
	/* check if a saving of an old simulation is given or if we start a new one from scratch */

	// the map on which the whole simulation process is based on
	if (file_of_params.getSavedState() == "0")
	{ // start from scratch
		cout << "Starting from scratch..." << endl;
		simulMap = new SimulMap(file_of_params);
	} else
	{ // start from previous simulation state
		cout << "Starting with outputs stored in " << file_of_params.getSavedState() << " file" << endl;
		{
			cout << "Loading previous simulation outputs..." << endl;
			loadFATE(file_of_params.getSavedState());
			cout << "> done! " << endl;
		}

		/* update the simulation parameters (replace the object saved ones by the current ones) */
		cout << "*** UPDATE simulation files..." << endl;
		simulMap->UpdateSimulationParameters(file_of_params);

		// FROM SAVED STATE BUT WITH NEW FG file_of_params
		cout << "*** REBUILDING Global simulation parameters..." << endl;
		GSP glob_params = GSP(file_of_params.getGlobSimulParams());
		int noFG = glob_params.getNoFG();
		cout << "*** REBUILDING Functional groups..." << endl;
		if (noFG!=(int)file_of_params.getFGLifeHistory().size())
		{
			cerr << "!!! Parameters NO_PFG (" << noFG << ") and --PFG_PARAMS_LIFE_HISTORY-- (" ;
			cerr << file_of_params.getFGLifeHistory().size() << ") do not match in term of number!" << endl;
			terminate();
		}
		if (glob_params.getDoDispersal() && noFG!=(int)file_of_params.getFGDispersal().size())
		{
			cerr << "!!! Parameters NO_PFG (" << noFG << ") and --PFG_PARAMS_DISPERSAL-- (" ;
			cerr << file_of_params.getFGDispersal().size() << ") do not match in term of number!" << endl;
			terminate();
		}
		if (glob_params.getDoDisturbances() && noFG!=(int)file_of_params.getFGDisturbance().size())
		{
			cerr << "!!! Parameters NO_PFG (" << noFG << ") and --PFG_PARAMS_DISTURBANCES-- (" ;
			cerr << file_of_params.getFGDisturbance().size() << ") do not match in term of number!" << endl;
			terminate();
		}
		if (glob_params.getDoFireDisturbances() && noFG!=(int)file_of_params.getFGFire().size())
		{
			cerr << "!!! Parameters NO_PFG (" << noFG << ") and --PFG_PARAMS_FIRE-- (" ;
			cerr << file_of_params.getFGFire().size() << ") do not match in term of number!" << endl;
			terminate();
		}
		if (glob_params.getDoDroughtDisturbances() && noFG!=(int)file_of_params.getFGDrought().size())
		{
			cerr << "!!! Parameters NO_PFG (" << noFG << ") and --PFG_PARAMS_DROUGHT-- (" ;
			cerr << file_of_params.getFGDrought().size() << ") do not match in term of number!" << endl;
			terminate();
		}
		vector<FG> fg_vec_tmp;
		for (int fg_id=0; fg_id<noFG; fg_id++)
		{
			FG fg_tmp = FG(glob_params, file_of_params, fg_id);
			fg_tmp.show();
			fg_vec_tmp.push_back(fg_tmp);
		}
		simulMap->setFGparams(fg_vec_tmp);
	}

	cout << "\n***" << " NoCPU = " << simulMap->getGlobalParameters().getNoCPU() << endl;
	fileStats << "Number of CPU used : " << simulMap->getGlobalParameters().getNoCPU() << endl;

	/*=============================================================================*/
	/* get all needed parameters */

	/* timing parameters */
	cout << "Getting timing parameters..." << endl;
	int simul_duration = simulMap->getGlobalParameters().getSimulDuration();
	bool seeding_on = false;
	int seeding_duration = simulMap->getGlobalParameters().getSeedingDuration();
	int seeding_timestep = simulMap->getGlobalParameters().getSeedingTimeStep();

	/* saving parameters */
	cout << "Getting saving parameters..." << endl;
	vector< int > summarised_array_saving_times = ReadTimingsFile( file_of_params.getSavingTimesMaps() );
	vector< int > simul_objects_saving_times = ReadTimingsFile( file_of_params.getSavingTimesObjects() );

	/* study area change parameters */
	cout << "Getting MASK timing parameters..." << endl;
	vector< int > mask_change_times = ReadTimingsFile( file_of_params.getMaskChangemaskYears() );
	vector< string > mask_change_files = file_of_params.getMaskChangemaskFiles();

	/* MODULES change parameters */
	if (simulMap->getGlobalParameters().getDoHabSuitability())
	{
		cout << "Getting HS timing parameters..." << endl;
	}
	vector< int > habsuit_change_times = ReadTimingsFile( file_of_params.getHabSuitChangemaskYears() );
	vector< string > habsuit_change_files = file_of_params.getHabSuitChangemaskFiles();
	if (simulMap->getGlobalParameters().getDoDisturbances())
	{
		cout << "Getting dist timing parameters..." << endl;
	}
	vector< int > dist_change_times = ReadTimingsFile( file_of_params.getDistChangemaskYears() );
	vector< string > dist_change_files = file_of_params.getDistChangemaskFiles();
	if (simulMap->getGlobalParameters().getDoFireDisturbances())
	{
		cout << "Getting fire timing parameters..." << endl;
	}
	vector< int > fire_change_times = ReadTimingsFile( file_of_params.getFireChangemaskYears() );
	vector< string > fire_change_files = file_of_params.getFireChangemaskFiles();
	vector< int > fire_freq_change_times = ReadTimingsFile( file_of_params.getFireChangefreqYears() );
	vector< string > fire_freq_change_files = file_of_params.getFireChangefreqFiles();
	if (simulMap->getGlobalParameters().getDoDroughtDisturbances())
	{
		cout << "Getting drought index timing parameters..." << endl;
	}
	vector< int > drought_change_times = ReadTimingsFile( file_of_params.getDroughtChangemaskYears() );
	vector< string > drought_change_files = file_of_params.getDroughtChangemaskFiles();
	if (simulMap->getGlobalParameters().getDoAliensIntroduction())
	{
		cout << "Getting aliens timing parameters..." << endl;
	}
	vector< int > aliens_change_times = ReadTimingsFile( file_of_params.getAliensChangemaskYears() );
	vector< string > aliens_change_files = file_of_params.getAliensChangemaskFiles();
	vector< int > aliens_freq_change_times = ReadTimingsFile( file_of_params.getAliensChangefreqYears() );
	vector< string > aliens_freq_change_files = file_of_params.getAliensChangefreqFiles();

	/*=============================================================================*/
	/* Simulation main loop */
	for (int year=0; year<=simul_duration; year++)
	{
		cout << endl;
		cout << "Starting year " << year << " :" << endl;

		/* SAVING OUTPUTS PROCEDURE =================================================*/
		/* Saving computing statistics */
		if (year%10==0)
		{
			#if defined(__unix__) || defined(__linux__) || defined(linux) || defined(LINUX)
				fileStats << "Year " << year << ", VIRTUAL MEMORY used : " << getMemUsed("virtual") << endl;
				fileStats << "Year " << year << ", PHYSICAL MEMORY used : " << getMemUsed("physical") << endl;
			#elif defined(__APPLE__)
				if (KERN_SUCCESS != task_info(mach_task_self(), TASK_BASIC_INFO, (task_info_t)&t_info, &t_info_count)) { return -1; }
				fileStats << "Year " << year << ", RESIDENT SIZE : " << t_info.resident_size << endl;
				fileStats << "Year " << year << ", VIRTUAL MEMORY used : " << t_info.virtual_size << endl;

				mach_port = mach_host_self();
				count = sizeof(vm_stats) / sizeof(natural_t);
				if (KERN_SUCCESS == host_page_size(mach_port, &page_size) && KERN_SUCCESS == host_statistics64(mach_port, HOST_VM_INFO,(host_info64_t)&vm_stats, &count))
				{
					long long free_memory = (int64_t)vm_stats.free_count * (int64_t)page_size;
					long long used_memory = ((int64_t)vm_stats.active_count + (int64_t)vm_stats.inactive_count + (int64_t)vm_stats.wire_count) *  (int64_t)page_size;
					fileStats << "Year " << year << ", PHYSICAL FREE MEMORY : " << free_memory << endl;
					fileStats << "Year " << year << ", PHYSICAL USED MEMORY : " << used_memory << endl;
				}
			#endif

			time(&End);
			int TotTime = difftime(End,Start);
			fileStats << "Year " << year << ", COMPUTATION TIME : " << TotTime/3600 << "h " << (TotTime%3600)/60 << "m " << (TotTime%3600)%60 << "s" << endl;
		}

		/* omp_set_num_threads( simulMap->getGlobalParameters().getNoCPU() );
		#pragma omp parallel
		{*/
		/* Saving summarised array */
		if (summarised_array_saving_times.size() > 0 && summarised_array_saving_times.front() == year)
		{
			cout << "Saving rasters..." << endl;
			simulMap->SaveRasterAbund( file_of_params.getSavingDir(), year, file_of_params.getMask());
			/* remove saved time from list */
			summarised_array_saving_times.erase(summarised_array_saving_times.begin());
		}

		/* Saving simulation object */
		if (simul_objects_saving_times.size() > 0 && simul_objects_saving_times.front() == year)
		{
			cout << "Saving simulation object..." << endl;
			{
				// Create an output archive
				string objFileName = file_of_params.getSavingDir() + "SimulMap_" + boost::lexical_cast<string>(year) + ".sav";
				cout << objFileName.c_str() << endl;
				saveFATE(objFileName);
			}
			cout << "> done! " << endl;

			/* remove saved time from list */
			simul_objects_saving_times.erase(simul_objects_saving_times.begin());

			/* TEST EQUALITY */
			/*string objFileName = file_of_params.getSavingDir() + "SimulMap_" + boost::lexical_cast<string>(year) + ".sav";
			loadFATE(objFileName);
			assert(*test == *simulMap);*/
		}

		/* Do mask, climat, disturbances and climatic data changes ==================*/
		/* Do MASK change */
		if (mask_change_times.size() > 0 && mask_change_times.front() == year)
		{
			cout << "Doing MASK change..." << endl;
			simulMap->DoFileChange(mask_change_files.front(), "mask");

			/* Change mask name in file_of_params to create outputs rasters with the correct studied area */
			string strTmp; // tmp string to keep change filenames
			vector< string > newNameFiles; // vector of change filenames

			/* open newChangeFile */
			ifstream file(mask_change_files.front().c_str(), ios::in);
			if (file)
			{
				/* Read file line by line */
				while (file >> strTmp)
				{
					if (strTmp != "")
					{
						/* store new files */
						newNameFiles.push_back(strTmp);
						cout << "*** " << strTmp << endl;
					}
				}

				/* Close file */
				file.close();
			} else
			{
				cerr << "Impossible to open " << mask_change_files.front() << " file!" << endl;
				terminate();
			}
			file_of_params.setMask(newNameFiles[0]); // change mask name
			file_of_params.checkCorrectMasks(); // check that new mask is similar to the other simulation masks

			/* remove useless time and file from list */
			mask_change_times.erase(mask_change_times.begin());
			mask_change_files.erase(mask_change_files.begin());
		}

		/* Do habitat suitability change */
		if (simulMap->getGlobalParameters().getDoHabSuitability())
		{
			changeFile(year, "habSuit", habsuit_change_times, habsuit_change_files);
		}

		/* Do disturbances change */
		if (simulMap->getGlobalParameters().getDoDisturbances())
		{
			changeFile(year, "dist", dist_change_times, dist_change_files);
		}

		/* FIRE DISTURBANCE */
		if (simulMap->getGlobalParameters().getDoFireDisturbances())
		{
			/* Do fire change */
			changeFile(year, "fire", fire_change_times, fire_change_files);
			/* Do fire frequencies change */
			changeFreq(year, "fire", fire_freq_change_times, fire_freq_change_files);
		}

		/* DROUGHT DISTURBANCE */
		/* Do drought index change */
		if (simulMap->getGlobalParameters().getDoDroughtDisturbances() ||
		    simulMap->getGlobalParameters().getDoFireDisturbances())
		{
			changeFile(year, "drought", drought_change_times, drought_change_files);
		}

		/* ALIENS DISTURBANCE */
		if (simulMap->getGlobalParameters().getDoAliensIntroduction())
		{
			/* Do aliens introduction change */
			changeFile(year, "aliens", aliens_change_times, aliens_change_files);
			/* Do aliens introduction frequencies change */
			changeFreq(year, "aliens", aliens_freq_change_times, aliens_freq_change_files);
		}

		/* Check seeding parameters =================================================*/
		/* DISPERSAL MODULE */
		if (simulMap->getGlobalParameters().getDoDispersal())
		{
			if (seeding_duration > 0 && year < seeding_duration)
			{
				if (year % seeding_timestep == 0)
				{
					cout << "Seeding occurs this year..." << endl;
					seeding_on = true;
					simulMap->StartSeeding();
				} else
				{
					seeding_on = false;
					simulMap->StopSeeding();
				}
			}

			/* Stop seeding the last year of seeding */
			if (seeding_duration > 0 && year == seeding_duration)
			{
				cout << "End of seeding campain..." << endl;
				seeding_on = false;
				simulMap->StopSeeding();
			}
		}

		/* Run aliens introduction model ============================================*/
		if (simulMap->getGlobalParameters().getDoAliensIntroduction())
		{
			simulMap->DoAliensIntroduction(year);
		}

		/* Run drought disturbance model : PREVIOUS succession ======================*/
		if (simulMap->getGlobalParameters().getDoDroughtDisturbances())
		{
			cout << "Calculate drought disturbances..." << endl;
			simulMap->DoDroughtDisturbance_part1();
			cout << "Apply drought disturbances..." << endl;
			simulMap->DoDroughtDisturbance_part2("prev");
		}

		/*===========================================================================*/
		/* Run succession model */
		cout << "Do Succession..." << endl;
		simulMap->DoSuccession();
		/*===========================================================================*/

		/* Run drought disturbance model : POST succession */
		if (simulMap->getGlobalParameters().getDoDroughtDisturbances())
		{
			cout << "Apply drought disturbances..." << endl;
			simulMap->DoDroughtDisturbance_part2("post");
		}

		/* Run seeds dispersal model ================================================*/
		if (simulMap->getGlobalParameters().getDoDispersal() && !seeding_on)
		{
			cout << "Disperse seeds..." << endl;
			simulMap->DoDispersal();
		}

		/* Run disturbance model ====================================================*/
		if (simulMap->getGlobalParameters().getDoDisturbances())
		{
			cout << "Apply disturbances..." << endl;
			simulMap->DoDisturbance(year);
		}

		/* Run fire disturbance model ===============================================*/
		if (simulMap->getGlobalParameters().getDoFireDisturbances())
		{
			cout << "Apply fire disturbances..." << endl;
			simulMap->DoFireDisturbance(year);
		}
	} // end main simulation loop
	//} // end PRAGMA

	delete simulMap;

	/* End of Run */
	time(&End);
	int TotTime = difftime(End,Start);

	fileStats << "End of simul, COMPUTATION TIME : " << TotTime/3600 << "h " << (TotTime%3600)/60 << "m " << (TotTime%3600)%60 << "s" << endl;
	fileStats.close();

	cout 	<< "Process executed normally! It took "
	<< TotTime/3600 << "h " << (TotTime%3600)/60
	<< "m " << (TotTime%3600)%60 << "s." << endl;

	return 0;
}
