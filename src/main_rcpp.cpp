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
/*               FUNCTIONAL ATTRIBUTES IN TERRESTRIAL ECOSYSTEMS              */
/*                                 Version 1.0                                */
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
  /* to get virtual and physical memory information */
  #include "sys/types.h" // Unix-Mac
  #include "sys/sysinfo.h" // Unix
  
  /* to get CPU information */
  #include "sys/times.h" // Unix
  #include "sys/vtimes.h" // Unix
  
	#include "gdal.h"
	#include <ogr_spatialref.h>
#elif defined(__APPLE__)
  /* to get virtual and physical memory information */
  #include <mach/mach.h>  // Mac
  #include <mach/vm_statistics.h> // Mac
  #include <mach/mach_types.h> // Mac
  #include <mach/mach_init.h> // Mac
  #include <mach/mach_host.h> // Mac
#else
  /* to get virtual and physical memory information */
  #include "windows.h" // Windows
  #include "psapi.h" // Windows
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
	// /* to get virtual and physical memory information */
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
				if (strncmp(line, "VmSize:", 7) == 0)
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

	
 	/* to get CPU information */
	static clock_t lastCPU, lastSysCPU, lastUserCPU;
	static int numProcessors = 0;
	
	double getCpuUsed()
	{
	  struct tms timeSample;
	  clock_t now = times(&timeSample);
	  double percent = 0.0;
	  
	  if (now <= lastCPU || timeSample.tms_stime < lastSysCPU || timeSample.tms_utime < lastUserCPU)
	  { //Overflow detection. Just skip this value.
	    percent = -1.0;
	  } else
	  {
	    percent = (timeSample.tms_stime - lastSysCPU) + (timeSample.tms_utime - lastUserCPU);
	    percent /= (now - lastCPU);
	    percent /= numProcessors;
	    percent *= 100;
	  }
	  lastCPU = now;
	  lastSysCPU = timeSample.tms_stime;
	  lastUserCPU = timeSample.tms_utime;
	  return percent;
	}
#elif defined(__APPLE__)
	/* to get virtual and physical memory information */

	struct task_basic_info t_info; // Mac
	mach_msg_type_number_t t_info_count = TASK_BASIC_INFO_COUNT; // Mac
#else
	/* to get virtual and physical memory information */
	int getMemUsed(string typeMEM)
	{ //Note: this value is in KB!
	  PROCESS_MEMORY_COUNTERS_EX pmc;
	  GetProcessMemoryInfo(GetCurrentProcess(), (PROCESS_MEMORY_COUNTERS*)&pmc, sizeof(pmc));
	  SIZE_T virtualMemUsedByMe = pmc.PrivateUsage;
	  SIZE_T physMemUsedByMe = pmc.WorkingSetSize;

	  if (strcmp(typeMEM.c_str(),"virtual") == 0)
	  {
	    // return static_cast<int>(pmc.PrivateUsage);
	    return static_cast<int>(virtualMemUsedByMe);
	  } else if (strcmp(typeMEM.c_str(),"physical") == 0)
	  {
	    // return static_cast<int>(pmc.WorkingSetSize);
	    return static_cast<int>(physMemUsedByMe);
	  }
	  return -1;
	}

	/* to get CPU information */
	static ULARGE_INTEGER lastCPU, lastSysCPU, lastUserCPU;
	static int numProcessors;
	static HANDLE self;
	double getCpuUsed()
	{
	  FILETIME ftime, fsys, fuser;
	  ULARGE_INTEGER now, sys, user;
	  double percent;

	  GetSystemTimeAsFileTime(&ftime);
	  memcpy(&now, &ftime, sizeof(FILETIME));

	  GetProcessTimes(self, &ftime, &ftime, &fsys, &fuser);
	  memcpy(&sys, &fsys, sizeof(FILETIME));
	  memcpy(&user, &fuser, sizeof(FILETIME));
	  percent = (sys.QuadPart - lastSysCPU.QuadPart) + (user.QuadPart - lastUserCPU.QuadPart);
	  percent /= (now.QuadPart - lastCPU.QuadPart);
	  percent /= numProcessors;
	  lastCPU = now;
	  lastUserCPU = user;
	  lastSysCPU = sys;

	  return percent * 100;
	}
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
		logg.warning("Compression failed for ", objFileName);
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
		logg.error("!!! File of SAVED STATE does not exists or can't be opened!");
	}
}

void changeFile(int year, string change_type, vector<int>& change_times,
								vector<string>& change_files)
{
	if (change_times.size() > 0 && change_times.front() == year)
	{
		logg.info("Doing ", change_type, " masks change...");
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
		logg.info("Doing ", change_type, " frequencies change...");
		simulMap->DoFreqChange(freq_change_files.front(), change_type);
		/* remove useless time and file from list */
		freq_change_times.erase(freq_change_times.begin());
		freq_change_files.erase(freq_change_files.begin());
	}
}


/*============================================================================*/
//' FATE Wrapper
//'
//' This function runs a \code{FATE} dynamical landscape vegetation simulation.
//'
//' @param simulParam a \code{string} corresponding to the name of a 
//' parameter file that will be contained into the \code{PARAM_SIMUL} folder 
//' of the \code{FATE} simulation
//' @param no_CPU (\emph{optional}) default \code{1}. \cr The number of 
//' resources that can be used to parallelize the simulation
//' @param verboseLevel (\emph{optional}) default \code{0}. \cr The logger 
//' verbose level : a \code{FATE} simulation can render different levels of 
//' information (from \code{0} to \code{4}, see 
//' \href{FATE#details}{\code{Details}}).
//' 
//' @details This function allows to run a vegetation simulation with the 
//' \code{FATE} model, based on a simulation folder and a species simulation 
//' parameter file.
//' 
//' A \code{FATE} simulation can be parallelized, using the \code{no_CPU} 
//' parameter, given that the user machine is multi-core !
//' 
//' Quantity of informations are rendered by the software into the \code{R} 
//' console, and the \code{verboseLevel} parameter allows to filter which 
//' level of information is printed :
//' 
//' \describe{
//'   \item{0. }{shows any message}
//'   \item{1. }{shows any message, except debug}
//'   \item{2. }{shows only warning and error messages}
//'   \item{3. }{shows only error messages}
//'   \item{4. }{mute}
//' }
//'
//' @return None
//'
//' @examples \dontrun{FATE()}
//'
//' @author Damien Georges, Isabelle Boulangeat, Maya Guéguen
//'
//' @export
// [[Rcpp::export]]
int FATE(std::string simulParam, int no_CPU = 1, int verboseLevel = 0)
{
	/*==========================================================================*/
	/* Initialization */

	// Logger initialization
	logg.configure(verboseLevel);  // Set logger verbose level
	// Number of CPUs used for computation
	if (no_CPU > 1)
	{
		logg.info("PARALLEL VERSION : programme will run on ", no_CPU, " CPU.");
		omp_set_num_threads(no_CPU);  // Set number of CPUs
	}
	// Timer initialization
	time_t Start, End;
	time(&Start);  // Start timer

	logg.info("*********************************************\n",
						"   WELCOME TO FATE SIMULATION RUNTIME\n",
						"*********************************************\n");

	/*==========================================================================*/
	/* create the simulation parameter object that stores paths to all needed
	parameters files */

	logg.info("This simulation will be based on ", simulParam,
						" parameters file.");
	FOPL file_of_params(simulParam);
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
		
		/* NUMBER OF PROCESSORS */
		{
		  FILE* file = fopen("/proc/cpuinfo", "r");
		  struct tms timeSample;
		  char line[128];
		  
		  lastCPU = times(&timeSample);
		  lastSysCPU = timeSample.tms_stime;
		  lastUserCPU = timeSample.tms_utime;
		  while (fgets(line, 128, file) != NULL)
		  {
		    if (strncmp(line, "processor", 9) == 0)
		    {
		      numProcessors++;
		    }
		  }
		  fclose(file);
		}

		fileStats << "TOTAL VIRTUAL MEMORY : " << totalVirtualMem << endl;
		fileStats << "TOTAL PHYSICAL MEMORY : " << totalPhysMem << endl;
		fileStats << "NUMBER OF PROCESSORS : " << numProcessors << endl;
		fileStats << "Initial VIRTUAL MEMORY USED : " << getMemUsed("virtual") << endl;
		fileStats << "Initial PHYSICAL MEMORY USED : " << getMemUsed("physical") << endl;
		fileStats << "Initial PERCENTAGE OF CPU USED : " << getCpuUsed() << endl;
  #elif defined(__APPLE__)
		/* Memory consuming measurement */
		vm_size_t page_size; // Mac
		mach_port_t mach_port; // Mac
		mach_msg_type_number_t count; // Mac
		vm_statistics64_data_t vm_stats; // Mac
  #else
		MEMORYSTATUSEX memInfo;
		memInfo.dwLength = sizeof(MEMORYSTATUSEX);
		GlobalMemoryStatusEx(&memInfo);
		DWORDLONG totalVirtualMem = memInfo.ullTotalPageFile;
		/* Note: The name "TotalPageFile" is a bit misleading here.
		 * In reality this parameter gives the "Virtual Memory Size",
		 * which is size of swap file plus installed RAM.
		 */
		DWORDLONG totalPhysMem = memInfo.ullTotalPhys;
		
		/* NUMBER OF PROCESSORS */
		{
		  SYSTEM_INFO sysInfo;
		  FILETIME ftime, fsys, fuser;

		  GetSystemInfo(&sysInfo);
		  numProcessors = sysInfo.dwNumberOfProcessors;

		  GetSystemTimeAsFileTime(&ftime);
		  memcpy(&lastCPU, &ftime, sizeof(FILETIME));

		  self = GetCurrentProcess();
		  GetProcessTimes(self, &ftime, &ftime, &fsys, &fuser);
		  memcpy(&lastSysCPU, &fsys, sizeof(FILETIME));
		  memcpy(&lastUserCPU, &fuser, sizeof(FILETIME));
		}
		
		fileStats << "TOTAL VIRTUAL MEMORY : " << totalVirtualMem << endl;
		fileStats << "TOTAL PHYSICAL MEMORY : " << totalPhysMem << endl;
		fileStats << "NUMBER OF PROCESSORS : " << numProcessors << endl;
		fileStats << "Initial VIRTUAL MEMORY USED : " << getMemUsed("virtual") << endl;
		fileStats << "Initial PHYSICAL MEMORY USED : " << getMemUsed("physical") << endl;
		fileStats << "Initial PERCENTAGE OF CPU USED : " << getCpuUsed() << endl;
	#endif

	/*==========================================================================*/
	/* check if a saving of an old simulation is given or if we start a new one
	from scratch */

	// the map on which the whole simulation process is based on
	if (file_of_params.getSavedState() == "0")
	{ // start from scratch
		logg.info("Starting from scratch...");
		simulMap = new SimulMap(file_of_params);
	} else
	{ // start from previous simulation state
		logg.info("Loading previous simulation outputs stored in ",
							file_of_params.getSavedState(), " file...");
		loadFATE(file_of_params.getSavedState());
		logg.info("> done!\n*** UPDATE simulation files...");

		// update the simulation parameters
		simulMap->UpdateSimulationParameters(file_of_params);

		// FROM SAVED STATE BUT WITH NEW FG file_of_params
		logg.info("*** REBUILDING Global simulation parameters...");
		GSP glob_params = GSP(file_of_params.getGlobSimulParams());
		int noFG = glob_params.getNoFG();
		logg.info("*** REBUILDING Functional groups...");
		vector<FG> fg_vec_tmp;
		for (int fg_id=0; fg_id<noFG; fg_id++)
		{
			FG fg_tmp = FG(glob_params, file_of_params, fg_id);
			fg_tmp.show();
			fg_vec_tmp.push_back(fg_tmp);
		}
		simulMap->setFGparams(fg_vec_tmp);
	}

	simulMap->getGlobalParameters().setNoCPU(no_CPU);
	logg.info("\n*** NoCPU = ", simulMap->getGlobalParameters().getNoCPU());
	fileStats << "Number of CPU used : "
						<< simulMap->getGlobalParameters().getNoCPU()
						<< endl;

	/*=============================================================================*/
	/* get all needed parameters */

	/* timing parameters */
	logg.info("Getting timing parameters...");
	int simul_duration = simulMap->getGlobalParameters().getSimulDuration();
	bool seeding_on = false;
	int seeding_duration = simulMap->getGlobalParameters().getSeedingDuration();
	int seeding_timestep = simulMap->getGlobalParameters().getSeedingTimeStep();

	/* saving parameters */
	logg.info("Getting saving parameters...");
	vector< int > summarised_array_saving_times = ReadTimingsFile( file_of_params.getSavingTimesMaps() );
	vector< int > simul_objects_saving_times = ReadTimingsFile( file_of_params.getSavingTimesObjects() );

	/* study area change parameters */
	logg.info("Getting MASK timing parameters...");
	vector< int > mask_change_times = ReadTimingsFile( file_of_params.getMaskChangemaskYears() );
	vector< string > mask_change_files = file_of_params.getMaskChangemaskFiles();

	/* MODULES change parameters */
	if (simulMap->getGlobalParameters().getDoHabSuitability())
	{
		logg.info("Getting HS timing parameters...");
	}
	vector< int > habsuit_change_times = ReadTimingsFile( file_of_params.getHabSuitChangemaskYears() );
	vector< string > habsuit_change_files = file_of_params.getHabSuitChangemaskFiles();
	if (simulMap->getGlobalParameters().getDoDisturbances())
	{
		logg.info("Getting dist timing parameters...");
	}
	vector< int > dist_change_times = ReadTimingsFile( file_of_params.getDistChangemaskYears() );
	vector< string > dist_change_files = file_of_params.getDistChangemaskFiles();
	if (simulMap->getGlobalParameters().getDoFireDisturbances())
	{
		logg.info("Getting fire timing parameters...");
	}
	vector< int > fire_change_times = ReadTimingsFile( file_of_params.getFireChangemaskYears() );
	vector< string > fire_change_files = file_of_params.getFireChangemaskFiles();
	vector< int > fire_freq_change_times = ReadTimingsFile( file_of_params.getFireChangefreqYears() );
	vector< string > fire_freq_change_files = file_of_params.getFireChangefreqFiles();
	if (simulMap->getGlobalParameters().getDoDroughtDisturbances())
	{
		logg.info("Getting drought index timing parameters...");
	}
	vector< int > drought_change_times = ReadTimingsFile( file_of_params.getDroughtChangemaskYears() );
	vector< string > drought_change_files = file_of_params.getDroughtChangemaskFiles();
	if (simulMap->getGlobalParameters().getDoAliensIntroduction())
	{
		logg.info("Getting aliens timing parameters...");
	}
	vector< int > aliens_change_times = ReadTimingsFile( file_of_params.getAliensChangemaskYears() );
	vector< string > aliens_change_files = file_of_params.getAliensChangemaskFiles();
	vector< int > aliens_freq_change_times = ReadTimingsFile( file_of_params.getAliensChangefreqYears() );
	vector< string > aliens_freq_change_files = file_of_params.getAliensChangefreqFiles();

	/*=============================================================================*/
	/* Simulation main loop */
	for (int year=0; year<=simul_duration; year++)
	{
		logg.info("\nStarting year ", year, " :");

		/* SAVING OUTPUTS PROCEDURE =================================================*/
		/* Saving computing statistics */
		if (year%10==0)
		{
			#if defined(__unix__) || defined(__linux__) || defined(linux) || defined(LINUX)
				fileStats << "Year " << year << ", VIRTUAL MEMORY USED : " << getMemUsed("virtual") << endl;
				fileStats << "Year " << year << ", PHYSICAL MEMORY USED : " << getMemUsed("physical") << endl;
				fileStats << "Year " << year << ", PERCENTAGE OF CPU USED : " << getCpuUsed() << endl;
      #elif defined(__APPLE__)
				if (KERN_SUCCESS != task_info(mach_task_self(), TASK_BASIC_INFO, (task_info_t)&t_info, &t_info_count)) { return -1; }
				fileStats << "Year " << year << ", RESIDENT SIZE : " << t_info.resident_size << endl;
				fileStats << "Year " << year << ", VIRTUAL MEMORY USED : " << t_info.virtual_size << endl;

				mach_port = mach_host_self();
				count = sizeof(vm_stats) / sizeof(natural_t);
				if (KERN_SUCCESS == host_page_size(mach_port, &page_size) &&
            KERN_SUCCESS == host_statistics64(mach_port, HOST_VM_INFO,(host_info64_t)&vm_stats, &count))
				{
					long long free_memory = (int64_t)vm_stats.free_count * (int64_t)page_size;
					long long used_memory = ((int64_t)vm_stats.active_count + (int64_t)vm_stats.inactive_count + (int64_t)vm_stats.wire_count) *  (int64_t)page_size;
					fileStats << "Year " << year << ", PHYSICAL FREE MEMORY : " << free_memory << endl;
					fileStats << "Year " << year << ", PHYSICAL USED MEMORY : " << used_memory << endl;
				}
      #else
				fileStats << "Year " << year << ", VIRTUAL MEMORY USED : " << getMemUsed("virtual") << endl;
				fileStats << "Year " << year << ", PHYSICAL MEMORY USED : " << getMemUsed("physical") << endl;
				fileStats << "Year " << year << ", PERCENTAGE OF CPU USED : " << getCpuUsed() << endl;
			#endif

			time(&End);
			int TotTime = difftime(End,Start);
			fileStats << "Year " << year << ", COMPUTATION TIME : " << TotTime/3600 << "h " << (TotTime%3600)/60 << "m " << (TotTime%3600)%60 << "s" << endl;
		}

		/* Saving summarised array */
		if (summarised_array_saving_times.size() > 0 && summarised_array_saving_times.front() == year)
		{
			logg.info("Saving rasters...");
			simulMap->SaveRasterAbund( file_of_params.getSavingDir(), year, file_of_params.getMask());
			/* remove saved time from list */
			summarised_array_saving_times.erase(summarised_array_saving_times.begin());
		}

		/* Saving simulation object */
		if (simul_objects_saving_times.size() > 0 && simul_objects_saving_times.front() == year)
		{
			logg.info("Saving simulation object...");
			{
				// Create an output archive
				string objFileName = file_of_params.getSavingDir() + "SimulMap_" + boost::lexical_cast<string>(year) + ".sav";
				logg.debug(objFileName.c_str());
				saveFATE(objFileName);
			}
			logg.info("> done!");

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
			logg.info("Doing MASK change...");
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
						logg.info("*** ", strTmp);
					}
				}

				/* Close file */
				file.close();
			} else
			{
				logg.error("Impossible to open ", mask_change_files.front(), " file!");
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
		/* SEEDING MODULE */
		if (simulMap->getGlobalParameters().getDoDispersal())
		{
			if (seeding_duration > 0 && year < seeding_duration)
			{
				if (year % seeding_timestep == 0)
				{
					logg.info("Seeding occurs this year...");
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
				logg.info("End of seeding campain...");
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
			logg.info("Calculate drought disturbances...");
			simulMap->DoDroughtDisturbance_part1();
			logg.info("Apply drought disturbances...");
			simulMap->DoDroughtDisturbance_part2("prev");
		}

		/*===========================================================================*/
		/* Run succession model */
		logg.info("Do Succession...");
		simulMap->DoSuccession();
		/*===========================================================================*/

		/* Run drought disturbance model : POST succession */
		if (simulMap->getGlobalParameters().getDoDroughtDisturbances())
		{
			logg.info("Apply drought disturbances...");
			simulMap->DoDroughtDisturbance_part2("post");
		}

		/* Run seeds dispersal model ================================================*/
		if (simulMap->getGlobalParameters().getDoDispersal() && !seeding_on)
		{
			logg.info("Disperse seeds...");
			simulMap->DoDispersal();
		}

		/* Run disturbance model ====================================================*/
		if (simulMap->getGlobalParameters().getDoDisturbances())
		{
			logg.info("Apply disturbances...");
			simulMap->DoDisturbance(year);
		}

		/* Run fire disturbance model ===============================================*/
		if (simulMap->getGlobalParameters().getDoFireDisturbances())
		{
			logg.info("Apply fire disturbances...");
			simulMap->DoFireDisturbance(year);
		}
	} // end main simulation loop
	//} // end PRAGMA

	delete simulMap;

	/* End of Run */
	time(&End);
	int TotTime = difftime(End,Start);

	fileStats << "End of simul, COMPUTATION TIME : "
						<< TotTime/3600 << "h "
						<< (TotTime%3600)/60 << "m "
						<< (TotTime%3600)%60 << "s"
						<< endl;
	fileStats.close();

	logg.info("Process executed normally! It took ", TotTime/3600, "h ",
						(TotTime%3600)/60, "m ", (TotTime%3600)%60, "s.");

	return 0;
}
