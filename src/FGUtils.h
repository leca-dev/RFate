/*============================================================================*/
/*                                FGUtils Class                               */
/*============================================================================*/

/*!
 * \file FGUtils.h
 * \brief Declaration of categorical variables and routines involved in FATE
 * \author Damien Georges, Maya Gueguen
 * \version 1.0
 * \date 2018
 */

#ifndef FGUTILS_H
#define FGUTILS_H

#include <iostream>
#include <string>
#include <cstring>
#include <fstream>
#include <cstdio>
#include <vector>

/*#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>*/

#include "Spatial.h"
#include "Logger.h"

using namespace std;

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/*-------------------------------------------*/
/* Categorical variables definition ---------*/
/*-------------------------------------------*/

/* Available values definition for FGs & environment parameters */

/*!
 * \enum Abund
 * \brief Factorial PFGs Abundances
 */
enum Abund
{
	ANone, /*!< No individuals */
	ALow, /*!< Low Abundance (< 2 000 000 virtual individuals) */
	AMedium, /*!< Medium Abundance (< 7 000 000 virtual individuals) */
	AHigh, /*!< High Abundance (> 7 000 000 virtual individuals) */
	Acount
};

/*!
 * \enum PoolType
 * \brief Different Propagules Pool type
 */
enum PoolType
{
	DormantP, /*!< Dormant Seed Pool */
	ActiveP, /*!< Active Seed Pool */
	PTcount
};

/*!
 * \enum Resource
 * \brief Factorial Light Resources
 */
enum Resource
{
	RLow, /*!< Shaded area (more than 9 000 000 virtual individuals above */
	RMedium, /*!< Semi-shaded area (more than 6 000 000 virtual individuals above */
	RHigh, /*!< Lighted area (less than 6 000 000 virtual individuals above */
	Rcount
};

/*!
 * \enum LifeStage
 * \brief PFGs individuals possible Life Stages
 */
enum LifeStage
{
	Propagule, /*!< Propagule Stage (= Seed) */
	Germinant, /*!< 1st state of Plants */
	Immature, /*!< Immature Stage */
	Mature, /*!< Mature Stage */
	LScount
};

/*!
 * \enum Fract
 * \brief Numerical fraction used in Modelling
 */
enum Fract
{
	PC00, /*!< 0% */
	PC10, /*!< 10% */
	PC20, /*!< 20% */
	PC30, /*!< 30% */
	PC40, /*!< 40% */
	PC50, /*!< 50% */
	PC60, /*!< 60% */
	PC70, /*!< 70% */
	PC80, /*!< 80% */
	PC90, /*!< 90% */
	PC100, /*!< 100% */
	Fcount
};

/*!
 * \enum Fract2
 * \brief Factorial fraction used in Modeling
 */
enum Fract2
{
	F2None, /*!< = 0% */
	F2Low, /*!< = 10% */
	F2Medium, /*!< = 50% */
	F2High, /*!< = 90% */
	F2All, /*!< = 100% */
	F2count
};

/*!
 * \enum DistFate
 * \brief PFG behaviour in response to Disturbances
 */
enum DistFate
{
	Kill, /*!< Plants die */
	Unaff, /*!< Plants are not affected */
	Respr, /*!< Plants resprout (a parameter define resprouting age */
	DFcount
};

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/*-------------------------------------------*/
/* Routine functions definition -------------*/
/*-------------------------------------------*/

/*!
 *	\brief Calculate the complement from a given fraction to 100%
 *
 *	This function returns the complement of one or two given fractions to reach
 *	100%. All involved fraction are categorical (input and output).
 *
 *	\param f1 : the first fraction to be completed
 *	\param f2 : an optional second fraction
 *	\return categorical fraction corresponding to 100% - (f1 + f2)
 */
Fract getLeavingFract(Fract f1, Fract f2 = Fract(0));

/*!
 *	\brief Convert a categorical Fract fraction into scalar one
 *
 *	This function returns a number between 0 and 1 corresponding to a given
 *	categorical fraction.
 *
 *	\param fract : a categorical fraction of class Fract
 *	\return a number between 0 and 1
 */
double FractToDouble(Fract fract);

/*!
 *	\brief Convert a categorical Fract2 fraction into scalar one
 *
 *	This function returns a number between 0 and 1 corresponding to a given
 *	categorical fraction.
 *
 *	\param fract : a categorical fraction of class Fract2
 *	\return a number between 0 and 1
 */
double FractToDouble(Fract2 fract);

/*!
 *	\brief Convert a scalar fraction fraction into categorical one
 *
 *	This function returns the closest categorical fraction to a given number
 * between 0 and 1.
 *
 *	\param d : a 0-1 scalar number
 *	\return a categorical fraction of class Fract
 */
Fract DoubleToFract(double d);

/*!
 *	\brief Convert a categorical light resources into scalar one
 *
 *	This function returns the closest categorical fraction to a given number
 * between 0 and 1.
 *
 *	\param d : a 0-1 scalar number
 *	\return a categorical fraction of class Fract
 */
double ResourceToDouble(Resource light);

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/*!
 *	\brief Check if a given string is recognised as a tag flag
 *
 *	This function will check if a given string is or not a FATE flag. This is
 * useful for reading correctly input parameter files.
 *
 *	\param strTmp : the string to analyze
 *	\return true if string is known as a tag, false otherwise
 */
bool IsATag(string strTmp);

/*!
 *	\brief Extract parameters from a parameter text file
 *
 *	This function attempts to extract parameters from a given parameter file.
 *
 *	\param paramFile : path to a text file containing simulation parameters
 *	\param flag : the flag of wanted parameters
 *	\param sepFlag : a string separating a set of parameters from each other
 *	\return vector of recovered parameters (as string). An empty list is returned
 * if wanted parameters are not in given file
 */
vector<string> ReadParamsWithinFile(string paramFile, string flag, string sepFlag="--");

/*!
 *	\brief Extract times from a timing parameter text file
 *
 *	This function attempts to extract times from a given parameter file.
 *
 *	\param paramFile : path to a text file containing simulation timing parameters
 *	\return vector of times (as integers)
 */
vector< int > ReadTimingsFile(string paramFile);

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/*!
 *	\brief Extract coordinates from an ASCII file
 *
 *	This function reads the header of a given ASCII file and transform it
 *	in a FATEHDD Coordinates object.
 *
 *	\param file_name : path to an .asc file
 *	\return a Coordinates object filled according to ASCII header
 */
Coordinates<double> ReadAsciiCoordinates( string file_name );

/*!
 *	\brief Extract coordinates from a TIF or IMG file
 *
 *	This function reads the header of a given TIF or IMG file and transform it
 *	in a FATEHDD Coordinates object.
 *
 *	\param file_name : path to an .tif or .img file
 *	\return a Coordinates object filled according to GDAL informations
 */
Coordinates<double> ReadRasterCoordinates( string file_name );

/*!
 *	\brief Select the function of coordinates extraction depending on the
 * extension of a raster file
 *
 *	This function reads the extension of a given raster file and calls the
 * corresponding function to obtain its coordinates and transform it in a
 * FATEHDD Coordinates object.
 *
 *	\param file_name : path to a .asc, .tif or .img file
 *	\return a Coordinates object
 */
Coordinates<double> ReadCoordinates( string file_name );

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/*!
 *	\brief Test folder existence
 *
 *	This function checks if a folder exists on hard drive. If not, the folder is
 * created.
 *
 *	\param param : name of the corresponding parameter
 * \param dir_name : path to a folder on hard drive
 * \param optional : is the folder optional (true) or not
 */
void testDirExist(const string& param, const string& dir_name, const bool& optional);

/*!
 *	\brief Test file existence
 *
 *	This function checks if a file exists on hard drive, and if it the case, if
 * it can be opened. If the file is related to an optional module (optional =
 * true), it can be missing. Otherwise (optional = false), if the file does not
 * exist, an error will be thrown.
 *
 *	\param param : name of the corresponding parameter
 * \param file_name : path to a file on hard drive
 * \param optional : is the file optional (true) or not
 */
void testFileExist(const string& param, const string& file_name, const bool& optional);

/*!
 *	\brief Test file existence
 *
 *	This function checks if a bunch of files exist on hard drive, and if it the
 * case, if they can be opened. If the files are related to an optional module
 * (optional = true), they can be missing. Otherwise (optional = false), if the
 * files do not exist, an error will be thrown.
 *
 *	\param param : name of the corresponding parameter
 * \param vector_name : vector of paths to files on hard drive
 * \param optional : is the file is optional (true) or not
 */
void testFileExist(const string& param, vector<string> vector_name, const bool& optional);

/*!
 *	\brief Test file existence
 *
 *	This function checks if a bunch of files exist on hard drive, and if it the
 * case, if they can be opened. If the files are related to an optional module
 * (optional = true), they can be missing. Otherwise (optional = false), if the
 * files do not exist, an error will be thrown.
 *
 *	\param param : name of the corresponding parameter
 * \param vector_name : vector of paths to files on hard drive
 * \param optional : is the file is optional (true) or not
 */
void testFileExist_changeFile(const string& param, vector<string> vector_name, const bool& optional);

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/*!
 *	\brief Convert an integer into a specific enum
 *
 *	This function is a wrapper to convert integer into one of FATEHD
 *	factorial variables (e.g Fract, Fract2, Resource,.. ).
 *
 *	\param key_param : parameter name
 * \param val : parameter value
 * \param key_enum : enum name
 * \param max_val : maximum value allowed
 *	\return value of asked Type.
 */
template<typename T>
T convert_int_to_enum(const string& key_param, int val, const string& key_enum, int max_val)
{
	if (val < 0 || val > max_val)
	{
		logg.error("!!! Wrong parameter given for <", key_param, ">.\n",
							 "Must be a number between 0 and ", max_val - 1,
							 " corresponding to a level of enum ", key_enum, ". Please check!");
	}
	return static_cast<T>(val);
}

/*!
 *	\brief Convert a vector of integers into a vector of specific enum
 *
 *	This function is a wrapper to convert integer into one of FATEHD
 *	factorial variables (e.g Fract, Fract2, Resource,.. ).
 *
 *	\param key_param : parameter name
 * \param vect : vector of parameter values
 * \param key_enum : enum name
 * \param max_val : maximum value allowed
 *	\return vector of asked Type.
 */
template<typename T>
vector<T> convert_int_to_enum(const string& key_param, vector<int> vect, const string& key_enum, int max_val)
{
	vector<T> result;
	for (unsigned i=0; i<vect.size(); i++)
	{
		result.push_back( convert_int_to_enum<T>(key_param, vect[i], key_enum, max_val) );
	}
	return result;
}

#endif //FGUTILS_H_INCLUDED
