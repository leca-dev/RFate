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
	ALow, /*!< Low Abundance (< AA virtual individuals) */
	AMedium, /*!< Medium Abundance (< BB virtual individuals) */
	AHigh, /*!< High Abundance (> CC virtual individuals) */
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
 *	100%. All involved fraction are integer (input and output).
 *
 *	\param f1 : the first fraction to be completed
 *	\param f2 : an optional second fraction
 *	\return integer fraction corresponding to 100% - (f1 + f2)
 */
int getLeavingFract(int f1, int f2);

/*!
 *	\brief Convert an integer percentage into a double fraction between 0 and 1
 *
 *	This function returns a number between 0 and 1 corresponding to a given
 *	integer percentage.
 *
 *	\param fract : an integer percentage
 *	\return a number between 0 and 1
 */
double IntToDouble(int fract);

/*!
 *	\brief Convert a double fraction between 0 and 1 into an integer percentage
 *
 *	This function returns a number between 0 and 100 corresponding to a given
 *	integer percentage.
 *
 *	\param fract : a double fraction between 0 and 1
 *	\return a number between 0 and 100
 */
int DoubleToInt( double fract);

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


#endif //FGUTILS_H_INCLUDED
