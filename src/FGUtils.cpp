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

#include "FGUtils.h"
#include "Params.h"

#include <filesystem>

namespace fs = std::filesystem;
using namespace std;


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Int / double conversion functions                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

int getLeavingFract(int f1, int f2)
{
  return ( 100 - ( f1 + f2 ) );
}

double IntToDouble(int fract)
{
  return ( static_cast<double>(fract) / 100 );
}

int DoubleToInt(double fract)
{
  return ( min(static_cast<int>(ceil(fract * 100)), 100) );
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

double ResourceToDouble(Resource light)
{
	double Resource_Real [ Rcount ] = {1.0, 2.0, 3.0};
	return Resource_Real[static_cast<int>(light)];
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Reading input files utilities                                                                   */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

bool IsATag(string strTmp)
{
	bool test = false;
	if (strTmp.compare(0,1,"-") == 0 && strTmp.compare(1,1,"-") == 0)
	{ /* this line is a tag */
		test = true;
	}
	return test;
} // end of IsATag(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

// TODO (georgeda#2#): make this function able to read several parameters separated by a comma
vector<string> ReadParamsWithinFile(string paramFile, string flag, string sepFlag)
{
	/* File existence checking */
	ifstream file(paramFile.c_str(), ios::in);
	if (!file)
	{
		logg.error("Impossible to open ", paramFile, " file! (parameters)");
	}
	vector<string>  res(1,"0");
	string strTmp = "", strTarget = sepFlag + flag + sepFlag;
	/* Read file line by line until target was found*/
	while (!(strTmp == strTarget) && !file.eof())
	{
		getline(file, strTmp);
	}

	/* Keep interesting part of file*/
	if (strTmp == strTarget)
	{
		getline(file, strTmp);
		if (strTmp.length() == 0)
		{
			logg.warning(flag, " parameter NOT recovered");
		} else
		{
			res.clear();
			/* Get following lines */
			while (!IsATag(strTmp) && !file.eof())
			{
				if (strTmp != "")
				{
					res.push_back(strTmp);
				}
				getline(file, strTmp);
			}
			logg.debug(flag, " parameter recovered");
		}
	} else
	{
		//res.resize(1,""); /* return vector with an empty string  */
		logg.warning(flag, " parameter NOT recovered");
	}
	/* Close file */
	file.close();
	/* return list of paths */
	return res;
} // end of ReadParamsWithinFile(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

vector< int > ReadTimingsFile(string paramFile)
{
	if (paramFile != "" && paramFile != "0" )
	{ // if empty return empty vector
		/* File existence checking */
		ifstream file(paramFile.c_str(), ios::in);
		if (!file)
		{
			logg.error("Impossible to open ", paramFile, " file! (timing)");
		}
		vector< int > out(0); // initialise output as a 0 length vector
		int intTmp; // temp integer
		while (!file.eof())
		{
			file >> intTmp;
			out.push_back(intTmp);
		}
		/* Close file */
		file.close();
		/* return times vector */
		return out;
	} else
	{
		return vector< int >(0);
	}
} // end of ReadTimingsFile(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Spatial functions utilities                                                                     */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Coordinates<double> ReadCoordinates( string file_name )
{
	fs::path file_name_path(file_name.c_str());
	if (file_name_path.extension() != ".asc" &&
			file_name_path.extension() != ".img" &&
			file_name_path.extension() != ".tif")
	{
		logg.error("!!! The file extension (", file_name_path.extension(),
		 					 ") is not taken into account!\n",
							 "!!! Please use either .img or .tif (or .asc) files!");
	}
	if (file_name_path.extension()==".asc")
	{ // ASCII file
		return Coordinates<double>(ReadAsciiCoordinates(file_name));
	} else// if (file_name_path.extension()==".img" || file_name_path.extension()==".tif")
	{ // IMG or TIF file
		return Coordinates<double>(ReadRasterCoordinates(file_name));
	}
} // end of ReadCoordinates(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Coordinates<double> ReadAsciiCoordinates( string file_name )
{
	/* File existence checking */
	ifstream file(file_name.c_str(), ios::in);
	if (!file)
	{
		logg.error("Impossible to open ", file_name, " file! (coordinates)");
	}
	double xmin, xres, ymin, ncols, nrows, nodata;

	/* read ascii file header */
	for (unsigned i=0; i<6; i++)
	{
		string strTmp;
		file >> strTmp;
		if (strTmp == "NCOLS"){ file >> ncols; }
		else if (strTmp == "NROWS"){ file >> nrows; }
		else if (strTmp == "XLLCORNER"){ file >> xmin; }
		else if (strTmp == "YLLCORNER"){ file >> ymin; }
		else if (strTmp == "CELLSIZE"){ file >> xres; }
		else if (strTmp == "NODATA_value"){ file >> nodata; }
	} // end of loop for header read

	/* close file */
	file.close();

	/* fill the other params */
	//double yres = xres;
	//double xmax = xmin + ncols * xres;
	//double ymax = ymin + nrows * yres;

	/* Create and return the output object */
	return Coordinates<double>(xmin,xmin + ncols * xres,xres, ymin,ymin + nrows * xres,xres);
} // end of ReadCoordinates(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Coordinates<double> ReadRasterCoordinates( string file_name )
{
	GDALAllRegister();

	/* Open the source file */
	GDALDatasetH rasInput = GDALOpen( file_name.c_str(), GA_ReadOnly );
	//GDALDataset *rasInput = (GDALDataset *) GDALOpen( file_name.c_str(), GA_ReadOnly );
	CPLAssert( rasInput != NULL );

	GDALRasterBandH hBand = GDALGetRasterBand( rasInput, 1 );
	//GDALRasterBand *hBand = rasInput->GetRasterBand( 1 );
	double ncols = GDALGetRasterBandXSize( hBand );
	double nrows = GDALGetRasterBandYSize( hBand );

	/* Write out the GeoTransform */
	double inputGeoTransform[6];
	GDALGetGeoTransform( rasInput, inputGeoTransform );
	//rasInput->GetGeoTransform( inputGeoTransform );

	/* Close file */
	GDALClose( rasInput );

	double xmin = inputGeoTransform[0]; /* top left x */
	double ymin = inputGeoTransform[3]; /* top left y */
	double xres = inputGeoTransform[1]; /* w-e pixel resolution */
	double yres = abs(inputGeoTransform[5]); /* n-s pixel resolution (negative value) */
	if (xres!=yres)
	{ /* n-s pixel resolution (negative value) */
		logg.error("!!! X & Y resolution are not the same! ( ", xres, " & ", yres, " )");
	}

	/* Fill the other params */
	double xmax = xmin + ncols * xres;
	double ymax = ymin + nrows * yres;

	/* Create and return the output object */
	return Coordinates<double>(xmin,xmax,xres,ymin,ymax,yres);
} // end of ReadCoordinates(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void testDirExist(const string& param, const string& dir_name, const bool& optional)
{
	if (dir_name != "0")
	{
		fs::path dir_to_test(dir_name);
		if (!fs::is_directory(dir_to_test))
		{
			fs::create_directory(dir_to_test);
			logg.warning("!!! Parameter ", param, " : the directory ", dir_name,
									 " has been created.");
		}
	} else if (optional == false)
	{
	  logg.error("!!! Parameter ", param, " has not been given. Please check!");
	}
}

void testFileExist(const string& param, const string& file_name, const bool& optional)
{
	fs::path file_to_test(file_name);
	if ((file_name == "0" && optional == false) ||
		(file_name != "0" && optional == false && !fs::exists(file_to_test)))
	{
		logg.error("!!! Parameter ", param, " : the file ", file_name,
							 " does not exist. Please check!");
	}
	if (file_name != "0" && optional == false && fs::exists(file_to_test))
	{
		ifstream f(file_name.c_str());
		if (!f.good())
		{
			logg.error("!!! Parameter ", param, " : the file ", file_name,
			 					 " can not be opened. Please check!");
		}
	}
}

void testFileExist(const string& param, vector<string> vector_name, const bool& optional)
{
	for (vector<string>::iterator file_name=vector_name.begin(); file_name!=vector_name.end(); ++file_name)
	{
		testFileExist(param, (*file_name), optional);
	}
}

void testFileExist_changeFile(const string& param, vector<string> vector_name, const bool& optional)
{
	for (vector<string>::iterator file_name=vector_name.begin(); file_name!=vector_name.end(); ++file_name)
	{
		fs::path file_to_test(*file_name);
		if ((*file_name) != "0" && fs::exists(file_to_test))
		{
			ifstream file((*file_name).c_str(), ios::in);
			if (file)
			{
				/* Read file line by line */
				string strTmp; // tmp string to keep change filenames
				while (file >> strTmp)
				{
					if (strTmp != "")
					{
						testFileExist(param+(*file_name), strTmp, optional);
					}
				}
				/* Close file */
				file.close();
			}
		}
	}
}
