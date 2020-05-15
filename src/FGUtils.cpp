#include "FGUtils.h"
#include "Params.h"

using namespace std;


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Categorical to scalar and scalar to categorical conversion functions                            */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Fract getLeavingFract(Fract f1, Fract f2)
{
	return DoubleToFract( 1.0 - ( FractToDouble(f1) + FractToDouble(f2) ) );
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

double FractToDouble(Fract fract)
{
	double Fract_Real [ Fcount ] = {0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0};
	return Fract_Real[int(fract)];
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

double FractToDouble(Fract2 fract)
{
	double Fract_Real [ Fcount ] = {0.0,0.1,0.5,0.9,1.0};
	return Fract_Real[int(fract)];
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Fract DoubleToFract(double d)
{
	if (d <= 0.0) return Fract(0);
	else if( d > 0.0 && d <= 0.15 ) return Fract(1);
	else if( d > 0.15 && d <= 0.25 ) return Fract(2);
	else if( d > 0.25 && d <= 0.35 ) return Fract(3);
	else if( d > 0.35 && d <= 0.45 ) return Fract(4);
	else if( d > 0.45 && d <= 0.55 ) return Fract(5);
	else if( d > 0.55 && d <= 0.65 ) return Fract(6);
	else if( d > 0.65 && d <= 0.75 ) return Fract(7);
	else if( d > 0.75 && d <= 0.85 ) return Fract(8);
	else if( d > 0.85 && d < 1.0 ) return Fract(9);
	else return Fract(10);
}

double ResourceToDouble(Resource light)
{
	double Resource_Real [ Rcount ] = {1.0, 2.0, 3.0};
	return Resource_Real[int(light)];
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
	if (file)
	{
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
				cerr << flag << " parameter NOT recovered " << endl;
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
				cout << flag << " parameter recovered " << endl;
			}
		} else
		{
			//res.resize(1,""); /* return vector with an empty string  */
			cerr << flag << " parameter NOT recovered " << endl;
		}
		/* Close file */
		file.close();
		/* return list of paths */
		return res;
	} else
	{
		cerr << "Impossible to open " << paramFile << " file! (parameters)" << endl;
		terminate();
	}	
} // end of ReadParamsWithinFile(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

vector< int > ReadTimingsFile(string paramFile)
{
	if (paramFile != "" && paramFile != "0" )
	{ // if empty return empty vector
		/* File existence checking */
		ifstream file(paramFile.c_str(), ios::in);
		if (file)
		{
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
			cerr << "Impossible to open " << paramFile << " file! (timing)" << endl;
			terminate();
		}
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
	boost::filesystem::path file_name_path(file_name.c_str());
	if (file_name_path.extension()==".asc")
	{ // ASCII file
		return Coordinates<double>(ReadAsciiCoordinates(file_name));
	} else if (file_name_path.extension()==".img" || file_name_path.extension()==".tif")
	{ // IMG or TIF file
		return Coordinates<double>(ReadRasterCoordinates(file_name));
	} else
	{
		cerr << "!!! The file extension (" << file_name_path.extension() << ") is not taking into account!" << endl;
		cerr << "!!! Please use either .img or .tif (or .asc) files!" << endl;
		terminate();
	}
} // end of ReadCoordinates(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Coordinates<double> ReadAsciiCoordinates( string file_name )
{
	/* File existence checking */
	ifstream file(file_name.c_str(), ios::in);
	if (file)
	{
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
	} else
	{
		cerr << "Impossible to open " << file_name << " file! (coordinates)" << endl;
		terminate();
	}
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
		cerr << "!!! X & Y resolution are not the same! ( " << xres << " & " << yres << " )" << endl;
		terminate();
	}
	
	/* Fill the other params */
	double xmax = xmin + ncols * xres;
	double ymax = ymin + nrows * yres;
	
	/* Create and return the output object */
	return Coordinates<double>(xmin,xmax,xres,ymin,ymax,yres);
} // end of ReadCoordinates(...)

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void testDirExist(const string& param, const string& dir_name)
{
	if (dir_name != "0")
	{
		boost::filesystem::path dir_to_test(dir_name);
		if (!boost::filesystem::is_directory(dir_to_test))
		{
			boost::filesystem::create_directories(dir_to_test);
			cerr << "!!! Parameter " << param << " : the directory " << dir_name << " has been created." << endl;
		}
	}
}

void testFileExist(const string& param, const string& file_name, const bool& optional)
{
	boost::filesystem::path file_to_test(file_name);
	if ((file_name == "0" && optional == false) ||
		(file_name != "0" && optional == false && !boost::filesystem::exists(file_to_test)))
	{
		cerr << "!!! Parameter " << param << " : the file " << file_name << " does not exist. Please check!" << endl;
		terminate();
	}
	if (file_name != "0" && optional == false && boost::filesystem::exists(file_to_test))
	{
		ifstream f(file_name.c_str());
		if (!f.good())
		{
			cerr << "!!! Parameter " << param << " : the file " << file_name << " can not be opened. Please check!" << endl;
			terminate();
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
		boost::filesystem::path file_to_test(*file_name);
		if ((*file_name) != "0" && boost::filesystem::exists(file_to_test))
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


