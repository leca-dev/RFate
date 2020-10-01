/*============================================================================*/
/*                    Spatial Class and Utilities Templates                   */
/*============================================================================*/

/*!
 * \file Spatial.h
 * \brief Spatial objects and templates class
 * \author Damien Georges, Maya Gueguen
 * \version 1.0
 * \date 2016/04/15
 */

#ifndef SPATIAL_H
#define SPATIAL_H

#include <iostream>
#include <string>
#include <cstring>
#include <fstream>
#include <cstdio>
#include <vector>

#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/filesystem.hpp>

#include "gdal_priv.h" // to read raster files
#include "gdal.h"
#include "cpl_conv.h"
#include "Logger.h"

using namespace std;


/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

 /*!
 * \class Coordinates
 * \brief Coordinates of any spatial object
 *
 * This object contains basic information to build a spatial map (X and Y
 * minimum and maximum coordinates, resolution and number of cells).
 * It will be used to build a SpatialMap object.
 */

template <class T>
class Coordinates
{
	protected:

	T Xmin; /*!< minimal longitude */
	T Xmax; /*!< maximal longitude */
	T Xres; /*!< longitudinal resolution */

	T Ymin; /*!< minimal latitude */
	T Ymax; /*!< maximal latitude */
	T Yres; /*!< latitudinal resolution */

	unsigned Xncell; /*!< number of cells in the whole longitudinal gradient */
	unsigned Yncell; /*!< number of cells in the whole latitudinal gradient */
	unsigned Totncell; /*!< total number of cells in the map */

	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		ar & Xmin;
		ar & Xmax;
		ar & Xres;
		ar & Ymin;
		ar & Ymax;
		ar & Yres;
		ar & Xncell;
		ar & Yncell;
		ar & Totncell;
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	Coordinates default constructor => All parameters are set to 0, False or None
	 */
	Coordinates() : Xmin(0), Xmax(0), Xres(0), Ymin(0), Ymax(0), Yres(0),
	Xncell(0), Yncell(0), Totncell(0)
	{
		/* Nothing to do */
	}

	/*!
	 *	\brief Full constructor
	 *
	 *	Coordinates full constructor
	 *
	 *	\param xmin : minimal longitude
	 *	\param xmax : maximal longitude
	 *	\param xres : longitudinal resolution
	 * \param ymin : minimal latitude
	 * \param ymax : maximal latitude
	 * \param yres : latitudinal resolution
	 */
	Coordinates(T xmin, T xmax, T xres, T ymin, T ymax, T yres) :
	Xmin(xmin), Xmax(xmax), Xres(xres), Ymin(ymin), Ymax(ymax), Yres(yres),
	Xncell(unsigned ((xmax - xmin) / xres)), Yncell(unsigned ((ymax - ymin) / yres)),
	Totncell(this->Xncell * this->Yncell)
	{
		/* Nothing to do */
	}

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	Coordinates destructor
	 */
	~Coordinates()
	{
		/* Nothing to do */
	}

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const Coordinates& o) const
	{
		return (Xmin == o.Xmin &&
		Xmax == o.Xmax &&
		Xres == o.Xres &&
		Xncell == o.Xncell &&
		Ymin == o.Ymin &&
		Ymax == o.Ymax &&
		Yres == o.Yres &&
		Yncell == o.Yncell &&
		Totncell == o.Totncell);
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	T getX(const int& i){ return Xmin + i*Xres; }
	T getY(const int& j){ return Ymin + j*Yres; }

	T getXres() { return Xres; }
	T getYres() { return Yres; }

	const unsigned& getXncell() const{ return Xncell; }
	const unsigned& getYncell() const{ return Yncell; }

	T getXmin(){ return Xmin; }
	T getYmin(){ return Ymin; }

	T getXmax(){ return Xmax; }
	T getYmax(){ return Ymax; }

	unsigned getTotncell(){ return Totncell; }

};

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

 /*!
 * \class SpatialMap
 * \brief Spatial map object
 *
 * This object is a map defined by its coordinates, and its values.
 */

template <class coor_T, class val_T>
class SpatialMap
{
	protected:

	Coordinates<coor_T> * XY; /*!< pointer to a coordinates object */
	vector<val_T> Values; /*!< list of stored values */

	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		ar & XY;
		ar & Values;
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	SpatialMap default constructor => All parameters are set to 0, False or None
	 */
	SpatialMap()
	{
		XY = NULL;
		Values.assign(0, (val_T)(0));
	}

	/*!
	 *	\brief Semi-default constructor (empty map)
	 *
	 *	SpatialMap semi-default constructor (empty map)
	 *
	 *	\param xy : pointer to a coordinates object
	 */
	SpatialMap(Coordinates<coor_T>* xy)
	{
		XY = xy;
		Values.assign(XY->getTotncell(), (val_T)(0));
	}

	/*!
	 *	\brief Full constructor
	 *
	 *	SpatialMap full constructor
	 *
	 *	\param xy : pointer to a coordinates object
	 *	\param values : vector containing all values to store
	 */
	SpatialMap(Coordinates<coor_T> * xy, vector<val_T> values)
	{
		Values.clear();
		XY = xy;
		Values = values;
	}

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	SpatialMap destructor
	 */
	~SpatialMap()
	{
		/* Nothing to do */
	}

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const SpatialMap& o) const
	{
		return ( *XY == *(o.XY) && Values == o.Values );
	}

	/* getter based on x,y coordinates */
	val_T& operator() (unsigned i, unsigned j)
	{
		if (i >= XY->getXncell() || j >= XY->getYncell())
		{
			logg.error("Matrix subscript out of bounds");
		}
		return Values[i + j * XY->getXncell()];
	}

	/* getter based on point id */
	val_T& operator() (unsigned id)
	{
		return this->getValue(id);
	}

	/* setter based on x,y coordinates */
	void operator() (unsigned i, unsigned j, const val_T value)
	{
		if (i >= XY->getXncell() || j >= XY->getYncell())
		{
			logg.error("Matrix subscript out of bounds");
		} else
		{
			this->Values[i + j * XY->getXncell()] = value;
		}
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	vector<coor_T> getXY(unsigned i, unsigned j)
	{
		vector<coor_T> xy(2);
		xy[0] = XY->getX(i);
		xy[1] = XY->getY(j);
		return xy;
	}
	coor_T getXres() { return XY->getXres(); }
	coor_T getYres() { return XY->getYres(); }
	unsigned getXncell() { return XY->getXncell(); }
	unsigned getYncell() { return XY->getYncell(); }
	unsigned getTotncell() { return XY->getTotncell(); }
	Coordinates<coor_T>* getCoordinates() { return XY; }
	vector<val_T> getValues() { return Values; };

	val_T& getValue(unsigned id)
	{
		if (id > XY->getXncell() * XY->getYncell())
		{
			logg.error("Matrix subscript out of bounds");
		}
		return Values[ id ];
	}

	void setValues(vector<val_T> values){ Values = values; }
	void setValue(unsigned id, val_T value)
	{
		if (id > XY->getXncell() * XY->getYncell())
		{
			logg.error("Matrix subscript out of bounds");
		}
		Values[ id ] = value;
	}

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Empty a map of its values
	 *
	 *	This function empties a spatial map of all its values by assigning them
	 * to a null value.
	 */
	void emptyMap()
	{
		val_T null_val = (val_T)(0);
		for (unsigned id=0; id<this->getTotncell(); id++)
		{
			Values[ id ] = null_val;
		}
	}
};

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

 /*!
 * \class SpatialStack
 * \brief Spatial stack object
 *
 * This object is a vector of SpatialMap objects, each defined by the same
 * coordinates, and with a vector of values specific to each map.
 */

template <class coor_T, class val_T>
class SpatialStack
{
	protected:

	vector< SpatialMap< coor_T, val_T > > Layers;

	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		ar & Layers;
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	SpatialStack default constructor => All parameters are set to 0, False or None
	 */
	SpatialStack()
	{
		/* Nothing to do */
	}

	/*!
	 *	\brief Semi-default constructor (empty stack)
	 *
	 *	SpatialStack semi-default constructor (empty stack)
	 *
	 *	\param xy : pointer to a coordinates object
	 *	\param no_layers : the number of layers
	 */
	SpatialStack( Coordinates<coor_T>* xy, unsigned no_layers)
	{
		SpatialMap<coor_T, val_T> layerTmp = SpatialMap<coor_T, val_T>(xy);
		Layers = vector< SpatialMap< coor_T, val_T > >(no_layers, layerTmp);
	}

	/*!
	 *	\brief Full constructor
	 *
	 *	SpatialStack full constructor
	 *
	 *	\param xy : pointer to a coordinates object
	 *	\param values : 2D vector containing all values to store
	 */
	SpatialStack( Coordinates<coor_T> * xy, vector< vector<val_T> > values )
	{
		Layers.clear();
		Layers.reserve(values.size());
		SpatialMap<coor_T, val_T> layerTmp = SpatialMap<coor_T, val_T>(xy);
		for (unsigned l=0; l<values.size(); l++)
		{
			layerTmp.setValues(values.at(l));
			Layers.emplace_back(layerTmp);
		}
	}

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	SpatialStack destructor
	 */
	~SpatialStack()
	{
		/* Nothing to do */
	}

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const SpatialStack& o) const
	{
		return (Layers == o.Layers);
	}

	/* getter based on x,y coordinates and layer id */
	val_T& operator() (unsigned i, unsigned j, unsigned k)
	{
		if (i >= Layers.at(0).getXncell() || j >= Layers.at(0).getYncell())
		{
			logg.error("Matrix subscript out of bounds");
		}
		return Layers.at(k)(i,j);
	}

	/* getter based on cell id and layer id */
	val_T& operator() (unsigned id, unsigned k)
	{
		if (id > Layers.at(0).getXncell() * Layers.at(0).getYncell())
		{
			logg.error("Matrix subscript out of bounds");
		}
		return Layers.at(k)(id);
	}

	/* getter based on layer id */
	val_T& operator() (unsigned k)
	{
		return Layers.at(k);
	}

	/* setter based on x,y coordinates and layer id */
	void operator() (unsigned i, unsigned j, unsigned k, const val_T value)
	{
		if (i >= Layers.at(0).getXncell() || j >= Layers.at(0).getYncell())
		{
			logg.error("Matrix subscript out of bounds");
		} else
		{
			this->Layers[k](i,j,value);
		}
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	vector<coor_T> getXY( unsigned i, unsigned j)
	{
		vector<coor_T> xy(2);
		xy = Layers.at(0).getXY(i, j);
		return xy;
	}
	Coordinates<coor_T>* getCoordinates(unsigned k) { return Layers.at(k).getCoordinates(); }

	coor_T getXres() { return Layers.at(0).getXres(); }
	coor_T getYres() { return Layers.at(0).getYres(); }

	unsigned getXncell() { return Layers.at(0).getXncell(); }
	unsigned getYncell() { return Layers.at(0).getYncell(); }
	unsigned getNoLayers() { return Layers.size(); }

	vector<val_T> getValues(unsigned k) {return Layers.at(k).getValues(); };

	void setValue(unsigned id, unsigned k, val_T value)
	{
		if (id > Layers.at(0).getXncell() * Layers.at(0).getYncell())
		{
			logg.error("Matrix subscript out of bounds");
		} else
		{
			this->Layers.at(k).setValue(id,value);
		}
	}

	void setValues(unsigned k, vector<val_T> values)
	{
		this->Layers.at(k).setValues(values);
	}

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Empty each map of a stack of its values
	 *
	 *	This function empties each spatial map of a spatial stack of all its
	 * values by assigning them to a null value.
	 */
	void emptyStack()
	{
		for (unsigned id=0; id<Layers.size(); id++)
		{
			Layers.at(id).emptyMap();
		}
	}
};

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/*!
 *	\brief Read mask from an ASCII file
 *
 *	This function will read an ASCII file and extract information contained in.
 *
 *	\param file_name : path to ASCII file
 *	\param only_defined_cell : should NA values be kept and stored
 */

template< typename T >
vector< T > ReadAscii(string file_name, bool only_defined_cell = false)
{
	/* check if file exists */
	ifstream file(file_name.c_str(), ios::in);
	if (file)
	{
		double xmin, ymin, cellres, ncols, nrows, valTmp, nodata;
		string strTmp;
		vector< T > res;
		/* read ASCII file header */
		for (unsigned i=0; i<6; i++)
		{
			file >> strTmp;
			if (strTmp == "NCOLS"){ file >> ncols; }
			else if (strTmp == "NROWS"){ file >> nrows; }
			else if (strTmp == "XLLCORNER"){ file >> xmin; }
			else if (strTmp == "YLLCORNER"){ file >> ymin; }
			else if (strTmp == "CELLSIZE"){ file >> cellres; }
			else if (strTmp == "NODATA_value"){ file >> nodata; }
		} // end of loop for header read

		/* Storing mask values */
		while (!file.eof())
		{
			file >> valTmp;
			if (only_defined_cell && valTmp != nodata)
			{
				res.push_back( (T)(valTmp) );
			} else
			{
				res.push_back( (T)(valTmp) );
			}
		}
		/* close file */
		file.close();
		/* return result */
		return res;
	} else
	{
		logg.error("Impossible to open ", file_name, " file! (mask)");
	}
}

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/*!
 *	\brief Read raster with RGDAL
 *
 *	This function will read a raster file (.img, .tif) and extract information
 * contained in.
 *
 *	\param file_name : path to raster file (.img, .tif)
 *	\param lim_inf : minimum value accepted within the raster file
 * \param lim_sup : maximum value accepted within the raster file
 * \param print_info : should informations be printed (projection, driver...)
 */

template< typename T >
vector< T > ReadRaster(string file_name, double lim_inf, double lim_sup, bool print_info)
{
	logg.info(file_name);
	GDALAllRegister();

	// Get output driver (GeoTIFF format).
	const char * driverInput = "GTiff";
	boost::filesystem::path file_name_path(file_name.c_str());
	if (file_name_path.extension()==".tif")
	{
		driverInput = "GTiff";
	} else if (file_name_path.extension()==".img")
	{
		driverInput = "HFA";
	} else
	{
		logg.error("!!! The file extension (", file_name_path.extension(),
               ") is not taking into account!",
               "\n!!! Please use either .img or .tif files!");
	}
	if (print_info)
  {
    logg.debug("Input driver is : ", driverInput, " (extension ",
               file_name_path.extension(), ")");
  }

	// Open the source file.
	GDALDatasetH rasInput = GDALOpen( file_name.c_str(), GA_ReadOnly );
	CPLAssert( rasInput != NULL );

	// Get Source coordinate system.
	const char *inputProjection = GDALGetProjectionRef( rasInput );
	CPLAssert( inputProjection != NULL && strlen(inputProjection) > 0 );
	if (print_info)
  {
    logg.debug("Input projection is : ", inputProjection);
  }

	// Create output with same datatype as first input band.
	GDALDataType inputDataType = GDALGetRasterDataType(GDALGetRasterBand(rasInput,1)); //GDT_Byte
	if (print_info)
  {
    logg.debug("Input data type is : ", inputDataType);
  }

	GDALRasterBandH hBand = GDALGetRasterBand( rasInput, 1 );
	int ncols = GDALGetRasterBandXSize( hBand );
	int nrows = GDALGetRasterBandYSize( hBand );

	// Write out the GeoTransform.
	double inputGeoTransform[6];
	GDALGetGeoTransform( rasInput, inputGeoTransform );
	double cellres = inputGeoTransform[1]; /* w-e pixel resolution */
	if (print_info)
  {
    logg.debug("Input resolution is : ", cellres);
  }
	vector< T > res;
	res.reserve(nrows * ncols);
	for (int i=0; i<nrows; i++)
	{
		//float scanline[ncols];
		//vector<float> scanline(ncols);
		float *scanline = new float[ncols];
		CPLErr rasterAccess = GDALRasterIO(
			hBand, GF_Read, 0, i, ncols, 1, scanline, ncols, 1, GDT_Float32, 0, 0
		);
		if (rasterAccess > 0)
		{
			logg.warning("Reading ", file_name, " raster: acces status ",
									 rasterAccess);
		}
		for (int j=0; j<ncols; j++)
		{
			res.emplace_back(scanline[j]);
		}
		delete[] scanline;
	}
	if (print_info)
  {
    logg.debug("Reading completed!");
  }

	/* close file */
	GDALClose( rasInput );

	/* check for values out of range */
	for (unsigned cell_ID=0; cell_ID<res.size(); cell_ID++)
	{
		if (res[cell_ID]<lim_inf || res[cell_ID]>lim_sup)
		{
			logg.error("!!! This map contains element that are not included between ",
                 lim_inf, " and ", lim_sup, "!\n",
                 "!!! NA values are not accepted : please replace them with 0.");
		}
	}

	/* return result */
	return res;
}

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/*!
 *	\brief Read raster file
 *
 *	This function will select the correct function to read a raster file (.img,
 * .tif) and extract information contained in.
 *
 *	\param file_name : path to raster file (.img, .tif)
 *	\param lim_inf : minimum value accepted within the raster file
 * \param lim_sup : maximum value accepted within the raster file
 * \param print_info : should informations be printed (projection, driver...)
 */

template< typename T >
vector< T > ReadMask(string file_name, double lim_inf=(-1.0)*numeric_limits<double>::infinity(),
double lim_sup=numeric_limits<double>::infinity(), bool print_info=false)
{
	boost::filesystem::path file_name_path(file_name.c_str());
	//if(file_name_path.extension()==".asc"){ // ASCII file
	//  res = ReadAscii< T >(file_name);
	//} else
	if (file_name_path.extension() != ".img" &&
			file_name_path.extension() != ".tif")
	{
		logg.error("!!! The file extension (", file_name_path.extension(),
               ") is not taking into account!",
               "\n!!! Please use either .img or .tif files!",
               "\n(NB: module to take into account .asc files is still coded ",
               "but has been removed to improve data reliability : with .img ",
               "or .tif, you can give the projection system and the program ",
               "will check that the maps provided have the same)");
	}
	return ReadRaster< T >(file_name, lim_inf, lim_sup, print_info);
}

#endif // SPATIAL_H
