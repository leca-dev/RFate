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
/*                            Seeds Dispersal Class                           */
/*============================================================================*/

/*!
 * \file Disp.h
 * \brief Dispersal modules definition
 * \author Damien Georges, Maya Gueguen
 * \version 1.0
 * \date 2020
 */

#ifndef DISP_H
#define DISP_H

#include "openmp.h"
#include "FG.h"
#include "Logger.h"

#include <random>

typedef vector<FG>* vecFGPtr;
typedef SpatialStack<double, int>* IntMapPtr;
using namespace std;

typedef std::default_random_engine RandomGenerator;


// TODO (damien#1#): make dispersal packet + proba dispersal procedure to prevent from seeds loss

/*!
 * \class Disp
 * \brief Dispersal modules definition
 *
 * This object simulates the dispersal of seeds. It is a module independent
 * from the core (demographic) model. The main dispersal routine can be cut
 * into three steps :
 *   - get for each FG the seed rain within each map pixel
 *   - disperse seeds according to FG's dispersal parameters (d50, d99 and ldd)
 * and selected dispersal module
 *   - return a dispersed seeds map that will be used by the core (demographic)
 * model
 * Several dispersal modules are available :
 *   1. uniform kernel
 *   2. exponential kernel
 *   3. exponential kernel and continuous decreasing probability with distance
 */

class Disp
{
	private:

	vecFGPtr m_FGparams; /*!< FG parameters */
	IntMapPtr m_SeedMapIn; /*!< Map of last year seed rain */
	IntMapPtr m_SeedMapOut; /*!< Map of dispersed seeds */

	vector< vector< vector<int> > > m_FGdistCircle; /*!< positions of pixels within the d50, d99 anf dld dispersal distances of PFG */
	vector< vector<float> > m_prop_d1; /* proportion of seeds into crown d50 */
	vector< vector<float> > m_prop_d2; /* proportion of seeds into crown d99 */
	vector< vector<float> > m_prob_d1; /* probability vector of receiving seeds into crown d50 */
	vector< vector<float> > m_prob_d2; /* probability vector of receiving seeds into crown d99 */

	/*-------------------------------------------*/
	/* Serialization function -------------------*/
	/*-------------------------------------------*/

	friend class boost::serialization::access;
	template<class Archive>
	void serialize(Archive & ar, const unsigned int /*version*/)
	{
		ar & m_FGparams;
		ar & m_SeedMapIn;
		ar & m_SeedMapOut;
		ar & m_FGdistCircle;
		ar & m_prop_d1;
		ar & m_prop_d2;
		ar & m_prob_d1;
		ar & m_prob_d2;
	}

	public:

	/*-------------------------------------------*/
	/* Constructors -----------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Default constructor
	 *
	 *	Disp default constructor => All parameters are set to 0, False or None
	 */
	Disp();

	/*!
	 *	\brief Default constructor
	 *
	 *	Disp default constructor => All parameters are set to 0, False or None
	 */
	Disp( vecFGPtr FGparams, IntMapPtr seedMapIn, IntMapPtr seedMapOut, bool doDisp);

	/*!
	 *	\brief Full constructor
	 *
	 *	Disp full constructor
	 *
	 *	\param FGparams : pointer to vector of FG parameters
	 *	\param seedMapIn : pointer to spatial map of input seeds
	 *	\param seedMapOut : pointer to spatial map of output seeds
	 */
	Disp( vecFGPtr FGparams, IntMapPtr seedMapIn, IntMapPtr seedMapOut );

	/*-------------------------------------------*/
	/* Destructor -------------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Destructor
	 *
	 *	Disp destructor
	 */
	~Disp();

	/*-------------------------------------------*/
	/* Operators --------------------------------*/
	/*-------------------------------------------*/

	bool operator==(const Disp& o) const
	{
		return ( m_FGdistCircle == o.m_FGdistCircle &&
		m_prop_d1 == o.m_prop_d1 &&
		m_prop_d2 == o.m_prop_d2 &&
		m_prob_d1 == o.m_prob_d1 &&
		m_prob_d2 == o.m_prob_d2 &&
		*m_FGparams == *(o.m_FGparams) &&
		*m_SeedMapIn == *(o.m_SeedMapIn) &&
		*m_SeedMapOut == *(o.m_SeedMapOut));
	}

	/*-------------------------------------------*/
	/* Getters & Setters ------------------------*/
	/*-------------------------------------------*/

	const vector< vector< vector<int> > >& getFGdistCircle() const;
	const vector< vector<int> >& getFGdistCircle(int fg) const;
	const vector<int>& getFGdistCircle(int fg, int d_xy) const;
	const vector< vector<float> >& getprop_d1() const;
	const vector<float>& getprop_d1(int fg) const;
	const vector< vector<float> >& getprop_d2() const;
	const vector<float>& getprop_d2(int fg) const;
	const vector< vector<float> >& getprob_d1() const;
	const vector<float>& getprob_d1(int fg) const;
	const vector< vector<float> >& getprob_d2() const;
	const vector<float>& getprob_d2(int fg) const;

	vecFGPtr getFGparams_();
	IntMapPtr getSeedMapIn_();
	IntMapPtr getSeedMapOut_();

	void setFGdistCircle(const vector< vector< vector<int> > >& FGdistCircle);
	void setFGdistCircle(const int& fg, const vector< vector<int> >& FGdistCircle);
	void setFGdistCircle(const int& fg, const int& d_xy, const vector<int>& FGdistCircle);
	void setprop_d1(const vector< vector<float> >& prop_d1);
	void setprop_d1(const int& fg, const vector<float>& prop_d1);
	void setprop_d2(const vector< vector<float> >& prop_d2);
	void setprop_d2(const int& fg, const vector<float>& prop_d2);
	void setprob_d1(const vector< vector<float> >& prob_d1);
	void setprob_d1(const int& fg, const vector<float>& prob_d1);
	void setprob_d2(const vector< vector<float> >& prob_d2);
	void setprob_d2(const int& fg, const vector<float>& prob_d2);

	/*-------------------------------------------*/
	/* Other functions --------------------------*/
	/*-------------------------------------------*/

	/*!
	 *	\brief Calculate exponential density function
	 *
	 *	This routine returns the value of lambda * exp( -lambda * x ) function.
	 * It is used for exponential kernel calculation.
	 *
	 *	\param x : distance (given in pixels)
	 * \param lambda : exponential law parameter
	 * \return : value of exponential density at point x for lambda parameter
	 */
	float ExpDensity(float x, float lambda );

	/*!
	 *	\brief Calculate continuous decreasing probability
	 *
	 *	This routine returns the value of 1 - d / d99.
	 * It is used for exponential kernel calculation with continuous decreasing
	 * of probability with distance.
	 *
	 *	\param d : distance (given in pixels)
	 * \param d99 : dispersal distance for 99% of seeds
	 * \return : value between 0-1 corresponding to the probability of having
	 * seeds at distance d from source
	 */
	float ContinuousDecrProba(int d, int d99);

	/*!
	 *	\brief Get XY of each cell within each dispersal distance (d50, d99, ldd)
	 *
	 *	This function computes, for each plant functional group, the coordinates
	 * of the cells within the 3 dispersal distance circles. These coordinates
	 * will be used as buffer to apply dispersal for each pixel and FG.
	 */
	void GetDistancesXY();

	/*!
	 *	\brief Get proportion and probability of seeds for each cell within each
	 * dispersal distance (d50, d99, ldd)
	 *
	 *	This function computes, for each plant functional group, the proportion
	 * of seeds (according to the exponential density function) and the
	 * probability (according to the continuous decreasing probability function)
	 * of each cell within the 3 dispersal distance circles. These values will
	 * be used to apply dispersal for each pixel and FG if dispersal modules 2
	 * or 3 are selected.
	 */
	void GetPropProb();

	/*!
	 *	\brief Disperse seeds
	 *
	 *	This function apply the dispersal of seeds for each plant functional
	 * group, according to the selected dispersal module.
	 * If uniform dispersal (1) is selected :
	 *	  - 50% of the seeds are dispersed uniformly within the 0-d50 disk ;
	 *   - 49% are dispersed in xx (number of cells within the 0-d50 disk / 2)
	 * randomly selected pairs of adjacent cells within the d50-d99 disk (to
	 * prevent from over-dilution of seeds) ;
	 *   - last 1% of seeds are dispersed within a single cell of the d99-ldd
	 * disk.
	 * If exponential kernel (2) is selected :
	 *   - 50% of the seeds are dispersed in a continuous decreasing way
	 * according to distance within the 0-d50 disk ;
	 *   - 49% of the seeds are dispersed in a continuous decreasing way
	 * according to distance within the d50-d99 disk ;
	 *   - last 1% of seeds are dispersed within a single cell of the d99-ldd
	 * disk.
	 * If exponential kernel + probability (3) is selected :
	 *   - 50% of the seeds are dispersed in a continuous decreasing way
	 * according to distance within the 0-d50 disk, and with a probability
	 * decreasing with distance ;
	 *   - 49% of the seeds are dispersed in a continuous decreasing way
	 * according to distance within the d50-d99 disk, and with a probability
	 * decreasing with distance ;
	 *   - last 1% of seeds are dispersed within a single cell of the d99-ldd
	 * disk.
	 *
	 * \param dispOption : dispersal module
	 * \param noCPU : number of computer resources that can be used to
	 * parallelize and speed up dispersal
	 * \param maskCells : vector of cells ID in which dispersal occurs
	 */
	// void DoDispersalPacket(unsigned dispOption, RandomGenerator& rng, int noCPU, vector<int> maskCells);
	void DoDispersalPacket(unsigned dispOption, int noCPU, vector<int> maskCells
                          , vector< vector< vector<int> > > randInt_1, vector< vector< vector<int> > > randInt_2
                          , vector< vector<double> > rand01_1, vector< vector<double> > rand01_2
                          , vector< vector<int> > LD_draw);
	
};

#endif //DISP_H
