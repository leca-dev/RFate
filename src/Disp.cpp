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

/*========================================================*/
/*           Seeds Dispersal Models Class                 */
/*    - severals ways to dispers are implemented -        */
/*========================================================*/


#include "Disp.h"
#include <vector>
#include <numeric>
#include <cmath>
#include <math.h>
#include <cstdlib>
#include <ctime>
#include <chrono>
#include <random>

using namespace std;

// boost::mt19937 est un Mersenne twister generator, ou générateur de nombres pseudo-aléatoires

typedef std::mt19937 RandomGenerator;
typedef std::uniform_real_distribution<double> UniReal;
typedef std::uniform_int_distribution<int> UniInt;


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Constructors                                                                                    */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Disp::Disp() : m_FGparams(), m_SeedMapIn(NULL), m_SeedMapOut(NULL),
m_FGdistCircle(0,vector< vector<int> >(0, vector<int>(0))),
m_prop_d1(0,vector<float>(0.0)), m_prop_d2(0,vector<float>(0.0)),
m_prob_d1(0,vector<float>(0.0)), m_prob_d2(0,vector<float>(0.0))
{
	/* Nothing to do */
}

Disp::Disp(vecFGPtr FGparams, IntMapPtr seedMapIn, IntMapPtr seedMapOut, bool doDisp) :
m_FGparams(FGparams), m_SeedMapIn(seedMapIn), m_SeedMapOut(seedMapOut),
m_FGdistCircle(0,vector< vector<int> >(0, vector<int>(0))),
m_prop_d1(0,vector<float>(0.0)), m_prop_d2(0,vector<float>(0.0)),
m_prob_d1(0,vector<float>(0.0)), m_prob_d2(0,vector<float>(0.0))
{
	/* Nothing to do */
}


Disp::Disp(vecFGPtr FGparams, IntMapPtr seedMapIn, IntMapPtr seedMapOut) :
m_FGparams(FGparams), m_SeedMapIn(seedMapIn), m_SeedMapOut(seedMapOut)
{
	m_FGdistCircle.resize(m_FGparams->size(),vector< vector<int> >(0));
	m_prop_d1.resize(m_FGparams->size(),vector<float>(0.0)); /* proportion of seeds into crown d50 */
	m_prop_d2.resize(m_FGparams->size(),vector<float>(0.0)); /* proportion of seeds into crown d99 */
	m_prob_d1.resize(m_FGparams->size(),vector<float>(0.0)); /* probability vector of receiving seeds into crown d50 */
	m_prob_d2.resize(m_FGparams->size(),vector<float>(0.0)); /* probability vector of receiving seeds into crown d99 */

	logg.info("> GetDistancesXY & GetPropProb...");
	GetDistancesXY();
	GetPropProb();
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Destructor                                                                                      */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

Disp::~Disp()
{
	/* Nothing to do */
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Getters & Setters                                                                               */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

const vector< vector< vector<int> > >& Disp::getFGdistCircle() const{ return m_FGdistCircle; }
const vector< vector<int> >& Disp::getFGdistCircle(int fg) const{ return m_FGdistCircle[fg]; }
const vector<int>& Disp::getFGdistCircle(int fg, int d_xy) const{ return m_FGdistCircle[fg][d_xy]; }
const vector< vector<float> >& Disp::getprop_d1() const{ return m_prop_d1; }
const vector<float>& Disp::getprop_d1(int fg) const{ return m_prop_d1[fg]; }
const vector< vector<float> >& Disp::getprop_d2() const{ return m_prop_d2; }
const vector<float>& Disp::getprop_d2(int fg) const{ return m_prop_d2[fg]; }
const vector< vector<float> >& Disp::getprob_d1() const{ return m_prob_d1; }
const vector<float>& Disp::getprob_d1(int fg) const{ return m_prob_d1[fg]; }
const vector< vector<float> >& Disp::getprob_d2() const{ return m_prob_d2; }
const vector<float>& Disp::getprob_d2(int fg) const{ return m_prob_d2[fg]; }

vecFGPtr Disp::getFGparams_() { return m_FGparams; }
IntMapPtr Disp::getSeedMapIn_() { return m_SeedMapIn; }
IntMapPtr Disp::getSeedMapOut_() { return m_SeedMapOut; }

void Disp::setFGdistCircle(const vector< vector< vector<int> > >& FGdistCircle){ m_FGdistCircle = FGdistCircle; }
void Disp::setFGdistCircle(const int& fg, const vector< vector<int> >& FGdistCircle){ m_FGdistCircle[fg] = FGdistCircle; }
void Disp::setFGdistCircle(const int& fg, const int& d_xy, const vector<int>& FGdistCircle){ m_FGdistCircle[fg][d_xy] = FGdistCircle; }
void Disp::setprop_d1(const vector< vector<float> >& prop_d1){ m_prop_d1 = prop_d1; }
void Disp::setprop_d1(const int& fg, const vector<float>& prop_d1){ m_prop_d1[fg] = prop_d1; }
void Disp::setprop_d2(const vector< vector<float> >& prop_d2){ m_prop_d2 = prop_d2; }
void Disp::setprop_d2(const int& fg, const vector<float>& prop_d2){ m_prop_d2[fg] = prop_d2; }
void Disp::setprob_d1(const vector< vector<float> >& prob_d1){ m_prob_d1 = prob_d1; }
void Disp::setprob_d1(const int& fg, const vector<float>& prob_d1){ m_prob_d1[fg] = prob_d1; }
void Disp::setprob_d2(const vector< vector<float> >& prob_d2){ m_prob_d2 = prob_d2; }
void Disp::setprob_d2(const int& fg, const vector<float>& prob_d2){ m_prob_d2[fg] = prob_d2; }

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Other functions                                                                                 */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

/* Exponential law density function */
float Disp::ExpDensity(float x, float lambda)
{
	return lambda * exp(-lambda * x);
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

float Disp::ContinuousDecrProba(int d, int d99)
{
	float res = 1.0 - d / (float) d99;
	if (res!=res) { return 0.0; // nan case
	} else { return res; }
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/
/* Several Dispersal functions                                                                     */
/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

int gaussCircleProblem(int radius)
{
    int allPoints=0; //holds the sum of points
    double y=0; //will hold the precise y coordinate of a point on the circle edge for a given x coordinate.
    long inscribedSquare = static_cast<long>(sqrt(radius * radius / 2)); //the length of the side of an inscribed square in the upper right quarter of the circle
    int x = static_cast<int>(inscribedSquare); //will hold x coordinate - starts on the edge of the inscribed square
    while(x<=radius){
        allPoints += static_cast<long>(y); //returns floor of y, which is initially 0
        x++; //because we need to start behind the inscribed square and move outwards from there
        y=sqrt(radius*radius-x*x); // Pythagorean equation - returns how many points there are vertically between the X axis and the edge of the circle for given x
    }
    allPoints*=8; //because we were counting points in the right half of the upper right corner of that circle, so we had just one-eightth
    allPoints+=(4*inscribedSquare*inscribedSquare); //how many points there are in the inscribed square
    allPoints+=(4*radius+1); //the loop and the inscribed square calculations did not touch the points on the axis and in the center
    return allPoints;
}

void Disp::GetDistancesXY()
{
	for(unsigned fg=0; fg<m_FGparams->size(); fg++)
	{
	/* conversion of dispersal distances in meters into pixels */
	int d1 = m_FGparams->at(fg).getDisp50() / m_SeedMapIn->getXres(); // disp50 into pixels
	int d2 = m_FGparams->at(fg).getDisp99() / m_SeedMapIn->getXres(); // disp99 into pixels
	int dld = m_FGparams->at(fg).getDispLD() / m_SeedMapIn->getXres(); // dispLD into pixels
	logg.info(">>> For PFG ", fg, ", pixels dispersal distances are : ", d1, ", ",
	 					d2, ", ", dld);

	/* Determination of indices of cells relative to seed pool for each crown */
	vector<int> v1x, v2x, vldx, v1y, v2y, vldy;
	v1x.reserve(gaussCircleProblem(d1)); v2x.reserve(gaussCircleProblem(d2)); vldx.reserve(gaussCircleProblem(dld));
	v1y.reserve(gaussCircleProblem(d1)); v2y.reserve(gaussCircleProblem(d2)); vldy.reserve(gaussCircleProblem(dld));
	for (int x= - (max(max(d1,d2),dld)); x<=(max(max(d1,d2),dld)); x++)
	{
		for (int y= - (max(max(d1,d2),dld)); y<=max(max(d1,d2),dld); y++)
		{
			if(x*x + y*y - d1 * d1 <= 0) /* into the r=d1 disk */
			{
				v1x.emplace_back(x);
				v1y.emplace_back(y);
			} else if(x*x + y*y - d2 * d2 <= 0) /* into the r1=d1, r2=d2 crown */
			{
				v2x.emplace_back(x);
				v2y.emplace_back(y);
			} else if(x*x + y*y - dld * dld <= 0) /* into the r1=d2, r2=dld crown */
			{
				vldx.emplace_back(x);
				vldy.emplace_back(y);
			}
		}
	}
	if (d1 == d2)
	{
		v2x = v1x;
		v2y = v1y;
	}
	v1x.shrink_to_fit(); v2x.shrink_to_fit(); vldx.shrink_to_fit();
	v1y.shrink_to_fit(); v2y.shrink_to_fit(); vldy.shrink_to_fit();

	m_FGdistCircle[fg].clear();
	m_FGdistCircle[fg].reserve(6);
	m_FGdistCircle[fg].emplace_back(v1x); m_FGdistCircle[fg].emplace_back(v2x); m_FGdistCircle[fg].emplace_back(vldx);
	m_FGdistCircle[fg].emplace_back(v1y); m_FGdistCircle[fg].emplace_back(v2y); m_FGdistCircle[fg].emplace_back(vldy);
	logg.info(">>> Number of corresponding pixels within circle are : ",
	 					m_FGdistCircle[fg][0].size(), ", ", m_FGdistCircle[fg][1].size(),
						", ", m_FGdistCircle[fg][2].size());
	}
}

/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

void Disp::GetPropProb()
{
	for(unsigned fg=0; fg<m_FGparams->size(); fg++)
	{
		/* conversion of dispersal distances in meters into pixels */
		int d1 = m_FGparams->at(fg).getDisp50() / m_SeedMapIn->getXres(); // disp50 into pixels
		int d2 = m_FGparams->at(fg).getDisp99() / m_SeedMapIn->getXres(); // disp99 into pixels

		/* 50 % of seeds are under 0 and ln(2) according to exponential density law (lambda=1) */
		/* 0-ln(2) interval is divide into d1+1 parts */
		m_prop_d1[fg].clear();
		m_prob_d1[fg].clear();
		m_prop_d1[fg].reserve(d1+1);
		m_prob_d1[fg].reserve(d1+1);
		for (int i=0; i<=d1; i++)
		{
			m_prop_d1[fg].emplace_back( ExpDensity(i*log(2.0)/(d1+1.0),1.0) - ExpDensity((i+1)*log(2.0)/(d1+1.0),1.0));
			m_prob_d1[fg].emplace_back(ContinuousDecrProba(i, d2));
		}

		/* 49% of seeds are dispersed between ln(2) and 6.65*ln(2) (less than 5/10000 seeds are lost) according to exponential density law (lambda=1) */
		/* ln(2)-6.65*ln(2) interval is divide into d2-d1+1 parts */
		m_prop_d2[fg].clear();
		m_prob_d2[fg].clear();
		m_prop_d2[fg].reserve(d2-d1+1);
		m_prob_d2[fg].reserve(d2-d1+1);
		for (int i=1; i<=d2-d1; i++)
		{
			m_prop_d2[fg].emplace_back( ExpDensity(log(2.0) + i*6.65*log(2.0)/(d2-d1+1.0),1.0) - ExpDensity(log(2.0) + (i+1)*6.65*log(2.0)/(d2-d1+1.0),1.0));
			m_prob_d2[fg].emplace_back(ContinuousDecrProba(i+d1, d2));
		}
	}
}


/*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*/

// TODO (damien#1#): make possible to consider different x and y resolution


void Disp::DoDispersalPacket(unsigned dispOption, unsigned seed, int noCPU, vector<int> maskCells)
{
	omp_set_num_threads( noCPU );
	#pragma omp parallel for schedule(dynamic) if(noCPU>1)

	for (unsigned fg=0; fg<m_FGparams->size(); fg++)
	{
		/* dispOption = 1 (uniform), 2 (expKernel) or 3 (expKernel + proba) */
		if (dispOption==1 || dispOption==2 || dispOption==3)
		{
			int xt, yt;
			vector<int> v1x_select, v2x_select, v1y_select, v2y_select;
			vector<float> prop_d1_select, prop_d2_select;
			unsigned noDrawMax = max(1, static_cast<int>(ceil(m_FGdistCircle[fg][0].size()/2.0)));

			RandomGenerator rng(seed);

			/* conversion of dispersal distances in meters into pixels */
			int d1 = m_FGparams->at(fg).getDisp50() / m_SeedMapIn->getXres(); // disp50 into pixels
			int d2 = m_FGparams->at(fg).getDisp99() / m_SeedMapIn->getXres(); // disp99 into pixels
			int dld = m_FGparams->at(fg).getDispLD() / m_SeedMapIn->getXres(); // dispLD into pixels

			if (dispOption==2 || dispOption==3)
			{
			  UniReal random_01(0.0, 1.0);

				/* select cell receiving seeds according to a probability decreasing with distance */
				for (unsigned id = 0; id < m_FGdistCircle[fg][0].size(); id++)
				{
					unsigned dist_pt = max(abs(m_FGdistCircle[fg][0][id]), abs(m_FGdistCircle[fg][3][id]));
					if (dist_pt < m_prob_d1[fg].size())
					{
						/* get an random number between 0-1 */
						/* compare this number to the probability vector value if < then the cell will receive seeds */
						if (dispOption == 2 || (dispOption == 3 && random_01(rng) < m_prob_d1[fg][dist_pt]))
						{
							v1x_select.push_back(m_FGdistCircle[fg][0][id]);
							v1y_select.push_back(m_FGdistCircle[fg][3][id]);
							prop_d1_select.push_back(m_prop_d1[fg][dist_pt]);
						}
					}
				}
				for (unsigned id = 0; id < m_FGdistCircle[fg][1].size(); id++)
				{
					unsigned dist_pt = max(abs(m_FGdistCircle[fg][1][id]), abs(m_FGdistCircle[fg][4][id])) - d1;
					if (dist_pt < m_prob_d2[fg].size())
					{
						/* get an random number between 0-1 */
						/* compare this number to the probability vector value if < then the cell will receive seeds */
						if (dispOption == 2 || (dispOption == 3 && random_01(rng) < m_prob_d2[fg][dist_pt]))
						{
							v2x_select.push_back(m_FGdistCircle[fg][1][id]);
							v2y_select.push_back(m_FGdistCircle[fg][4][id]);
							prop_d2_select.push_back(m_prop_d2[fg][dist_pt]);
						}
					}
				}
			}

			SpatialMap<double, int> new_SeedMapOut(SpatialMap<double, int>(m_SeedMapOut->getCoordinates(fg), m_SeedMapOut->getValues(fg)));

			/* Do dispersal for each cell containing seeds */
			for(vector<int>::iterator cell_ID=maskCells.begin(); cell_ID!=maskCells.end(); ++cell_ID)
			{ // loop on pixels

				unsigned x = *cell_ID % m_SeedMapIn->getXncell();
				unsigned y = trunc(*cell_ID / m_SeedMapIn->getXncell());

				if ((*m_SeedMapIn)(x,y,fg) > 0)
				{ // test of value of each pixel

					/* chose p= 100% of seeds into d1 disk and put 50% / area of disk seeds into */
					if (d1==0) /* all seeds fall in the same pixel */
					{
						new_SeedMapOut(x,y) += static_cast<int>( (*m_SeedMapIn)(x,y,fg) * 0.5 );
					} else  if (d1>0)
					{
						if (dispOption==1)
						{
							v1x_select.clear();
							v1y_select.clear();
							v1x_select = m_FGdistCircle[fg][0];
							v1y_select = m_FGdistCircle[fg][3];
						}
						for (unsigned id=0; id<v1x_select.size(); id++) /* loop on d1 disk */
						{
							xt = x + v1x_select[id];
							yt = y + v1y_select[id];
							if (xt>=0 && yt>=0 && xt < static_cast<int>(m_SeedMapIn->getXncell()) && yt < static_cast<int>(m_SeedMapIn->getYncell()))
							{
								if (dispOption==1)
								{
									new_SeedMapOut(xt,yt) += static_cast<int>( (*m_SeedMapIn)(x,y,fg) * 0.5 / static_cast<double>(v1x_select.size()) );
								} else if (dispOption==2 || dispOption==3)
								{
									new_SeedMapOut(xt,yt) += static_cast<int>( (*m_SeedMapIn)(x,y,fg) * prop_d1_select[id] );
								}
							}
						} // end of loop on d1 disk
					}

					/* chose as many as in first disk and into d1 d2 crow and put 49% / area of crown / p seeds into */
					if (d2 == 0)
					{
						new_SeedMapOut(x,y)+= static_cast<int>( (*m_SeedMapIn)(x,y,fg) * 0.49 );
					} else if (d2>0)
					{
						/* seeds will be dispersed in 2 neighbouring cells min disperse in 1 cell and one of its neighbour */
						if (dispOption==1)
						{
							v2x_select.clear();
							v2y_select.clear();
							v2x_select.reserve(noDrawMax);
							v2y_select.reserve(noDrawMax);

							UniInt distrib(0, m_FGdistCircle[fg][1].size());
							for (unsigned noDraw = 0; noDraw < noDrawMax; noDraw++)
							{ /* Draw of cells into crown that will received seeds */
								/*!*/
								int d2_draw = distrib(rng);
							  /*!*/
								v2x_select.push_back(m_FGdistCircle[fg][1][d2_draw]);
								v2y_select.push_back(m_FGdistCircle[fg][4][d2_draw]);
							}
						}
						for (unsigned id = 0; id < v2x_select.size(); id++)
						{
							xt = x + v2x_select[id];
							yt = y + v2y_select[id];
							if (xt>=0 && yt>=0 && xt < static_cast<int>(m_SeedMapIn->getXncell()) && yt < static_cast<int>(m_SeedMapIn->getYncell()))
							{
								/* First cell selected */
								if (dispOption==1)
								{
									new_SeedMapOut(xt,yt) += static_cast<int>( (*m_SeedMapIn)(x,y,fg) * 0.49 / (noDrawMax * 2.0) );

									/* x of its neighbour */
									UniInt distrib(0,3);
									switch(distrib(rng))
									{
										case 0 : xt++;
													break;
										case 1 : xt--;
													break;
										case 2 : yt++;
													break;
										case 3 : yt--;
													break;
									}
									if (xt>=0 && yt>=0 && xt < static_cast<int>(m_SeedMapIn->getXncell()) && yt < static_cast<int>(m_SeedMapIn->getYncell()))
									{
										new_SeedMapOut(xt,yt) += static_cast<int>( (*m_SeedMapIn)(x,y,fg) * 0.49 / (noDrawMax * 2.0) );
									}
								} else if (dispOption==2 || dispOption==3)
								{
									new_SeedMapOut(xt,yt) += static_cast<int>( (*m_SeedMapIn)(x,y,fg) * prop_d2_select[id] );
								}
							}
						}
					} // end of d50 -> d99 crown dispersal

					/* chose 1 cells into d1 d2 crow and put 1% / area of crown / p seeds into */
					if (dld == 0)
					{
						new_SeedMapOut(x,y) += static_cast<int>( (*m_SeedMapIn)(x,y,fg) * 0.01 );
					} else if (dld>0)
					{
						if(m_FGdistCircle[fg][2].size() > 0)
						{
							UniInt distrib(0, m_FGdistCircle[fg][2].size() - 1);

							/*!*/
							int LD_draw = distrib(rng); //rand()% vSize;
							/*!*/
							xt = x + m_FGdistCircle[fg][2][LD_draw];
							yt = y + m_FGdistCircle[fg][5][LD_draw];
							if (xt>=0 && yt>=0 && xt < static_cast<int>(m_SeedMapIn->getXncell()) && yt < static_cast<int>(m_SeedMapIn->getYncell()))
							{
								new_SeedMapOut(xt,yt) += static_cast<int>( (*m_SeedMapIn)(x,y,fg) * 0.01 );
							}
						}
					} // end of d99 -> ldd crown dispersal
				} // end test if some seed to disperse
			} // end loop over pixels

			m_SeedMapOut->setValues(fg, new_SeedMapOut.getValues());
		} else
		{
			logg.info("No seed dispersal for the PFG ", fg, "!");
		}
	} // end loop over PFGs
}
