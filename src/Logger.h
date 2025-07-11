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
/*                                Logger Class                                */
/*============================================================================*/

/*!
 * \file Logger.h
 * \brief Logger Class
 * \author Guillaume Dupouy
 * \version 1.0
 * \date 2020/09/15
 */


#ifndef LOGGER_H
#define LOGGER_H

#include "openmp.h"
#include "LogMessage.h"

/*!
 * \class Logger
 * \brief Manage output streams according to verbosity level.
 *
 * A Logger stores a verbosity level
 *    0: shows any message,
 *    1: shows any message except debug,
 *    2: shows only warning and error messages,
 *    3: shows only error messages,
 *    4+: mute.
 * It shows LogMessage instances, or ignores it, 
 * depending on the verbosity level.
 */

class Logger
{
  private:
    
  int m_Verbosity;  // verbosity level
  
  public:
  
  /*-------------------------------------------*/
  /* Constructors -----------------------------*/
  /*-------------------------------------------*/
  
  /*!
   *	\brief Default constructor
   *
   *	Logger default constructor => Verbosity default to 0 (shows any LogMessage)
   */
  Logger();
    
    /*!
     *	\brief Full constructor
     *
     *	Logger full constructor
     *
     *	\param verbosity : verbosity level
     *	    0: shows any message
     *	    1: shows any message except debug
     *	    2: shows only warning and error messages
     *	    3: shows only error messages
     *	    4+: mute
     */
    Logger(int verbosity);

    
  /*-------------------------------------------*/
  /* Destructor -------------------------------*/
  /*-------------------------------------------*/
    
  /*!
   *	\brief Destructor
   *
   *	Logger destructor
   */
  ~Logger();
  
  /*-------------------------------------------*/
  /* Getters & Setters ------------------------*/
  /*-------------------------------------------*/
  
  int getVerbosity() const;
  
  void setVerbosity(const int& verbosity);
  
  /*-------------------------------------------*/
  /* Other functions --------------------------*/
  /*-------------------------------------------*/

  /**
    Fires a DebugMessage instance.
    @param t1 message content.
    @param t2 message content.
	*/
  template <typename T, typename... Types>
  void debug(const T& t1, const Types&... t2) const
  {
    // omp critical to avoid simultaneous couts when multithreading.
    #pragma omp critical
    {
      DebugMessage logMessage(m_Verbosity, t1, t2...);
      logMessage.show();  // Fires message if verbosity <= 0.
    }
  }

  /**
    Fires a InfoMessage instance.
    @param t1 message content.
    @param t2 message content.
	*/
  template <typename T, typename... Types>
  void info(const T& t1, const Types&... t2) const
  {
    // omp critical to avoid simultaneous couts when multithreading.
    #pragma omp critical
    {
      InfoMessage logMessage(m_Verbosity, t1, t2...);
      logMessage.show();  // Fires message if verbosity <= 1.
    }
  }

  /**
    Fires a WarningMessage instance.
    @param t1 message content.
    @param t2 message content.
	*/
  template <typename T, typename... Types>
  void warning(const T& t1, const Types&... t2) const
  {
    // Don't need omp critical there: an exception while multithreading will
    // crash R anyway...
    {
      WarningMessage logMessage(m_Verbosity, "[WARNING] : ", t1, t2...);
      logMessage.show();  // Fires message if verbosity <= 2.
    }
  }

  /**
    Fires a ErrorMessage instance.
    @param t1 message content.
    @param t2 message content.
	*/
  template <typename T, typename... Types>
  void error(const T& t1, const Types&... t2) const
  {
    // Don't need omp critical there: an exception while multithreading will
    // crash R anyway...
    {
      ErrorMessage logMessage(m_Verbosity, "[ERROR] : ", t1, t2...);
      logMessage.show();  // Fires message if verbosity <= 3.
    }
  }
};


extern Logger logg;

#endif