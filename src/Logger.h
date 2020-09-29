/**
  @file Logger.h
  @author Guillaume dupouy
  @version 1.0
  @date 2020/09/15
*/

#ifndef LOGGER_H
#define LOGGER_H

#include "openmp.h"
#include "LogMessage.h"


/**
  @class Logger
  Manage output streams according to verbosity level.
  A Logger stores a verbosity level (0: shows any message,
                                     1: shows any message except debug,
                                     2: shows only warning and error messages,
                                     3: shows only error messages,
                                     4+: mute).
  It shows LogMessage instances, or ingnores it, depending on the verbosity
  level.
*/
class Logger
{
private:
  int m_verbosity;  // verbosity level

public:
	/**
    Logger constructor.
    Verbosity default to 0 (shows any LogMessage)
	*/
  Logger(int verbosity=0);

  /**
    Logger destructor
	*/
  ~Logger();

  /**
    Sets verbosity level of the Logger instance.
    @param verbosity verbosity level.
	*/
  void configure(int verbosity);

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
      DebugMessage logMessage(m_verbosity, t1, t2...);
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
      InfoMessage logMessage(m_verbosity, t1, t2...);
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
      WarningMessage logMessage(m_verbosity, "[WARNING] : ", t1, t2...);
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
      ErrorMessage logMessage(m_verbosity, "[ERROR] : ", t1, t2...);
      logMessage.show();  // Fires message if verbosity <= 3.
    }
  }
};


extern Logger logg;

#endif
