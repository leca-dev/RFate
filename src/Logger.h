#ifndef LOGGER_H
#define LOGGER_H

#include <omp.h>
#include "LogMessage.h"


class Logger
{
private:
  int m_verbosity;

public:
  Logger(int verbosity=0);
  ~Logger();
  void configure(int verbosity);

  template <typename T, typename... Types>
  void debug(const T& t1, const Types&... t2) const
  {
    #pragma omp critical
    {
      DebugMessage logMessage(m_verbosity, //"[DEBUG] : ",
                              t1, t2...);
      logMessage.show();
    }
  }

  template <typename T, typename... Types>
  void info(const T& t1, const Types&... t2) const
  {
    #pragma omp critical
    {
      InfoMessage logMessage(m_verbosity, t1, t2...);
      logMessage.show();
    }
  }

  template <typename T, typename... Types>
  void warning(const T& t1, const Types&... t2) const
  {
    {
      WarningMessage logMessage(m_verbosity, "[WARNING] : ", t1, t2...);
      logMessage.show();
    }
  }

  template <typename T, typename... Types>
  void error(const T& t1, const Types&... t2) const
  {
    {
      ErrorMessage logMessage(m_verbosity, "[ERROR] : ", t1, t2...);
      logMessage.show();
    }
  }
};


extern Logger logg;

#endif
