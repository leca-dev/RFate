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
/*                             LogMessage Class                               */
/*============================================================================*/

/*!
 * \file LogMessage.h
 * \brief LogMessage Class
 * \author Guillaume Dupouy
 * \version 1.0
 * \date 2020/09/15
 */

#ifndef LOGMESSAGE_H
#define LOGMESSAGE_H

#include <Rcpp.h>
#include <RcppThread.h>
#include <sstream>
#include <iterator>
#include <string>
#include <vector>

using namespace std;

/**
  Append a message string with any single element.
  @param message initial message string.
  @param t further element to merge to the message.
  @return message the updated message

  appendMessage("nono ", 1) -> "nono 1"
*/
template <typename T>
string& appendMessage(string& message, const T& t)
{
  ostringstream oss;
  oss << t;
  message += oss.str();
  return message;
}

/**
  Append a message string with any 1D-vector of elements.
  @param message initial message string.
  @param v further vector of elements to merge to the message.
  @return message the updated message

  appendMessage("nono ", [1, 2]) -> "nono 1 2"
*/
template <typename T>
string& appendMessage(string& message, const vector<T>& v)
{
  if(!v.empty())
  {
    ostringstream oss;
    copy(v.begin(), v.end(), ostream_iterator<T>(oss, " "));
    message += oss.str();
  }
  return message;
}

/**
  Append a message string with any 2D-vector of elements.
  @param message initial message string.
  @param m further vector of elements to merge to the message.
  @return message the updated message

  appendMessage("nono ", [[1, 2],  -> "nono
                          [3, 4]])     1 2
                                       3 4"
*/
template <typename T>
string& appendMessage(string& message, const vector<vector<T>>& m)
{
  typename vector<vector<T>>::const_iterator v;
  for (v = m.begin(); v != m.end(); ++v)
  {
    message += "\n";
    appendMessage(message, *v);
  }
  return message;
}

/**
  Append a message string with any 3D-vector of elements.
  @param message initial message string.
  @param m further vector of elements to merge to the message.
  @return message the updated message

  appendMessage("nono ", [[[1, 2],      "nono
                           [3, 4]],  ->  1 2
                          [[5, 6],       3 4
                           [7, 8]]])
                                         5 6
                                         7 8
                                         "
*/
template <typename T>
string& appendMessage(string& message, const vector<vector<vector<T>>>& m)
{
  typename vector<vector<vector<T>>>::const_iterator v;
  for (v = m.begin(); v != m.end(); ++v)
  {
    appendMessage(message, *v);
    message += "\n";
  }
  return message;
}

/**
  Append a message string with any list of elements or vectors as set before.
  @param message initial message string.
  @param t further element or vector of elements to merge to the message.
  @param t2 further element or vector of elements to merge to the message.
  @return message the updated message

  appendMessage("nono ", 1, [[2, 3], [4, 5]]) -> "nono 1
                                                  2 3
                                                  4 5"
*/
template <class T, class ...Types>
string& appendMessage(string& message, const T& t, const Types&... t2)
{
  appendMessage(message, t);
  appendMessage(message, t2...);
  return message;
}


/**
  @class LogMessage
  Holds a message string with a given importance, compared to a verbosity level.
*/
class LogMessage
{
protected:
  string m_message;  // Log message.
  int m_importance, m_verbosity;  // message importance and verbosity level.

public:
  /**
    LogMessage constructor.
    @param importance importance level of the message.
    @param verbosity verbosity level.
    @param t1 content of the message.
    @param t2 any further contents of the message.
  */
  template <typename T, typename... Types>
  LogMessage(int importance, int verbosity, const T& t1, const Types&... t2) :
    m_importance(importance), m_verbosity(verbosity)
  {
    // Fill the message string only if verbosity tolerates its importance level.
    if (m_importance > m_verbosity)
    {
      appendMessage(m_message, t1, t2...);
    }
  }

  /**
    LogMessage destructor.
  */
  ~LogMessage();

  /**
    Shows the message only if verbosity tolerates its importance level.
  */
  void show() const;
};


/**
  @class DebugMessage
  LogMessage with importance level 1.
*/
class DebugMessage: public LogMessage
{
public:
  /**
    DebugMessage constructor.
    @param verbosity verbosity level.
    @param t1 content of the message.
    @param t2 any further contents of the message.
  */
  template <typename T, typename... Types>
  DebugMessage(int verbosity, const T& t1, const Types&... t2) :
    LogMessage(1, verbosity, t1, t2...) {}
};


/**
  @class InfoMessage
  LogMessage with importance level 2.
*/
class InfoMessage: public LogMessage
{
public:
  /**
    InfoMessage constructor.
    @param verbosity verbosity level.
    @param t1 content of the message.
    @param t2 any further contents of the message.
  */
  template <typename T, typename... Types>
  InfoMessage(int verbosity, const T& t1, const Types&... t2) :
    LogMessage(2, verbosity, t1, t2...) {}
};


/**
  @class WarningMessage
  LogMessage with importance level 3.
*/
class WarningMessage: public LogMessage
{
public:
  /**
    WarningMessage constructor.
    @param verbosity verbosity level.
    @param t1 content of the message.
    @param t2 any further contents of the message.
  */
  template <typename T, typename... Types>
  WarningMessage(int verbosity, const T& t1, const Types&... t2) :
  LogMessage(3, verbosity, t1, t2...) {}

  /**
    Shows the message only if verbosity tolerates its importance level.
  */
  void show() const;
};


/**
  @class ErrorMessage
  LogMessage with importance level 4.
*/
class ErrorMessage: public LogMessage
{
public:
  /**
    ErrorMessage constructor.
    @param verbosity verbosity level.
    @param t1 content of the message.
    @param t2 any further contents of the message.
  */
  template <typename T, typename... Types>
  ErrorMessage(int verbosity, const T& t1, const Types&... t2) :
  LogMessage(4, verbosity, t1, t2...) {}

  /**
    Shows the message only if verbosity tolerates its importance level.
  */
  void show() const;
};

#endif