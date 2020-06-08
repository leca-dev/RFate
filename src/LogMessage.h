#ifndef LOGMESSAGE_H
#define LOGMESSAGE_H

#include <Rcpp.h>
#include <sstream>
#include <string>
#include <vector>


template <typename T>
std::string& appendMessage(std::string& message, const T& t)
{
  std::ostringstream oss;
  oss << t;
  message += oss.str();
  return message;
}


template <typename T>
std::string& appendMessage(std::string& message, const std::vector<T>& v)
{
  if(!v.empty())
  {
    std::ostringstream oss;
    std::copy(v.begin(), v.end(), std::ostream_iterator<T>(oss, " "));
    message += oss.str();
  }
  return message;
}


template <typename T>
std::string& appendMessage(std::string& message,
                           const std::vector<std::vector<T>>& m)
{
  typename std::vector<std::vector<T>>::const_iterator v;
  for (v = m.begin(); v != m.end(); ++v)
  {
    message += "\n";
    appendMessage(message, *v);
  }
  return message;
}


template <typename T>
std::string& appendMessage(std::string& message,
                           const std::vector<std::vector<std::vector<T>>>& m)
{
  typename std::vector<std::vector<std::vector<T>>>::const_iterator v;
  for (v = m.begin(); v != m.end(); ++v)
  {
    appendMessage(message, *v);
    message += "\n";
  }
  return message;
}


template <typename T, typename... Types>
std::string& appendMessage(std::string& message, const T& t1,
                           const Types&... t2)
{
  appendMessage(message, t1);
  appendMessage(message, t2...);
}


class LogMessage
{
protected:
  int m_importance, m_verbosity;
  std::string m_message;

public:
  template <typename T, typename... Types>
  LogMessage(int importance, int verbosity, const T& t1, const Types&... t2) :
  m_importance(importance), m_verbosity(verbosity)
  {
    if (m_importance > m_verbosity)
    {
      appendMessage(m_message, t1, t2...);
    }
  }
  void show() const;
};


class DebugMessage: public LogMessage
{
public:
  template <typename T, typename... Types>
  InfoMessage(int verbosity, const T& t1, const Types&... t2) :
  LogMessage(1, verbosity, t1, t2...) {}
};


class InfoMessage: public LogMessage
{
public:
  template <typename T, typename... Types>
  WarningMessage(int verbosity, const T& t1, const Types&... t2) :
  LogMessage(2, verbosity, t1, t2...) {}
  void show() const;
};


class WarningMessage: public LogMessage
{
public:
  template <typename T, typename... Types>
  WarningMessage(int verbosity, const T& t1, const Types&... t2) :
  LogMessage(3, verbosity, t1, t2...) {}
  void show() const;
};


class ErrorMessage: public LogMessage
{
public:
  template <typename T, typename... Types>
  ErrorMessage(int verbosity, const T& t1, const Types&... t2) :
  LogMessage(4, verbosity, t1, t2...) {}
  void show() const;
};

#endif
