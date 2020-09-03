#include "LogMessage.h"
//#include <iostream>

using namespace std;


LogMessage::~LogMessage() {}


void LogMessage::show() const
{
  if (m_importance > m_verbosity)
  {
    RcppThread::Rcout << m_message << endl;
    //cout << m_message << endl;
  }
}


void WarningMessage::show() const
{
  if (m_importance > m_verbosity)
  {
    Rcpp::warning(m_message);
    //cerr << m_message << endl;
  }
}


void ErrorMessage::show() const
{
  if (m_importance > m_verbosity)
  {
    Rcpp::stop(m_message);
    //cerr << m_message << endl;
  }
  // else
  // {
  //   // Rcpp::stop("ERROR");
  // }
  //terminate();
  // LogMessage::show();
  // throw runtime_error("Error");
}
