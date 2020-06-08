#include "LogMessage.h"

using namespace std;


void LogMessage::show() const
{
  if (m_importance > m_verbosity)
  {
    Rcpp::Rcout << m_message << endl;
  }
}


void WarningMessage::show() const
{
  if (m_importance > m_verbosity)
  {
    Rcpp::warning(m_message);
  }
}


void ErrorMessage::show() const
{
  if (m_importance > m_verbosity)
  {
    Rcpp::stop(m_message);
  } else
  {
    Rcpp::stop("ERROR");
  }
}
