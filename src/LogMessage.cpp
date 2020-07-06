#include "LogMessage.h"

using namespace std;


LogMessage::~LogMessage() {}


void LogMessage::show() const
{
  if (m_importance > m_verbosity)
  {
    RcppThread::Rcout << m_message << endl;
  }
}


// void WarningMessage::show() const
// {
//   if (m_importance > m_verbosity)
//   {
//     #pragma omp critical
//     Rcpp::warning(m_message);
//   }
// }


void ErrorMessage::show() const
{
  if (m_importance > m_verbosity)
  {
    #pragma omp critical
    Rcpp::stop(m_message);
  } else
  {
    #pragma omp critical
    Rcpp::stop("ERROR");
  }
}
