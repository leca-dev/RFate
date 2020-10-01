#include "LogMessage.h"

using namespace std;


LogMessage::~LogMessage() {}


void LogMessage::show() const
{
  // Fires the message only if verbosity tolerates its importance level.
  if (m_importance > m_verbosity)
  {
    // Use RcppThread output stream to handle parallel C++ vs monothread R
    RcppThread::Rcout << m_message << endl;
  }
}


void WarningMessage::show() const
{
  // Fires the message only if verbosity tolerates its importance level.
  if (m_importance > m_verbosity)
  {
    // Use Rcpp::warning to transfert C++ exception into R warning.
    Rcpp::warning(m_message);  // !!Will crash R when multithreading!!
  }
}


void ErrorMessage::show() const
{
  // Fires the message only if verbosity tolerates its importance level.
  if (m_importance > m_verbosity)
  {
    // Use Rcpp::stop to transfert C++ exception into R error.
    Rcpp::stop(m_message);  // !!Will crash R when multithreading!!
  }
}