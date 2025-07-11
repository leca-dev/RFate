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

#include "LogMessage.h"

using namespace std;


LogMessage::~LogMessage() {}


void LogMessage::show() const
{
  // Fires the message only if verbosity tolerates its importance level.
  if (m_Importance > m_Verbosity)
  {
    // Use RcppThread output stream to handle parallel C++ vs monothread R
    RcppThread::Rcout << m_message << endl;
  }
}


void WarningMessage::show() const
{
  // Fires the message only if verbosity tolerates its importance level.
  if (m_Importance > m_Verbosity)
  {
    // Use Rcpp::warning to transfert C++ exception into R warning.
    Rcpp::warning(m_message);  // !!Will crash R when multithreading!!
  }
}


void ErrorMessage::show() const
{
  // Fires the message only if verbosity tolerates its importance level.
  if (m_Importance > m_Verbosity)
  {
    // Use Rcpp::stop to transfert C++ exception into R error.
    Rcpp::stop(m_message);  // !!Will crash R when multithreading!!
  }
}
