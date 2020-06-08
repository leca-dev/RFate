#include "Logger.h"

using namespace std;


Logger::Logger(int verbosity) : m_verbosity(verbosity) {}


void Logger::configure(int verbosity)
{
  m_verbosity = verbosity;
}
