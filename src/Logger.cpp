#include "Logger.h"

using namespace std;


Logger::Logger(int verbosity) : m_verbosity(verbosity) {}


Logger::~Logger() {}


void Logger::configure(int verbosity)
{
  m_verbosity = verbosity;
}
