/*============================================================================*/
/*                     Parameters reading utilities Class                     */
/*============================================================================*/

/*!
 * \file Params.h
 * \brief Utilites to read and use some parameters given into text files
 * \author Matthew Talluto, updated by Isabelle Boulangeat
 * \version 1.0
 * \date 2014/11/05
 */
 
#ifndef Params_h
#define Params_h

#include <string>
#include <map>
#include <vector>
#include <stdexcept>
#include <iostream>
#include <sstream>

using namespace std;


/*!
 * \class Params
 * \brief Utilites to read and use some parameters given into text files
 *
 * A parameter files should be a text file filled with parameters separated by
 * parameter names and delimiters. Comments can be skipped, and parameters may
 * be optional.
 */

namespace par
{
	vector<string> split(const string &s, const vector<char> &delim, const string &comment = "#");
	template<typename T>
	T str_convert(const string &s);
	
	class Params
	{
		map<string, vector<string> > data;
		const char * source;
		vector<char> delimiters;
		const string comment;
		
		void read_file();
		void get_lines(ifstream &file);
		
		public:
		
		Params(const char * f, const string &delim = " \t", const string &c = "#");
		
		template<typename T>
		vector<T> get_val(const string &key, const bool optional = false, const string &message = "") const;
	};
	
	// TEMPLATE FUNCTIONS
	
	template<typename T>
	vector<T> Params::get_val(const string &key, const bool optional, const string &message) const
	{
		vector<T> result;
		try
		{
			const vector<string> vals = data.at(key);
			for (unsigned i = 0; i < vals.size(); i++)
			{
				result.push_back(str_convert<T>(vals[i]));
			}
		}
		catch(const out_of_range& ex)
		{
			stringstream ss;
			if (! optional)
			{ // if param is not optional => return an error that will stop execution
				ss << "\n\nError: parameter parser tried to access unknown parameter <" << key << ">\t" << ex.what() << endl;
				ss << "!!! This is a required parameter. Please define it." << endl;
				ss << message << endl;
				throw (runtime_error (ss.str()));
			} else
			{ // just print a warning; you will have to test that parameter length is not null to know if parameter is recovered or not
				ss << "\nWarning: parameter parser tried to access unknown parameter <" << key << ">\t" << ex.what();
				cout << ss.str() << endl;
			}
		}
		return result;
	}
	
	template<typename T>
	T str_convert(const string &s)
	{
		T result;
		istringstream val(s); // create stream from the string
		if (!(val >> result))
		{
			stringstream ss;
			ss << "Cannot convert value <" << s << "> from string into requested type";
			throw( runtime_error (ss.str() ));
		}
		return result;
	}
} // end namespace par

#endif /* defined(Params_h) */
