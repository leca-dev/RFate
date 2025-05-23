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

#include <fstream>
#include "Params.h"

using namespace std;

par::Params::Params(const char * f, const string &delim, const string &c) : source(f), comment(c)
{
	for (unsigned i = 0; i < delim.length(); i++)
	{
		delimiters.push_back(delim[i]);
	}
	read_file();
}

void par::Params::read_file()
{
	ifstream inputFile (source);
	if (!inputFile.is_open())
	{
		logg.error("Error: problem reading from file <", source, ">");
	}
	get_lines(inputFile);
	inputFile.close();
}

void par::Params::get_lines(ifstream &file)
{
	while (file.good())
	{
		static size_t lineno = 0; // for reporting the line number of errors
		string line;
		getline(file, line);
		lineno++;

		vector<string> lineData;
		string varName;
		try
		{
			lineData = split(line, delimiters);
		}
		catch(...)
		{
			logg.error("Error: problem parsing input, line ", lineno);
		}

		if (lineData.size() <= 1) continue; // skip lines that are empty or were commented
		varName = lineData[0];
		lineData.erase(lineData.begin());
		data[varName] = lineData;
	}
}

vector<string> par::split(const string &s, const vector<char> &delim, const string &comment)
{
	vector<string> dest;

	// ignore commented lines
	if (s.substr(0,comment.length()) == comment)	return dest;

	stringstream ls(s); // create stringstream out of input
	string dat;
	while (getline(ls, dat, delim.back()))
	{
		if (delim.size() > 1)
		{
			vector<string> fillDest = split(dat, vector<char> (delim.begin(), delim.end() - 1), comment);
			dest.insert(dest.end(), fillDest.begin(), fillDest.end());
		} else
		{
			dest.push_back(dat);
		}
	}
	return dest;
}
