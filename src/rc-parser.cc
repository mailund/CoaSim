
#include "rc-parser.hh"

#ifndef SSTREAM_INCLUDED
# include <sstream>
# define SSTREAM_INCLUDED
#endif


static void parse_run_commands(std::istream &is, 
			       std::map<std::string,std::string> &m)
{
  std::string line;
  while (getline(is,line))
    {
      std::string::iterator i;
      i = find(line.begin(), line.end(), '#');
      if (i != line.end())
	// comment
	line.erase(i,line.end());

      i = find(line.begin(), line.end(), ':');
      if (i == line.end())
	{
	  // warning, no key/value pair!
	}
      else
	{
	  std::string key(line.begin(),i);
	  std::string val(i+1,line.end());

	  static const char *ws = " \t";
	  std::string::size_type start_key, stop_key;
	  std::string::size_type start_val, stop_val;

	  start_key = key.find_first_not_of(ws);
	  stop_key = std::min(key.find_last_not_of(ws)+1, key.size());

	  std::string real_key = key.substr(start_key,stop_key);

	  start_val = val.find_first_not_of(ws);
	  stop_val = std::min(val.find_last_not_of(ws)+1, val.size());

	  std::string real_val = val.substr(start_val,stop_val);

	  m[real_key] = real_val;
	}
    }
}


RCParser::RCParser(std::istream &is)
{
  parse_run_commands(is,i_m);
}

int RCParser::get_int(const std::string &key)
{
  return atoi(i_m[key].c_str());
}

std::string RCParser::get_string(const std::string &key)
{
  return i_m[key];
}

double RCParser::get_double(const std::string &key)
{
  return atof(i_m[key].c_str());
}

bool RCParser::get_bool(const std::string &key)
{
  const std::string &val = i_m[key];
  if (val == "true" or atoi(val.c_str()) != 0) return true;
  return false;
}


std::vector<int> RCParser::get_int_vector(const std::string &key)
{
  std::istringstream is(i_m[key]);
  std::vector<int> v;
  int i;
  while (is >> i) v.push_back(i);
  return v;
}

std::vector<std::string> RCParser::get_string_vector(const std::string &key)
{
  std::istringstream is(i_m[key]);
  std::vector<std::string> v;
  std::string s;
  while (is >> s) v.push_back(s);
  return v;
}

std::vector<double> RCParser::get_double_vector(const std::string &key)
{
  std::istringstream is(i_m[key]);
  std::vector<double> v;
  double d;
  while (is >> d) v.push_back(d);
  return v;
}
