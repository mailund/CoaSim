
#ifndef RC_PARSER_HH_INCLUDED
#define RC_PARSER_HH_INCLUDED

#ifndef IOSTREAM_INCLUDED
# include <iostream>
# define IOSTREAM_INCLUDED
#endif
#ifndef MAP_INCLUDED
# include <map>
# define MAP_INCLUDED
#endif
#ifndef STRING_INCLUDED
# include <string>
# define STRING_INCLUDED
#endif
#ifndef VECTOR_INCLUDED
# include <vector>
# define VECTOR_INCLUDED
#endif

class RCParser {
  // disable these
  RCParser(const RCParser&);
  RCParser &operator = (const RCParser&);
  
public:
  RCParser(std::istream &is);

  int         get_int   (const std::string &key);
  std::string get_string(const std::string &key);
  double      get_double(const std::string &key);
  bool        get_bool  (const std::string &key);

  std::vector<int>         get_int_vector   (const std::string &key);
  std::vector<std::string> get_string_vector(const std::string &key);
  std::vector<double>      get_double_vector(const std::string &key);

private:
  std::map<std::string,std::string> i_m;
};

#endif // RC_PARSER_HH_INCLUDED
