#ifndef REPORTER_HH
#define REPORTER_HH
#include <iostream>
#include <fstream>

class Reporter
{
public:
  Reporter(){};
  ~Reporter(){};
  static void append(std::string file, std::string text)
  {
    std::ofstream to(file.c_str(), std::ofstream::app);
    to << text << std::endl;
    to.flush();
    to.close();
  };

  static void write_progress(std::string file, int n, int size)
  {
    std::ofstream to(file.c_str());
    to << n << " " << size << std::endl;
    to.flush();
    to.close();
  };
private:
};

#endif
