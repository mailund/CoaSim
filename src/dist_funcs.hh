#ifndef DIST_FUNCTIONS_HH
#define DIST_FUNCTIONS_HH
#include <cmath>
#include <cstdlib>
#include <algorithm>
#include <iostream>


namespace Distribution_functions
{
  static double expdev(const double param)
  {
    return -log(double(std::rand())/double(RAND_MAX))/param;
  }

  static double expdev(const int fac, const double param)
  {
    return -log(double(std::rand())/double(RAND_MAX))/param/fac;
  }

  
  static double expdist(const double param, const double x)
  {
    return 1.0-exp(-param*x);
  }

  static double uniform()
  {
    return double(std::rand())/double(RAND_MAX);
  }
  
  static int random_sign()
  {
    int r = -1;
    if (uniform()>0.5) r = 1;
    return r;
  }
  
  static int uniform(double part_1, double part_2, double part_3)
  {
    double r = uniform()*(part_1+part_2+part_3);
    if (r<part_1) return 0;
    else if (r<part_1+part_2) return 1;
    return 2;
  }
  
  static int irand(int n)
  {
    int r = int((double(std::rand()) / RAND_MAX) * n);
    return std::min(n-1, r);
  }
  
  static void two_int_rand(int& n1, int& n2, int n)
  {
    n1 = irand(n);
    n2 = irand(n-1);
    if (n1==n2) n2 = n-1;
  }

};

#endif
