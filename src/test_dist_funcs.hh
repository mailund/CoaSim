
#ifndef TEST_DIST_FUNCTIONS_HH
#define TEST_DIST_FUNCTIONS_HH

namespace Distribution_functions
{
  extern double expdev_result;
  inline double expdev(const double param) { return expdev_result; }
  inline double expdev(const int fac, const double param)
  { return expdev_result; }

  extern double expdist_result;
  inline double expdist(const double param, const double x)
  { return expdist_result; }

  extern double uniform_result;
  inline double uniform() { return uniform_result; }
  
  extern int random_sign_result;
  inline int random_sign() { return random_sign_result; }
  
  extern int uniform_part_result;
  inline int uniform(double part_1, double part_2, double part_3)
  { return uniform_part_result; }

  extern int irand_result;
  inline int irand(int n)
  { return irand_result; }

  extern int two_int_rand1;
  extern int two_int_rand2;
  inline void two_int_rand(int& n1, int& n2, int n)
  { n1 = two_int_rand1 ; n2 = two_int_rand2; }
};

#endif
