#ifndef DESCENDER_HH_INCLUDED
#define DESCENDER_HH_INCLUDED

#ifndef CONFIGURATION_HH_INCLUDED
# include "configuration.hh"
#endif

class ARG;

class Descender
{
public:
  Descender(const Configuration &conf) : _conf(conf) {}
  ~Descender() {}

  // assign evolution to the ARG as specified by the configuration.
  // If the traits cannot be assigned according to specification, the
  // method returns `false' which means that a new ARG should be build
  // and processed.  If everything goes well, evolve returns `true'.
  void evolve(ARG &arg) const;

private:
  const Configuration &_conf;
};

#endif
