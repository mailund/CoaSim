#ifndef BUILDER_HH_INCLUDED
#define BUILDER_HH_INCLUDED

#ifndef NODE_HH_INCLUDED
# include "node.hh"
#endif


class Builder
{
public:
  Builder(const Configuration &conf) : _conf(conf) {};
  ~Builder() {};

  // Builds an ARG.  The ARG is dynamically allocated and must be
  // deleted after use.
  ARG *build() const;

private:
  const Configuration &_conf;
};



#endif
