#ifndef BUILDER_HH
#define BUILDER_HH

#include <vector>
#include <sstream>
#include "node.hh"

class Builder
{
public:
  Builder(Configuration &conf) : _conf(conf) {};
  ~Builder() {};

  // Builds an ARG with the given number of leaf nodes.  The ARG is
  // dynamically allocated and must be deleted after use.
  ARG *build(size_t no_leaf_nodes) const;

private:
  Configuration &_conf;
};



#endif
