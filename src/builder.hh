#ifndef BUILDER_HH_INCLUDED
#define BUILDER_HH_INCLUDED

#ifndef NODE_HH_INCLUDED
# include "node.hh"
#endif


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
