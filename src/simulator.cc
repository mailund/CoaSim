
#include "simulator.hh"

#ifndef BUILDER_HH_INCLUDED
# include "builder.hh"
#endif
#ifndef DESCENDER_HH_INCLUDED
# include "descender.hh"
#endif
#ifndef MARKER_HH_INCLUDED
# include "marker.hh"
#endif

ARG *Simulator::simulate(const Configuration &conf)
{
  Builder builder(conf);
  Descender descender(conf);
  ARG *arg = 0;

 retry:
  try {
    arg = builder.build();
    descender.evolve(*arg);
  } catch (Mutator::retry_arg&) {
    goto retry;
  }

  return arg;
}
