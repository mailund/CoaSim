
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
#ifndef MONITOR_HH_INCLUDED
# include "monitor.hh"
#endif

ARG *Simulator::simulate(const Configuration &conf)
{
  Builder builder(conf);
  Descender descender(conf);
  ARG *arg = 0;
  SimulationMonitor *mon = conf.monitor();

  try {

  retry:
    try {
      if (mon) mon->start_arg_building();
      arg = builder.build();
      if (mon) mon->start_mutating();
      descender.evolve(*arg);
    } catch (Mutator::retry_arg&) {
      if (mon) mon->retry_arg_building();
      goto retry;
    }

    if (mon) mon->simulation_terminated();

  } catch(SimulationMonitor::AbortSimulation&) {
    if (arg) delete arg; arg = 0;
  }

  return arg;
}
