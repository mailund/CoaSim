
#ifndef SIMULATOR_HH_INCLUDED
#define SIMULATOR_HH_INCLUDED

class ARG;
class Configuration;

namespace Simulator {
  // Returns the resulting ARG, or 0 if the simulation was aborted
  ARG *simulate(const Configuration &conf);
}

#endif // SIMULATOR_HH_INCLUDED
