
#ifndef SIMULATOR_HH_INCLUDED
#define SIMULATOR_HH_INCLUDED

class ARG;
class Configuration;

namespace Simulator {
  ARG *simulate(const Configuration &conf);
}

#endif // SIMULATOR_HH_INCLUDED
