#ifndef BUILDER_HH
#define BUILDER_HH
#include <vector>
#include <sstream>
#include "node.hh"
#include "dist_funcs.hh"
#include "reporter.hh"

class Builder{
public:
  Builder(double _rho, double _G, double _Q, double _growth, std::string log): rho(_rho), Q(_Q), G(_G), growth(_growth), _log(log){};
  ~Builder(){};
  void cleanup(std::vector<Retired_intervals*>& finished_intervals);
  void build(std::vector<Node*>& leaf_nodes, std::vector<Retired_intervals*>& finished_intervals);
  void retire(Coalescent_node* coa_node, int leaf_nodes, std::vector<Node*>& _top_nodes, std::vector<Retired_intervals*>& finished_intervals);
  bool has_growth(){ return ( fabs(growth) > 0.001); };
private:
  double rho;
  double Q;
  double G;
  double growth;
  std::string _log;
};

#endif
