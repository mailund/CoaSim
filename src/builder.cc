#include "builder.hh"

void Builder::build(std::vector<Node*>& leaf_nodes, std::vector<Retired_intervals*>& finished_intervals)
{
  std::vector<Node*> _top_nodes(0);
  for (unsigned int i=0; i<leaf_nodes.size(); i++){
    if (leaf_nodes[i]->validate()){
      _top_nodes.push_back(leaf_nodes[i]);
    }
    else {
      Reporter::append(_log,"non-valid leaf node");
    }
  }

  int k = 0;
  int n;
  int type = 0;
  int node_1;
  int node_2;
  double time = 0.0;
  int cnt = 0;
  int coalescent = 0;
  int recombination = 0;
  int genconversion = 0;
  double time_interval = 0.0;
  double time_1 = 0.0;
  double time_2 = 0.0;
  Coalescent_node* coa_node;
  Genconversion_node* gcon_node_1;
  Genconversion_node* gcon_node_2;
  Recombination_node* rcom_node_1;
  Recombination_node* rcom_node_2;
  int initial_size = _top_nodes.size();
  while (_top_nodes.size()>=2) {
    k = _top_nodes.size();
    if (has_growth()){
      time_1 = log(1.0+growth*Distribution_functions::expdev(k,(k-1)/2)*exp(-growth*time))/growth;
      time_2 = Distribution_functions::expdev(k,G+rho );
      if (time_1 < time_2) time_interval = time_1;
      else time_interval = time_2;
    }
    else {
      time_interval = Distribution_functions::expdev(k,(k-1)/2.+G+rho );
    }
    time += time_interval;
    type = Distribution_functions::uniform((k-1)/2.,G, rho); //k has been divided out of all terms to insure no integer overflow
    // 0 if coalescent, 1 if geneConversion, 2 for recombination
    switch (type) {
    case 0:
      coalescent++;
      Distribution_functions::two_int_rand(node_1,node_2,_top_nodes.size());
      coa_node = (*_top_nodes[node_1]) + (*_top_nodes[node_2]); //implements a addition operator on node
      coa_node->set_time(time);
      if (node_2==int(_top_nodes.size())-1){
	_top_nodes.pop_back();
	std::swap(_top_nodes[node_1], _top_nodes.back());
	_top_nodes.pop_back();
      }
      else{
	std::swap(_top_nodes[node_1], _top_nodes.back());
	_top_nodes.pop_back();
	std::swap(_top_nodes[node_2], _top_nodes.back());
	_top_nodes.pop_back();
      }
      retire(coa_node, leaf_nodes.size(),_top_nodes,finished_intervals);
      break;    
    case 1:
      genconversion++;
      n = Distribution_functions::irand(_top_nodes.size());
      _top_nodes[n] -> genconversion(gcon_node_1, gcon_node_2, Q, time);
      if (gcon_node_1!=0){ // it is enough if one of the nodes are created becouse either is both or none created
	std::swap(_top_nodes[n], _top_nodes.back());
	_top_nodes.pop_back();
	_top_nodes.push_back(gcon_node_1);
	_top_nodes.push_back(gcon_node_2);
      }
      break;
    case 2:
      recombination++;
      n = Distribution_functions::irand(_top_nodes.size());
      _top_nodes[n] -> recombination(rcom_node_1, rcom_node_2, time);
      if (rcom_node_1!=0){// it is enough if one of the nodes is created becouse either is both or none created
	std::swap(_top_nodes[n], _top_nodes.back());
	_top_nodes.pop_back();
	_top_nodes.push_back(rcom_node_1);
	_top_nodes.push_back(rcom_node_2);
      }
      break;
    }
    cnt++;
    if ((cnt>999)&&(cnt%1000==0)){
      Reporter::write_progress(_log+"_progress.dat", _top_nodes.size(),initial_size);
    }
  }
  if (_top_nodes.size()==1)
    finished_intervals.push_back(new Retired_intervals((&(_top_nodes[0]->interval())), _top_nodes[0])); 
};

void Builder::retire(Coalescent_node* coa_node, int leaf_nodes, std::vector<Node*>& _top_nodes, std::vector<Retired_intervals*>& finished_intervals)
{
  if (Intervals* intervals = (coa_node->has_finished_intervals(leaf_nodes))){
     Retired_intervals* r_int = new Retired_intervals(intervals, coa_node);
    finished_intervals.push_back(r_int);
  }
  if (coa_node->retire_finished_intervals(leaf_nodes)){
    _top_nodes.push_back(coa_node);
  }

  //   for (unsigned int i=0; i<finished_intervals.size(); i++)
  //     for (int j=0; j<(finished_intervals[i]->interval()).size(); j++) std::cout << i << ", " << j << " : " << (finished_intervals[i]->interval(j)).start() << " -> " << (finished_intervals[i]->interval(j)).end() << std::endl;
};

void Builder::cleanup(std::vector<Retired_intervals*>& finished_intervals)
{
  for (unsigned int i = 0; i< finished_intervals.size(); i++){
    delete finished_intervals[i];
  }
  finished_intervals.resize(0);
  Coalescent_node::cleanup();
  Recombination_node::cleanup();
  Genconversion_node::cleanup();
};
