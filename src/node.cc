#include "node.hh"
#include "dist_funcs.hh"

#include <sstream>
#include<string>
#include <valarray>


namespace {

  class LeafNode : public ARG::Node
  {
    friend Node *ARG::leaf();
    LeafNode() : Node(0.0) {}
  };


  // WARNING: None of the following classes checks whether they are
  // initialized with a null-child, but will crash if that is the
  // case.  They should only be created with the corresponding factory
  // method anyway, and it checks for it, so *DON'T* create these
  // objects in any other way!

  class CoalescentNode : public ARG::Node
  {
    friend Node *ARG::coalescence(double,Node*,Node*);
    CoalescentNode(double time, Node *left, Node *right, const Intervals &is)
      : Node(time,is), _left(left), _right(right)
    {}
    Node *const _left;
    Node *const _right;
  };
  
  class RecombinationNode : public ARG::Node
  {
    friend ARG::node_pair_t ARG::recombination(double,Node*);
    RecombinationNode(double time, Node *child, const Intervals &is)
      : Node(time,is), _child(child)
    {}
    Node *const _child;
  };

  class GeneConversionNode : public ARG::Node
  {
    friend ARG::node_pair_t ARG::gene_conversion(double,Node*);
    GeneConversionNode(double time, Node *child, const Intervals &is)
      : Node(time,is), _child(child)
    {}
    Node *const _child;
  };
}


ARG::~ARG()
{
  std::vector<Node*>::iterator itr;
  for (itr = _node_pool.begin(); itr != _node_pool.end(); ++itr)
    delete *itr;
}

ARG::Node *ARG::leaf() throw()
{
  LeafNode *n = new LeafNode();
  _node_pool.push_back(n);
  return n;
}

ARG::Node *ARG::coalescence(double time, Node *left, Node *right)
  throw(null_child)
{
  if (left == 0 or right == 0) throw null_child();
  CoalescentNode *n =
    new CoalescentNode(time,left,right,left->intervals()|right->intervals());
  _node_pool.push_back(n);
  return n;
}

ARG::node_pair_t ARG::recombination(double time, Node *child)
  throw(null_child)
{
  if (child == 0) throw null_child();

  double cross_over_point = Distribution_functions::uniform();
  if (cross_over_point <= child->intervals().first_point())
    return std::make_pair<Node*,Node*>(child,0);
  if (child->intervals().last_point() <= cross_over_point)
    return std::make_pair<Node*,Node*>(child,0);

  Intervals left  = child->intervals().copy(0,cross_over_point);
  Intervals right = child->intervals().copy(cross_over_point,1);

  RecombinationNode *n1 = new RecombinationNode(time,child,left);
  RecombinationNode *n2 = new RecombinationNode(time,child,right);
  _node_pool.push_back(n1); _node_pool.push_back(n2);

  return std::make_pair(n1,n2);
}

ARG::node_pair_t ARG::gene_conversion(double time, Node *child)
  throw(null_child)
{
  if (child == 0) throw null_child();
  return std::make_pair<Node*,Node*>(child,0);
#if 0 // FIXME
  GeneConversionNode *n = new GeneConversionNode(time,child);
  _gene_conversion_node_pool.push_back(n);
  return n;
#endif
}




#if 0
std::vector< std::vector<int> > Node::_value_set(0);
std::vector<double> Node::_pos(0);


std::valarray<int> Node::_mutation_type(0);
std::vector< double > Node::_low_freq(0);
std::vector< double > Node::_high_freq(0);

std::vector<Coalescent_node*> Coalescent_node::coalescent_nodes(0);
int Coalescent_node::next_node(-1);
std::vector<Recombination_node*> Recombination_node::recombination_nodes(0);
int Recombination_node::next_node(-1);
std::vector<Genconversion_node*> Genconversion_node::geneconversion_nodes(0);
int Genconversion_node::next_node(-1);
 
void Retired_intervals::evolve(int site, double mu)
{
  if (interval().contains_point(Node::position(site))){
    connection()->evolve(site,mu);
  }
}

void Retired_intervals::evolve_snp(int site)
{
  double surface = 0.0;
  if (interval().contains_point(Node::position(site))){
    connection()->collect_sub_tree_surface(site, surface);
    surface = surface * Distribution_functions::uniform();
    set_is_mutating_on_subtree(true, site, surface);
    if (connection()->state(site)==-1) std::cout << "Retired interval error" << std::endl;
    connection()->evolve_snp(site);
  }
}

void Retired_intervals::reset_written()
{
  connection()->reset_written();
}

void Retired_intervals::write_mutation(std::string& mutation)
{
  connection()->write_mutation(mutation);
}


void Retired_intervals::set_is_mutating_on_subtree(bool forced, int site, double& surface)
{
  double tmp = 0.0;
  connection()->set_is_mutating_on_subtree(forced, site, surface, tmp);
}  

void Retired_intervals::write(std::string& _nodes, std::string& _haplotypes)
{
  std::ostringstream to_str("");
  to_str << "  <interval_node id=\"i_" << this << "\">" << std::endl;
  to_str << "    <child ref=\"i_" << connection() << "\"/>" << std::endl;
  for (int i=0; i<interval().size(); i++){
    to_str << "    <interval start=\"" << interval(i).start() << "\" end=\"" << interval(i).end() << "\"/>" << std::endl;
  }
  to_str << "  </interval_node>" << std::endl; 
  _nodes += to_str.str();
  connection()->write(_nodes,_haplotypes);
}  

void Retired_intervals::write_leaf_nodes(std::string& _nodes, std::string& _haplotypes)
{
  connection()->write_leaf_nodes(_nodes,_haplotypes);
}  

void Retired_intervals::count_leaf_snp_states(int site, int& zeros, int& ones)
{
  if (interval().contains_point(Node::position(site))){
    connection()->count_leaf_snp_states(site, zeros, ones);
  }
}  

double Genconversion_node::conversion_start()
{
  if (conversion_length()<0){
    return conversion_point()+conversion_length();
  }
  else {
    return conversion_point();
  }
}

double Genconversion_node::conversion_end()
{
  if (conversion_length()<0){
    return conversion_point();
  }
  else {
    return conversion_point()+conversion_length();
  }
}

Intervals* Coalescent_node::has_finished_intervals(int leaf_nodes_size)
{
  std::vector<Interval> ivals;
  for (int i=0; i<interval().size(); i++){
    if (interval(i).leaf_contacts()==leaf_nodes_size){
      ivals.push_back(interval(i));
    }
  }
  Intervals* iv = 0;
  if (ivals.size()!=0){
    iv = new Intervals();
    for (unsigned int i=0;i<ivals.size();i++)
      iv->add(ivals[i]);
  }
  return iv;
}  

bool Coalescent_node::retire_finished_intervals(int leaf_nodes_size)
{
  for (int i=0; i<interval().size(); i++){
    if (interval(i).leaf_contacts()==leaf_nodes_size){
      //FIXME: remove has been removed for a bit, so get it back in some form to do this!!!  interval().remove(i);
      i--;
    }
  }
  
  return (interval().size());
};

Coalescent_node::Coalescent_node(Node* left_child, Node* right_child, int size, Intervals* i_val): 
  Node(-1.0,size,0,i_val),_left_child(left_child), _right_child(right_child), _left_is_mutating(false,size), _right_is_mutating(false,size) {};

Recombination_node::Recombination_node(bool is_l, Node* child, double cross_over, double time, int size, Intervals* i_val ): 
  Node(time,size,2,i_val), _is_left(is_l), _child(child), _cross_over_point(cross_over) 
{
};

Genconversion_node::Genconversion_node(bool is_i, Node* child, double conversion_point, 
				       double conversion_length, double time, int size, Intervals* i_val) : 
  Node(time,size,1,i_val), _is_inside(is_i), _conversion_point(conversion_point), 
  _conversion_length(conversion_length), _child(child) {};


Coalescent_node* Node::operator+(Node& n){
  Intervals *dummy = new Intervals(interval().merge(n.interval()));
  return (&(Coalescent_node::get_new_coalescent_node(&n, this, size(),dummy))); 
};

void Node::genconversion(Genconversion_node*& gcon_node_1, Genconversion_node*& gcon_node_2, double Q, double time)
{
  double conversion_point = Distribution_functions::uniform();
  double conversion_length = Distribution_functions::random_sign()*Distribution_functions::expdev(Q);
  double start = 0.0;
  double end = 0.0;
  gcon_node_1 = 0;
  gcon_node_2 = 0;
  // if only one of the new nodes should be made, the new node will be identical to the old node, and nothing therefore needs to be done
  if (conversion_length<0){
    start = conversion_point+conversion_length;
    end = conversion_point;
  }
  else{
    end = conversion_point+conversion_length;
    start = conversion_point;
  }
  if (start>1.0) start = 1.0;
  if (start<0.0) start = 0.0;
  if (end>1.0) end = 1.0;
  if (end<0.0) end = 0.0;
  if (contains_active_region(start,end)){
    if (is_active_region(start, end)){ // if not; the interval from start to end is located in non-ancestral material, 
                                       //and one of the new nodes would contain only non-ancestral, and the other be identical to the starting node
      if ((is_active_region(0.0,start))||(is_active_region(end,1.0))){ // if not; the intervals from 0 to start and the interval from end to 1.0 
	                                                               //only contain non-ancestral material, 
                                                                       //and one of the new nodes would contain only non-ancestral, 
	                                                               //and the other be identical to the starting node
	// we have to make two new nodes!
	Intervals i_val = interval().copy(start, end);
	gcon_node_1 = &(Genconversion_node::get_new_geneconversion_node(true, this, start, end-start, time, size(), &i_val)); 
	Intervals i_val1 = interval().copy(active_left(), start);
	Intervals i_val2 = interval().copy(end, active_right());
	Intervals i_val3 = (i_val1.add_intervals(i_val2));
 //	i_val = (*(interval().copy(active_left(), start)) + *(interval().copy(end, active_right())));
	gcon_node_2 = &(Genconversion_node::get_new_geneconversion_node(false, this, start, end-start, time, size(), &i_val3 ));   
      }
    }
  }  
};

void Node::recombination(Recombination_node*& rcom_node_1, Recombination_node*& rcom_node_2, double time)
{
  double cross_over_point = Distribution_functions::uniform();
  rcom_node_1 = 0;
  rcom_node_2 = 0;
  // if only one of the new nodes should be made, the new node will be identical to the old node, and nothing therefore needs to be done
  if ((cross_over_point>=active_left())&&(cross_over_point<active_right())){
    Intervals i_val = interval().copy(active_left(),cross_over_point);
    rcom_node_1 = &(Recombination_node::get_new_recombination_node(true, this, cross_over_point, time, size(), &i_val));
    i_val = interval().copy(cross_over_point, active_right());
    rcom_node_2 = &(Recombination_node::get_new_recombination_node(false, this, cross_over_point, time, size(), &i_val ));
  }
};

void Node::reset_written()
{
  written = false;
}


void Node::write(std::string& _nodes, std::string& _haplotypes)
{
  if (!written){
    std::ostringstream to_str("");
    to_str << "  <leaf time=\"" << time() << "\" id=\"i_" << this << "\" haplotype=\"h_" << this << "\"/>" << std::endl;
    _nodes += to_str.str();
    _haplotypes += write_haplotype();
  }
  written = true;
}  

void Node::write_leaf_nodes(std::string& _nodes, std::string& _haplotypes)
{
  if (!written){
    std::ostringstream to_str("");
    to_str << "  <leaf time=\"" << time() << "\" id=\"i_" << this << "\" haplotype=\"h_" << this << "\"/>" << std::endl;
    _nodes += to_str.str();
    _haplotypes += write_haplotype();
  }
  written = true;
}  

void Node::count_leaf_snp_states(int site, int& zeros, int& ones)
{
  if (!written){
    if (state(site) == 0) zeros++;
    if (state(site) != 0) ones++;
  }
  written = true;
}  


std::string Node::write_haplotype()
{
  std::ostringstream to_str("");
  to_str << "    <haplotype id=\"h_" << this << "\"> " << std::endl;
  for (unsigned int i=0; i<size(); i++){
    to_str << "      <loci marker_ref=\"marker_" << Node::position(i) << "\">";
    to_str << "<allele>" << state(i) << "</allele>";
    to_str << "</loci>" << std::endl;
  }
  to_str << "    </haplotype>" << std::endl;
  return to_str.str();
}  



void Coalescent_node::reset_written()
{
  if (written) {
    written = false;
    left_child()->reset_written();
    right_child()->reset_written();
  }
}

void Coalescent_node::write_mutation(std::string& mutation)
{
  if (!written){
    std::ostringstream to_str("");
    for (unsigned int i=0; i<size(); i++){
      if (left_is_mutating(i))
	to_str << "    <mutation marker_ref=\"marker_" << Node::position(i) <<"\" parent_ref=\"i_" << this << "\" child_ref=\"i_" << left_child() << "\"/> " << std::endl; 
      if (right_is_mutating(i))
	to_str << "    <mutation marker_ref=\"marker_" << Node::position(i) <<"\" parent_ref=\"i_" << this << "\" child_ref=\"i_" << right_child() << "\"/> " << std::endl; 
    }
    mutation += to_str.str();
    left_child()->write_mutation(mutation);
    right_child()->write_mutation(mutation);
  }
  written = true;
}

void Coalescent_node::write(std::string& _nodes, std::string& _haplotypes)
{
  if (!written){
    std::ostringstream to_str("");
    to_str << "  <coalescent time=\"" << time() << "\" id=\"i_" << this << "\" haplotype=\"h_" << this << "\">" << std::endl;
    to_str << "    <child ref=\"i_" << left_child() << "\"/>" << std::endl;
    to_str << "    <child ref=\"i_" << right_child() << "\"/>" << std::endl;
    to_str << "  </coalescent>" << std::endl; 
    _nodes += to_str.str();
    _haplotypes += write_haplotype();

    left_child()->write(_nodes, _haplotypes);
    right_child()->write(_nodes, _haplotypes);
  }
  written = true;
}  

void Coalescent_node::count_leaf_snp_states(int site, int& zeros, int& ones)
{
  if (!written){
    left_child()->count_leaf_snp_states(site, zeros, ones);
    right_child()->count_leaf_snp_states(site, zeros, ones);
  }
  written = true;
}  

void Coalescent_node::write_leaf_nodes(std::string& _nodes, std::string& _haplotypes)
{
  if (!written){
    left_child()->write_leaf_nodes(_nodes, _haplotypes);
    right_child()->write_leaf_nodes(_nodes, _haplotypes);
  }
  written = true;
}  

void Recombination_node::write_mutation(std::string& mutation)
{
  if (!written){
    std::ostringstream to_str("");
    for (unsigned int i=0; i<size(); i++){
      if (is_mutating(i))
	to_str << "    <mutation marker_ref=\"marker_" << Node::position(i) <<"\" parent_ref=\"i_" << this << "\" child_ref=\"i_" << child() << "\"/> " << std::endl; 
    }
    mutation += to_str.str();
    child()->write_mutation(mutation);
  }
  written = true;
}

void Recombination_node::reset_written()
{
  if (written) {
    written = false;
    child()->reset_written();
  }
}

void Recombination_node::write(std::string& _nodes, std::string& _haplotypes)
{
  if (!written){
    std::ostringstream to_str("");
    to_str << "  <recombination time=\"" << time() << "\" crossover=\"" << cross_over() << "\" id=\"i_" << this << "\" haplotype=\"h_" << this << "\" is_left=\"" << is_left() << "\">" << std::endl;
    to_str << "    <child ref=\"i_" << child() << "\"/>" << std::endl;
    to_str << "  </recombination>" << std::endl; 
    _nodes += to_str.str();
    _haplotypes += write_haplotype();

    child()->write(_nodes, _haplotypes);
  }
  written = true;
}  

void Recombination_node::write_leaf_nodes(std::string& _nodes, std::string& _haplotypes)
{
  if (!written){
    child()->write_leaf_nodes(_nodes, _haplotypes);
  }
  written = true;
}  

void Recombination_node::count_leaf_snp_states(int site, int& zeros, int& ones)
{
  if (!written){
    child()->count_leaf_snp_states(site, zeros, ones);
  }
  written = true;
}  



void Genconversion_node::reset_written()
{
  if (written) {
    written = false;
    child()->reset_written();
  }
}

void Genconversion_node::write_mutation(std::string& mutation)
{
  if (!written){
    std::ostringstream to_str("");
    for (unsigned int i=0; i<size(); i++){
      if (is_mutating(i))
	to_str << "    <mutation marker_ref=\"marker_" << Node::position(i) <<"\" parent_ref=\"i_" << this << "\" child_ref=\"i_" << child() << "\"/> " << std::endl; 
    }
    mutation += to_str.str();
    child()->write_mutation(mutation);
  }
  written = true;
}


void Genconversion_node::write(std::string& _nodes, std::string& _haplotypes)
{
  if (!written){
    std::ostringstream to_str(" ");
    to_str << "  <genconversion time=\"" << time() << "\" conversion_start=\"" << conversion_start() << "\" conversion_end=\"" << conversion_end() << "\" id=\"i_" << this << "\" haplotype=\"h_" << this << "\"  is_inside=\"" << is_inside() <<"\">" << std::endl;
    to_str << "    <child ref=\"i_" << child() << "\"/>" << std::endl;
    to_str << "  </genconversion>" << std::endl; 
    _nodes += to_str.str();
    _haplotypes += write_haplotype();

    child()->write(_nodes, _haplotypes);
  }
  written = true;
}  

void Genconversion_node::write_leaf_nodes(std::string& _nodes, std::string& _haplotypes)
{
  if (!written){
    child()->write_leaf_nodes(_nodes, _haplotypes);
  }
  written = true;
}  

void Genconversion_node::count_leaf_snp_states(int site, int& zeros, int& ones)
{
  if (!written){
    child()->count_leaf_snp_states(site, zeros, ones);
  }
  written = true;
}  

bool Node::is_active_region(double left, double right)
  // does an interval exist which has regions in common with the interval from left to rigth
{
  bool r = false;
  for (int i=0; i<interval().size();i++){
    if (interval(i).start()>right) break;
    if ((interval(i).end()>=left)&&(interval(i).start()<right)){
      r = true;
      break;
    }
  }
  return r;
}

void Coalescent_node::set_is_mutating_on_subtree(bool m, int site, double depth, double& cur_depth)
{
  if (is_left_interval(Node::position(site))){
    if ((depth>cur_depth)&&(depth<cur_depth+(time()-left_child()->time()))){
      set_left_is_mutating(m,site);
    }
    cur_depth += time()-left_child()->time();
    left_child()->set_is_mutating_on_subtree(m, site, depth, cur_depth);
  }

  if (is_right_interval(Node::position(site))){
    if ((depth>cur_depth)&&(depth<cur_depth+(time()-right_child()->time()))){
      set_right_is_mutating(m,site);
    }
    cur_depth += time()-right_child()->time();
    right_child()->set_is_mutating_on_subtree(m, site, depth, cur_depth);
  }
};

void Recombination_node::set_is_mutating_on_subtree(bool m, int site, double depth, double& cur_depth)
{
    if ((depth>cur_depth)&&(depth<cur_depth+(time()-child()->time()))){
      set_is_mutating(m,site);
    }
    cur_depth += time()-child()->time();
    child()->set_is_mutating_on_subtree(m, site, depth, cur_depth);
};

void Genconversion_node::set_is_mutating_on_subtree(bool m, int site, double depth, double& cur_depth)
{
    if ((depth>cur_depth)&&(depth<cur_depth+(time()-child()->time()))){
      set_is_mutating(m,site);
    }
    cur_depth += time()-child()->time();
    child()->set_is_mutating_on_subtree(m, site, depth, cur_depth);
};


bool Coalescent_node::is_left_interval(double pos)
{
  return left_interval().contains_point(pos);
}  


bool Coalescent_node::is_right_interval(double pos)
{
  return right_interval().contains_point(pos);
}  



void Coalescent_node::collect_sub_tree_surface(int site, double& surface)
{
  if (is_left_interval(Node::position(site))){
    set_left_is_mutating(false,site);
    surface += time()-left_child()->time();
    left_child()->collect_sub_tree_surface(site,surface);
  }

  if (is_right_interval(Node::position(site))){
    set_right_is_mutating(false,site);
    surface += time()-right_child()->time();
    right_child()->collect_sub_tree_surface(site,surface);
  }
};


void Recombination_node::collect_sub_tree_surface(int site, double& surface)
{
  set_is_mutating(false,site);
  surface += time()-child()->time();
  child()->collect_sub_tree_surface(site, surface);
};

void Genconversion_node::collect_sub_tree_surface(int site, double& surface)
{
  set_is_mutating(false,site);
  surface += time()-child()->time();
  child()->collect_sub_tree_surface(site, surface);
};

int Node::mutate(const double mu, const double time, int site)
{
  int k = state(site);
  if (Distribution_functions::uniform()<Distribution_functions::expdist(mu,time)){
    k = value_set(site)[Distribution_functions::irand(value_set(site).size()-1)];
    if (k == state(site)) k = value_set(site)[value_set(site).size()-1];
    set_is_mutating(true, site);
  }
  return k;
}; 

int Node::mutate_snp(int site, bool forced)
{
  int k = state(site);
  if (forced){
    int i = Distribution_functions::irand(value_set(site).size()-1);
    k = value_set(site)[i];
    if (k == state(site)) k = value_set(site)[value_set(site).size()-1];
  }
  return k;
}; 

void Coalescent_node::evolve_snp(int site)
{
  (left_child()->state(site)) = mutate_snp(site,left_is_mutating(site));
  if (((left_child()->state(site))==-1) or (state(site)==-1)) std::cout << "left error" << std::endl;
  (right_child()->state(site)) = mutate_snp(site,right_is_mutating(site));
  if (((right_child()->state(site))==-1) or (state(site)==-1)) std::cout << "right error" << std::endl;
  left_child()->evolve_snp(site);
  right_child()->evolve_snp(site);
};


void Recombination_node::evolve_snp(int site)
{
  bool parse = false;
  if (is_left()){
    if (cross_over()>=Node::position(site)) parse = true; 
  }
  else{
    if (cross_over()<Node::position(site)) parse = true;
  }
  if (parse) {
    (child()->state(site)) = mutate_snp(site,is_mutating(site));
    child()->evolve_snp(site);
  }      
};


void Genconversion_node::evolve_snp(int site)
{
  double lp;
  double rp;
  bool parse = false;
  if (conversion_length()<0) {
    lp = conversion_point()+conversion_length();
    rp = conversion_point();
  }
  else{
    rp = conversion_point()+conversion_length();
    lp = conversion_point();
  }

  if (is_inside()){
    if ((lp<=Node::position(site))&&(Node::position(site)<rp)) parse = true;
  }
  else{
    if ((Node::position(site)<lp)||(Node::position(site)>=rp)) parse = true;
  }
  if (parse) {
    (child()->state(site)) = mutate_snp(site,is_mutating(site));
    child()->evolve_snp(site);
  }      
};


int Coalescent_node::left_mutate(double mu, int site)
{
  int old_state = state(site);
  int new_state = mutate(mu, time()-left_child()->time(),site);
  if (old_state!=new_state) set_left_is_mutating(true, site);
  return new_state;
}

int Coalescent_node::right_mutate(double mu, int site)
{
  int old_state = state(site);
  int new_state = mutate(mu, time()-right_child()->time(),site);
  if (old_state!=new_state) set_right_is_mutating(true, site);
  return new_state;
}

void Coalescent_node::evolve(int site, double mu)
{
  (left_child()->state(site)) = left_mutate(mu, site);
  if (((left_child()->state(site))==-1) and (state(site)!=-1)) std::cout << "left error" << std::endl;
  (right_child()->state(site)) = right_mutate(mu, site);
  if (((right_child()->state(site))==-1) and (state(site)!=-1)) std::cout << "right error" << std::endl;
  left_child()->evolve(site,mu);
  right_child()->evolve(site,mu);
};


void Recombination_node::evolve(int site, double mu)
{
  bool parse = false;
  if (is_left()){
    if (cross_over()>=Node::position(site)) parse = true; 
  }
  else{
    if (cross_over()<Node::position(site)) parse = true;
  }
  if (parse) {
    (child()->state(site)) = mutate(mu, time()-child()->time(),site);
    child()->evolve(site,mu);
  }      
};


void Genconversion_node::evolve(int site, double mu)
{
  double lp;
  double rp;
  bool parse = false;
  if (conversion_length()<0) {
    lp = conversion_point()+conversion_length();
    rp = conversion_point();
  }
  else{
    rp = conversion_point()+conversion_length();
    lp = conversion_point();
  }

  if (is_inside()){
    if ((lp<=Node::position(site))&&(Node::position(site)<rp)) parse = true;
  }
  else{
    if ((Node::position(site)<lp)||(Node::position(site)>=rp)) parse = true;
  }
  if (parse) {
    (child()->state(site)) = mutate(mu, time()-child()->time(),site);
    child()->evolve(site,mu);
  }      
};

#endif
