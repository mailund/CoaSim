#include <iostream>
#include <sstream>
#include <fstream>
#include "node.hh"
#include "builder.hh"
#include "descender.hh"
#include <vector>
#include "reporter.hh"




/*********************************************************************************************/  
/**                                                                                         **/   
/**  The program is started by the following commandline:                                   **/  
/**  <program name> -o <out xml-file> -gl <geneconversion length> -gr <geneconversion rate> **/
/**  -rr <recombination rate> -mr <mutation rate> -ln <# of leafs>                          **/  
/**  -v <values (indicating values the different markers can take. The format is:           **/
/**  {0 1 2 ....})>                                                                         **/
/**  -rand <rand> should a new random seed be taken 0: do not reset seed, 1                 **/  
/**  -is_snp <is a snp, indicating rather the marker is a snp or microsattelite marker.     **/
/**           The format is:                                                                **/
/**  "0 1 0 ...." where 0 (microsatelite) and 1 (snp)>                                      **/
/**  -pos <positions, The format is:"0.1 0.2 ...." >                                        **/
/**  -low_freq <lower limit of accepted allele freq., The format is:"0.1 0.2 ...." >        **/
/**  -high_freq <high limit of accepted allele freq., The format is:"0.8 0.9 ...." >        **/
/**  -dtd <dtd> the dtd-file (and path)                                                     **/  
/**  -log <log> is a log file                                                               **/  
/**                                                                                         **/  
/**  An exsample:                                                                           **/
/**  coasim -o simulation_output.xml -gl 250 -gr 250 -rr 50.0 -mr 0.005 -ln 1000            **/
/**  -v "{ 0 1 2 3 4 5 6 7 8 9 } { 0 1 2 3 4 5 6 7 8 9 } { 0 1 2 3 4 5 6 7 8 9 }            **/
/**  { 0 1 2 3 4 5 6 7 8 9 } { 0 1 2 3 4 5 6 7 8 9 } { 0 1 } { 0 1 2 3 4 5 6 7 8 9 }        **/
/**  { 0 1 } { 0 1 2 3 4 5 6 7 8 9 } { 0 1 2 3 4 5 6 7 8 9 } { 0 1 2 3 4 5 6 7 8 9 }        **/
/**  { 0 1 2 3 4 5 6 7 8 9 }" -rand 1 -is_snp "0 0 0 0 0 1 0 1 0 0 0 0"                     **/
/**  -pos "0.01 0.11 0.22 0.33 0.44 0.5 0.55 0.6 0.66 0.77 0.88 0.99"                       **/
/**  -low_freq "0.0 0.0 0.0 0.0 0.0 0.08 0.0 0.04 0.0 0.0 0.0 0.0"                          **/
/**  -high_freq "1.0 1.0 1.0 1.0 1.0 0.12 1.0 0.06 1.0 1.0 1.0 1.0"                         **/
/**  -dtd coasim.dtd -log coasim.log -mode leaf_nodes_only                                  **/
/**                                                                                         **/
/**                                                                                         **/  
/*********************************************************************************************/  

int main(int argc, char* argv[])
{
  std::string marker_type_string = "";
  std::string low_freq_string = "";
  std::string high_freq_string = "";

  std::string outfile = "coasim.xml";
  std::string pos_string = "";
  std::string alp_string = "";
  std::string dtd = "";
  std::string log = "";
  std::string output_mode = "";
  int leaf_nodes = 0;
  double Q =0.0;
  double G =0.0;
  double rho =0.0;
  double mu = 0.0;
  double growth = 0.0;
  int seed = 0;
  std::string str(""); 

  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-o"){
       outfile = argv[i+1];
       break;

    }    
  }

  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-gl"){
       Q = atof(argv[i+1]);
       break;
    }    
  }


  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-gr"){
       G = atof(argv[i+1]);
       break;
    }    
  }

  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-rr"){
       rho = atof(argv[i+1]);
       break;
    }    
  }

  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-exp"){
       growth = atof(argv[i+1]);
       break;
    }    
  }

  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-mr"){
       mu = atof(argv[i+1]);
       break;
    }    
  }
 
  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-ln"){
       leaf_nodes = atoi(argv[i+1]);
       break;
    }    
  }

  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-v"){
       alp_string = argv[i+1];
       break;
    }    
  }

  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-rand"){
       seed = atoi(argv[i+1]);
       break;
    }    
  }

  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-marker_type"){
       marker_type_string = argv[i+1];
       break;
    }    
  }


  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-pos"){
       pos_string = argv[i+1];
       break;
    }    
  }

  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-low_freq"){
       low_freq_string = argv[i+1];
       break;
    }    
  }

  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-high_freq"){
       high_freq_string = argv[i+1];
       break;
    }    
  }


  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-dtd"){
       dtd = argv[i+1];
       break;
    }    
  }

  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-log"){
       log = argv[i+1];
       break;
    }    
  }

  for (int i=0; i<argc; i++){
    str = argv[i];
    if (str == "-mode"){
       output_mode = argv[i+1];
       break;
    }    
  }

  int tmp_time = int(time(0));
  if (seed!=0) std::srand(tmp_time); //setting random seed
  //  std::srand(1075214223);
  //  std::cout<< "seed = " << tmp_time << std::endl;
  // setting positions
  std::istringstream in_pos_stream(pos_string);
  in_pos_stream.precision(20);  
  double pos = 0;
  Intervals* i_val;
  while (in_pos_stream >> pos) Node::add_position(pos);
  std::vector<Node*> _leaf_nodes;
  for (int i=0; i<leaf_nodes; i++){
    i_val = new Intervals();
    for (int j=0; j<Node::position_size(); j++) i_val -> add(Node::position(j)-0.00001, 0.00002,1);
    Node* np = new Node(0.0, Node::position_size(), -1, i_val);
    _leaf_nodes.push_back(np);
  }
  
  // setting the alphabet


  Node::init_value_set();
  std::istringstream in_alp_stream(alp_string);
  in_alp_stream.precision(20);  
  std::string element_str;
  int mark = 0;
  int value = 0;
  while (in_alp_stream >> element_str){
    if ((element_str != "{")&&(element_str != "}")){
      std::istringstream element_stream(element_str);
      element_stream >> value;
      Node::add_to_set_value(mark,value);
    }
    
    if (element_str == "}") {
      mark++;
    }
  }

  // setting marker_type 0 is microsattelite, 1 is snp, and 2 is a disease marker
  Node::init_mutation_type();
  std::istringstream in_marker_type_stream(marker_type_string);
  in_marker_type_stream.precision(20);  
  int marker_type = 0;
  mark = 0;
  while (in_marker_type_stream >> marker_type) {
    Node::set_mutation_type(mark, marker_type);
    mark++;
  }


  //setting allele freq

  Node::init_freq();
  std::istringstream in_low_freq_stream(low_freq_string);
  in_low_freq_stream.precision(20);  
  double freq = 0.0;
  mark = 0;
  while (in_low_freq_stream >> freq) {
    Node::set_low_freq(mark, freq);
    mark++;
  }

  std::istringstream in_high_freq_stream(high_freq_string);
  in_high_freq_stream.precision(20);  
  freq = 0.0;
  mark = 0;
  while (in_high_freq_stream >> freq) {
    Node::set_high_freq(mark, freq);
    mark++;
  }

  // building graph
  Builder build(rho, G, Q, growth, log);
  std::vector<Retired_intervals*> finished_intervals(0);


  // descending though the graph
  Reporter::append(log,"Start Descending to place mutations and parse states");
  Descender descend(mu,log);  
  int rounds = 0;
  bool mutations_placed = false;
  while (!mutations_placed) {
    //  std::cout << rounds << std::endl;
    rounds++;
    build.cleanup(finished_intervals);
    for (unsigned int i = 0; i< _leaf_nodes.size(); i++) _leaf_nodes[i] -> reset_node(0.0, true);
    build.build( _leaf_nodes, finished_intervals );
    Reporter::append(log,"Finished Building Tree");
    // setting root alleles
    for (unsigned int i=0; i<finished_intervals.size(); i++) {
      for (int j=0; j<Node::position_size(); j++){ 
	//	std::cout << i << "," << j << std::endl;
	if ((finished_intervals[i]->interval()).contains_point(Node::position(j))){ 
	  ((finished_intervals[i]->connection())->state(j))=Node::value_set_value(j,0);
	}
      }
    }
    mutations_placed = descend.evolve(finished_intervals);
  }
 

  Reporter::append(log,"Generating and writing output");
  Reporter::append(log,"   Writing haplotypes and tree-structure");
  
  std::string nodes;
  std::string haplotypes;
  std::string mutation;
  std::ofstream out(outfile.c_str());
  for (unsigned int i=0; i<finished_intervals.size(); i++) finished_intervals[i]->reset_written();
  if (output_mode!="leaf_nodes_only"){
    for (unsigned int i=0; i<finished_intervals.size(); i++) finished_intervals[i]->write(nodes,haplotypes);
  }
  else {
    for (unsigned int i=0; i<finished_intervals.size(); i++) finished_intervals[i]->write_leaf_nodes(nodes,haplotypes);
  }
  
  Reporter::append(log,"   Writing haplotypes and tree-structure");
  for (unsigned int i=0; i<finished_intervals.size(); i++) finished_intervals[i]->reset_written();
  Reporter::append(log,"   Writing mutations");
  if (output_mode!="leaf_nodes_only"){
    for (unsigned int i=0; i<finished_intervals.size(); i++) finished_intervals[i]->write_mutation(mutation);  
  }
  Reporter::append(log,"   Composing XML-output file");
  
  out << "<?xml version=\"1.0\"?>" << std::endl;
  out <<"<!DOCTYPE coasim SYSTEM \"" << dtd << "\"> " << std::endl;
  //  out << "<?xml-stylesheet type=\"text/xsl\" href=\"coasim_to_dot.xsl\"?>" << std::endl;
  out << "<coasim output_mode=\"" << output_mode << "\" leaf_nodes=\"" << leaf_nodes << "\" positions=\""<<pos_string<<"\" value_set=\""<<alp_string<<"\" Q=\""<<Q<<"\" G=\""<<G<<"\" rho=\""<<rho<<"\" mu=\""<<mu<<"\">" << std::endl;
  out << "  <markers>" << std::endl;
  for (int i=0; i<Node::position_size(); i++){
    out << "    <marker id=\"marker_" << Node::position(i) << "\">" <<std::endl;
    out << "      <position>" << Node::position(i) << "</position> " << std::endl;
    out << "      <value-set>";
    for (unsigned int j=0;j<Node::value_set(i).size();j++)
      out << "<value>" << Node::value_set(i)[j] << "</value>";
    out << "</value-set> " << std::endl;
    out << "    </marker>" << std::endl;
  }
  out << "  </markers>" << std::endl;
  out << "  <haplotypes>" << std::endl << haplotypes << "  </haplotypes>" << std::endl;
  out << nodes;
  out << "  <mutations>" << std::endl;
  out << mutation;
  out << "  </mutations>" << std::endl;
  out << "</coasim>" << std::endl; 
  out.close();

  return 0;
}
