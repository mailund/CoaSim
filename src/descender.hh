#ifndef DESCENDER_HH
#define DESCENDER_HH
#include "dist_funcs.hh"

class Descender 
{
public:
  Descender(double mu, std::string log): _mu(mu),_log(log) {};
  ~Descender(){};

  bool evolve(std::vector<Retired_intervals*>& finished_intervals)
  { 
    //           std::cout << "finished intervals size " << finished_intervals.size() << std::endl;
    //  for (unsigned int i=0; i<finished_intervals.size(); i++){
    //  std::cout << i << ", size " << (finished_intervals[i]->interval()).size() << std::endl;
    //  for (int j=0; j<(finished_intervals[i]->interval()).size(); j++)
    // std::cout << i << " : " << (finished_intervals[i]->interval(j)).start() << " -> " << (finished_intervals[i]->interval(j)).end() << std::endl;
    // }

    int zeros = 0;
    int ones = 0;
    bool ok = true;
    for (int j=0; j<Node::position_size(); j++){
      if (Node::is_snp(j)){
	//	std::cout << "doing SNP position " << std::endl;
	ok = false;
	Reporter::append(_log,"doing SNP position ");
	for (unsigned int i=0; i<finished_intervals.size(); i++){
	  if (finished_intervals[i]->interval().contains_point(Node::position(j))){
	    while (!ok){
	      zeros = 0;
	      ones = 0;
	      finished_intervals[i]->evolve_snp(j);
	      finished_intervals[i]->reset_written();
	      //	      std::cout << j << std::endl;
	      finished_intervals[i]->count_leaf_snp_states(j,zeros,ones);
	      //	      std::cout << j << "0s = " << zeros << ", 1s = " << ones << ", low = " << Node::low_freq(j) << ", high = " << Node::high_freq(j) << std::endl;
	      // std::cout << double(zeros)/double(zeros+ones) << " " << double(ones)/double(zeros+ones) << std::endl;
	      //  ok = ((double(zeros)/double(zeros+ones)>Node::low_freq(j))&&(double(zeros)/double(zeros+ones)<Node::high_freq(j))&&(double(ones)/double(zeros+ones)>Node::low_freq(j))&&(double(ones)/double(zeros+ones)<Node::high_freq(j)));
	      ok = ((double(ones)/double(zeros+ones)>Node::low_freq(j))&&(double(ones)/double(zeros+ones)<Node::high_freq(j)));
	    }
	  }
	}
      }
      else if (Node::is_microsattelite(j)){
	//	std::cout << "doing microsattelite position " << std::endl;
	Reporter::append(_log,"doing microsatelite position ");
	for (unsigned int i=0; i<finished_intervals.size(); i++){
	  finished_intervals[i]->evolve(j,_mu);
	}
      }
      else if (Node::is_disease_marker(j)){
	//	std::cout << "doing disease marker position " << std::endl;
	Reporter::append(_log,"doing disease marker position ");
	for (unsigned int i=0; i<finished_intervals.size(); i++){
	  if (finished_intervals[i]->interval().contains_point(Node::position(j))){
	    zeros = 0;
	    ones = 0;
	    finished_intervals[i]->evolve_snp(j);
	    finished_intervals[i]->reset_written();
	    finished_intervals[i]->count_leaf_snp_states(j,zeros,ones);
	    //	    std::cout << j << "0s = " << zeros << ", 1s = " << ones << ", low = " << Node::low_freq(j) << ", high = " << Node::high_freq(j) << std::endl;
	    // std::cout << double(zeros)/double(zeros+ones) << " " << double(ones)/double(zeros+ones) << std::endl;
	    //	      ok = ((double(zeros)/double(zeros+ones)>Node::low_freq(j))&&(double(zeros)/double(zeros+ones)<Node::high_freq(j))&&(double(ones)/double(zeros+ones)>Node::low_freq(j))&&(double(ones)/double(zeros+ones)<Node::high_freq(j)));
	    ok = ((double(ones)/double(zeros+ones)>Node::low_freq(j))&&(double(ones)/double(zeros+ones)<Node::high_freq(j)));
	    //	    std::cout << "disease allele freq: " << double(ones)/double(zeros+ones) << std::endl;
	    if (!ok) return false;
	  }
	}
      }
    }
    return ok;
  }

private:
  double _mu;
  std::string _log;
};
#endif
