
#include "descender.hh"

#ifndef DIST_FUNCTIONS_HH_INCLUDED
# include "dist_funcs.hh"
#endif
#ifndef NODE_HH_INCLUDED
# include "node.hh"
#endif

#if 0
namespace {
  using std::unary_function;

  // Function mutating from a retired interval according to the value
  // sets corresponding to the points in the interval
  struct mutator : public unary_function<ARG::RetiredInterval,void> {
    mutator(struct Configuration &conf) : _conf(conf) {}
    void operator () (const ARG::RetiredInterval &ri) const {
      // FIXME: we could do this merging of retired intervals and
      // value sets faster if we sorted the retired intervals, but if
      // the number of value sets is fairly small it won't matter
      // much.
      for (size_t m = 0; m < _conf.no_markers(); ++m)
	if (ri.contains_point(_conf.position(m)))
	  {
	  retry: // we jump to a retry if a SNP mutation doesn't fit
		 // the desired frequencies
	    try {
	      _conf.value_set(m).mutate(ri.top_node(),m);
	    } catch (Configuration::ValueSet::wrong_snp_frequency&) {
	      goto retry;
	    }
	  }
    }
  private:
    const Configuration &_conf;
  };
}
#endif

// FIXME: we can speed up the mutation-stuff by first mutating the
// traits and then the rest; that way we won't waste time with
// mutating the other markers if we are going to delete the tree
// anyway.

bool Descender::evolve(ARG &arg) const
{
  return false;
#if 0
  try {
    std::for_each(arg.retired_intervals().begin(),
		  arg.retired_intervals().end(),
		  mutator(_conf));
  } catch (?) {
  }
#endif

  return true;
}

