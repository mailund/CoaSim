
#include "descender.hh"

#ifndef NODE_HH_INCLUDED
# include "node.hh"
#endif
#ifndef MARKER_HH_INCLUDED
# include "marker.hh"
#endif


void Descender::evolve(ARG &arg) const
{
  std::vector<RetiredInterval>::const_iterator ri_itr, ri_begin, ri_end;
  ri_begin = arg.retired_intervals().begin();
  ri_end   = arg.retired_intervals().end();

  // FIXME: we could do this merging of retired intervals and value
  // sets faster if we sorted the retired intervals, but if the number
  // of markers is fairly small it won't matter much.
  for (ri_itr = ri_begin; ri_itr != ri_end; ++ri_itr)
    {
      for (size_t m = 0; m < _conf.no_markers(); ++m)
	{
	  if (ri_itr->contains_point(_conf.position(m)))
	    {
#if 0
	      std::cout << "mutating marker " << m << '\n'
			<< "position " << _conf.position(m) << ' '
			<< "in interval " << *ri_itr << std::endl;
#endif
	    retry: // handle retries when wrong freqs
	      try { ri_itr->mutate(_conf,m); } 
	      catch (Mutator::retry_mutation&) {
#if 0
		std::cout << "\t!!! retrying mutation " << m << std::endl;
#endif
		goto retry; 
	      }
	    }
	}
    }
}
