
#include "descender.hh"

#ifndef NODE_HH_INCLUDED
# include "node.hh"
#endif
#ifndef MARKER_HH_INCLUDED
# include "marker.hh"
#endif
#ifndef MONITOR_HH_INCLUDED
# include "monitor.hh"
#endif


void Descender::evolve(ARG &arg) const
{
  std::vector<RetiredInterval>::const_iterator ri_itr, ri_begin, ri_end;
  ri_begin = arg.retired_intervals().begin();
  ri_end   = arg.retired_intervals().end();

  SimulationMonitor *mon = i_conf.monitor();

  // FIXME: we could do this merging of retired intervals and value
  // sets faster if we sorted the retired intervals, but if the number
  // of markers is fairly small it won't matter much.
  for (ri_itr = ri_begin; ri_itr != ri_end; ++ri_itr)
    {
      for (size_t m = 0; m < i_conf.no_markers(); ++m)
	{
	  if (ri_itr->contains_point(i_conf.position(m)))
	    {
	      if (mon) mon->mutator_update(m);
	    retry: // handle retries when wrong freqs
	      try { ri_itr->mutate(i_conf,m); } 
	      catch (Mutator::retry_mutation&) {
		if (mon) mon->retry_mutation();
		goto retry; 
	      }
	    }
	}
    }
}
