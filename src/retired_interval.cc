
#include "retired_interval.hh"
#ifndef NODE_HH_INCLUDED
# include "node.hh"
#endif

double RetiredInterval::surface() const
{ 
  return _top_node->surface_at_point(start()); 
}

void RetiredInterval::to_xml(std::ostream &os) const
{
  os << "  <interval_node id=\"i_" << this << "\">" << std::endl
     << "    <child ref=\"i_" << top_node() << "\"/>" << std::endl
     << "    <interval start=\"" << start() << "\" end=\"" << end() << "\"/>\n"
     << "  </interval_node>" << std::endl;
}
