
#ifndef RETIRED_INTERVAL_HH_INCLUDED
#define RETIRED_INTERVAL_HH_INCLUDED

#ifndef INTERVAL_HH_INCLUDED
# include "interval.hh"
#endif

class Node;

// -- Intervals that are retired because they connect to all leaves -----
class RetiredInterval : private Interval
{
public:
  struct null_top_node : public std::logic_error {
    null_top_node() : std::logic_error("null top node") {}
  };
  
  // exposing some interval-properties
  using Interval::is_start;
  using Interval::is_end;
  using Interval::contains_point;
  using Interval::leaf_contacts;
  
  RetiredInterval(const Interval &interval, Node *const top_node)
    throw(null_top_node)
    : Interval(interval), _top_node(top_node)
  { if (top_node == 0) throw null_top_node(); }
  
  Node *top_node() const { return _top_node; }
  double surface() const;
  
  
  void to_xml(std::ostream &os) const;
  
private:
  Node *_top_node;
};

#endif // RETIRED_INTERVAL_HH_INCLUDED
