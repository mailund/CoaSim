
#ifndef RETIRED_INTERVAL_HH_INCLUDED
#define RETIRED_INTERVAL_HH_INCLUDED

#ifndef INTERVAL_HH_INCLUDED
# include "interval.hh"
#endif

class Node;
class Configuration;

// -- Intervals that are retired because they connect to all leaves -----
class RetiredInterval : public Interval
{
public:
  struct null_top_node : public std::logic_error {
    null_top_node() : std::logic_error("null top node") {}
  };
  
  RetiredInterval(const Interval &interval, Node *const top_node)
    throw(null_top_node)
    : Interval(interval), i_top_node(top_node)
  { if (top_node == 0) throw null_top_node(); }
  
  Node *top_node() const { return i_top_node; }
  double surface() const;

  void mutate(const Configuration &conf, unsigned int marker_index) const;
  
  void to_xml(std::ostream &os) const;
  
private:
  Node *i_top_node;
};

inline std::ostream & operator << (std::ostream &os, const RetiredInterval &i)
{ i.print(os); return os; }


#endif // RETIRED_INTERVAL_HH_INCLUDED
