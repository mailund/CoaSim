/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#ifndef CORE__CONFIGURATION_HH_INCLUDED
#define CORE__CONFIGURATION_HH_INCLUDED

#ifndef CORE__MARKER_HH_INCLUDED
# include "marker.hh"
#endif

#ifndef STDEXCEPT_INCLUDED
# include <stdexcept>
# define STDEXCEPT_INCLUDED
#endif
#ifndef VECTOR_INCLUDED
# include <vector>
# define VECTOR_INCLUDED
#endif
#ifndef CASSERT_INCLUDED
# include <cassert>
# define VECTOR_INCLUDED
#endif

namespace core {
    class SimulationMonitor;


    // FIXME: The interface for epochs needs to be rethought before I
    // add population structure!  Passing the coalescence event
    // handler isn't the right way to do this -- the sub-populations
    // should be passed instead, and from those I should get the right
    // event handlers.
    class Scheduler;
    class CoalescenceEvent;
    struct Epoch {
	virtual ~Epoch();

	// polymorphic copying
	virtual Epoch *copy() const = 0;

	virtual void add_events(Scheduler &scheduler,
				CoalescenceEvent &coa_event) = 0;
    };

    class Configuration
    {
    public:

	// Exception thrown if the configuration is initialized with
	// un-sorted positions
	struct out_of_sequence : public std::logic_error {
	    out_of_sequence() : std::logic_error("Marker positions not sorted."){}
	};

	// Exception thrown if we try to add to a value set before the type
	// of the marker has been initialized
	struct uninitialized_marker : public std::logic_error {
	    uninitialized_marker() : std::logic_error("uninitialized marker."){}
	};

	// Initialize the configuration with the markers given by the
	// sequence from begin to end -- an exception is thrown if the
	// positions are not sorted in increasing order; the build
	// parameters rho, Q, gamma, and growth.
	template <typename InItr>
	Configuration(unsigned int no_leaves,
		      InItr begin, InItr end,
		      double rho, double Q, double gamma, double growth)
	    throw(out_of_sequence);
	template <typename MarkerItr, typename EpochItr>
	Configuration(unsigned int no_leaves,
		      MarkerItr m_begin, MarkerItr m_end,
		      EpochItr  e_begin, EpochItr  e_end,
		      double rho, double Q, double gamma, double growth)
	    throw(out_of_sequence);
	~Configuration();

	int no_leaves() const { return i_no_leaves; }

	// Number of markers for the configuration
	int no_markers() const { return i_no_markers; }
	// The positions of the markers
	double position(int index) const throw(std::out_of_range);

	// Accessors to markers
	const Marker &first_marker(int index) const throw(uninitialized_marker,
							  std::out_of_range);
	const Marker &plain_marker(int index) const throw(uninitialized_marker,
							  std::out_of_range);
	const Marker &marker(int index)       const throw(uninitialized_marker,
							  std::out_of_range);

	bool is_first_marker(int index) const;
	bool is_plain_marker(int index) const;


	// Insert a marker at the position index -- this method only borrows
	// the reference, so don't free or change the marker after setting
	// it and before deleting the configuration -- and remember to free
	// it yourself after use of the configuration.  The run_first flag
	// is used to prioritize the order of marker-mutations; all markers
	// set with run_first are mutated before all markers without
	// run_first.
	void set_marker(int pos_index, const Marker *marker)
	    throw(std::out_of_range);

	// Parameters for building the ARG and assigning mutations
	double rho()    const { return i_rho; }
	double Q()      const { return i_Q; }
	double gamma()  const { return i_gamma; }
	double growth() const { return i_growth; }

	typedef std::vector<Epoch*>::const_iterator epoch_iterator;
	epoch_iterator epochs_begin() const { return i_epochs.begin(); }
	epoch_iterator epochs_end()   const { return i_epochs.end(); }

    private:
	// Disable these
	Configuration(const Configuration&);
	Configuration& operator = (const Configuration&);

	int i_no_markers;
	int i_no_leaves;

	const Marker** i_first_markers;
	const Marker** i_plain_markers;

	double i_rho;
	double i_Q;
	double i_gamma;
	double i_growth;

	std::vector<Epoch*> i_epochs;
    };


    template <typename InItr>
    Configuration::Configuration(unsigned int no_leaves,
				 InItr begin, InItr end,
				 double rho, 
				 double Q, double gamma,
				 double growth)
	throw(out_of_sequence)
	: i_no_leaves(no_leaves),
	  i_rho(rho), i_Q(Q), i_gamma(gamma), i_growth(growth)
    {
	i_no_markers = end - begin;
    
	i_first_markers = new const Marker* [i_no_markers];
	for (int m = 0; m < i_no_markers; ++m) i_first_markers[m] = 0;

	i_plain_markers = new const Marker* [i_no_markers];
	for (int m = 0; m < i_no_markers; ++m) i_plain_markers[m] = 0;

	for (int m = 0; m < i_no_markers; ++m) 
	    set_marker(m, *(begin++));

	for (int m = 1; m < i_no_markers; ++m)
	    if (position(m-1) >= position(m)) throw out_of_sequence();

    }

    template <typename MarkerItr, typename EpochItr>
    Configuration::Configuration(unsigned int no_leaves,
				 MarkerItr m_begin, MarkerItr m_end,
				 EpochItr  e_begin, EpochItr  e_end,
				 double rho, double Q, double gamma, 
				 double growth)
	throw(out_of_sequence)
	: i_no_leaves(no_leaves),
	  i_rho(rho), i_Q(Q), i_gamma(gamma), i_growth(growth)
    {
	i_no_markers = m_end - m_begin;
    
	i_first_markers = new const Marker* [i_no_markers];
	for (int m = 0; m < i_no_markers; ++m) i_first_markers[m] = 0;

	i_plain_markers = new const Marker* [i_no_markers];
	for (int m = 0; m < i_no_markers; ++m) i_plain_markers[m] = 0;

	for (int m = 0; m < i_no_markers; ++m) 
	    set_marker(m, *(m_begin++));

	for (int m = 1; m < i_no_markers; ++m)
	    if (position(m-1) >= position(m)) throw out_of_sequence();
	
	for ( ; e_begin != e_end; ++e_begin)
	    i_epochs.push_back((*e_begin)->copy());
    }


    inline double Configuration::position(int index)
	const throw(std::out_of_range)
    {
	return marker(index).position();
    }

    inline const Marker &Configuration::first_marker(int index)
	const throw(uninitialized_marker,std::out_of_range)
    {
	if (index >= no_markers()) throw std::out_of_range("No marker at index");
	if (!i_first_markers[index]) throw uninitialized_marker();
	else return *i_first_markers[index];
    }

    inline const Marker &Configuration::plain_marker(int index)
	const throw(uninitialized_marker,std::out_of_range)
    {
	if (index >= no_markers()) throw std::out_of_range("No marker at index");
	if (!i_plain_markers[index]) throw uninitialized_marker();
	else return *i_plain_markers[index];
    }

    inline bool Configuration::is_first_marker(int index) const
    { return i_first_markers[index] != 0; }
    inline bool Configuration::is_plain_marker(int index) const
    { return i_plain_markers[index] != 0; }

    inline const Marker &Configuration::marker(int index) 
	const throw(uninitialized_marker, std::out_of_range)
    {
	if (is_first_marker(index)) return first_marker(index);
	else                        return plain_marker(index);
    }


    inline void Configuration::set_marker(int index, const Marker *marker)
	throw(std::out_of_range)
    {
	assert(marker != 0);

	if (index >= no_markers()) throw std::out_of_range("No marker at index");
  
	if (marker->run_first()) 
	    {
		i_first_markers[index] = marker->copy();
		i_plain_markers[index] = 0;
	    }
	else
	    {
		i_first_markers[index] = 0;
		i_plain_markers[index] = marker->copy();
	    }
    }

}

#endif
