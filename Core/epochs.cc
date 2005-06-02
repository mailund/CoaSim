/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#include "epochs.hh"

#ifndef CORE__BUILDER_EVENTS_HH_INCLUDED
# include "builder_events.hh"
#endif
#ifndef CORE__DIST_FUNCTIONS_HH_INCLUDED
# include "dist_funcs.hh"
#endif


#ifndef LIMITS_INCLUDED
# include <limits>
# define LIMITS_INCLUDED
#endif
#ifndef CMATH_INCLUDED
# include <cmath>
# define CMATH_INCLUDED
#endif

using namespace core;

CoalescenceEpoch::~CoalescenceEpoch()
{
    delete i_underlying;
}

double
CoalescenceEpoch::nested_event_time(State &s, double current_time)
{
    // use start time until we have executed the first event that
    // pushes this to the coalescence event stack.
    if (!i_underlying) return start_time();
    return CoalescenceEvent::event_time(s, current_time);
}

void
CoalescenceEpoch::nested_update_state(Scheduler &scheduler, State &s,
				      double event_time)
{
    CoalescenceEvent::update_state(scheduler, s, event_time);
}

void
CoalescenceEpoch::update_state(Scheduler &scheduler, State &s, 
			       double event_time)
{
    if (start_time() <= event_time and !i_underlying)
	// we are entering the epoch -- push this epoch on top of the stack
	{
	    Population &p = s.populations().at(population());
	    i_underlying = p.coalescence_event();
	    p.coalescence_event(this);
	    scheduler.remove_event(i_underlying);
	}
    else if (end_time() > 0 and end_time() <= event_time)
	// we are leaving, now pop the epoch
	{
	    Population &p = s.populations().at(population());
	    p.coalescence_event(i_underlying);
	    scheduler.remove_event(static_cast<Epoch*>(this));
	    scheduler.add_event(i_underlying);
	    i_underlying = 0;
	    delete this;
	}
    else
	// inside the epoch we just propagate
	nested_update_state(scheduler, s, event_time);
}


double
BottleNeck::waiting_time(State &s, double c_time)
{
    return i_scale_fraction * basic_waiting_time(s, c_time);
}


Event *
BottleNeck::copy() const
{
    return static_cast<Epoch*>(new BottleNeck(*this));
}



double
Growth::waiting_time(State &s, double current_time)
{
    using namespace Distribution_functions;
    double t_k_star = basic_waiting_time(s, current_time);
    double v_k_1 = current_time - start_time();
    return log(1.0+i_beta*t_k_star*std::exp(-i_beta*v_k_1))/i_beta;
}

Event *
Growth::copy() const
{
    return static_cast<Epoch*>(new Growth(*this));
}





Migration::Migration(int source, int destination,
		     double migration_rate,
		     double start_time, double end_time)
    : Epoch(start_time, end_time),
      i_source(source), i_destination(destination),
      i_migration_rate(migration_rate)
{
    assert(source >= 0);
    assert(destination >= 0);
    assert(migration_rate >= 0);
    assert(end_time >= 0);
}


Event *
Migration::copy() const
{
    return new Migration(*this);
}


double
Migration::nested_event_time(State &s, double current_time)
{
    Population &src = s.populations()[i_source];
    unsigned int k = src.size();
    if (k < 1) return std::numeric_limits<double>::max();
    double rate = i_migration_rate*k/2;
    return Distribution_functions::expdev(rate);
}

void
Migration::nested_update_state(Scheduler &scheduler, State &s,
			     double event_time)
{
    Population &src = s.populations()[i_source];
    Population &dst = s.populations()[i_destination];
    dst.push(src.pop_random());
}




