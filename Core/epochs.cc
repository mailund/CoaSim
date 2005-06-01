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

using namespace core;

void
BottleNeck::add_events(Scheduler &scheduler, unsigned int &event_counter)
{
    CoalescenceEventBottleneck *event
	= new CoalescenceEventBottleneck(i_population,
					 event_counter,
					 i_scale_fraction);
    scheduler.add_event(new CoalEpochStartEvent(start_time(), event));
    if (end_time() > 0)
      scheduler.add_event(new CoalEpochEndEvent(end_time(), event));
}

Epoch *
BottleNeck::copy() const
{
    return new BottleNeck(*this);
}



void
Growth::add_events(Scheduler &scheduler, unsigned int &event_counter)
{
    CoalescenceEventGrowth *event
	= new CoalescenceEventGrowth(i_population, event_counter, i_beta);
    scheduler.add_event(new CoalEpochStartEvent(start_time(), event));
    if (end_time() > 0)
      scheduler.add_event(new CoalEpochEndEvent(end_time(), event));
}

Epoch *
Growth::copy() const
{
    return new Growth(*this);
}


void
PopulationMerge::add_events(Scheduler &scheduler, unsigned int &event_counter)
{
    scheduler.add_event(new MergePopulationsEvent(i_pop_1, i_pop_2,
						  i_merge_time));
}

Epoch *
PopulationMerge::copy() const
{
    return new PopulationMerge(*this);
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

void
Migration::add_events(Scheduler &scheduler, unsigned int &event_counter)
{
    // make a copy, since this remains in the configuration for future
    // use (and will be deleted when the configuration is
    scheduler.add_event(copy());
}

core::Epoch *
core::Migration::copy() const
{
    return new Migration(*this);
}


double
Migration::nested_event_time(State &s, double current_time)
{
    // FIXME: not sure about this
    Population &src = s.populations()[i_source];
    unsigned int k = src.size();
    if (k < 1) return std::numeric_limits<double>::max();
    double rate = i_migration_rate*k/2;	// population scale fraction???
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




