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

void
core::BottleNeck::add_events(Scheduler &scheduler, unsigned int &event_counter)
{
    CoalescenceEventBottleneck *event
	= new CoalescenceEventBottleneck(i_population,
					 event_counter,
					 i_scale_fraction);
    scheduler.add_event(new EpochStartEvent(i_start_point, event));
    if (i_end_point > 0)
      scheduler.add_event(new EpochEndEvent(i_end_point, event));
}

core::Epoch *
core::BottleNeck::copy() const
{
    return new BottleNeck(*this);
}



void
core::Growth::add_events(Scheduler &scheduler, unsigned int &event_counter)
{
    CoalescenceEventGrowth *event
	= new CoalescenceEventGrowth(i_population, event_counter, i_beta);
    scheduler.add_event(new EpochStartEvent(i_start_point, event));
    if (i_end_point > 0)
      scheduler.add_event(new EpochEndEvent(i_end_point, event));
}

core::Epoch *
core::Growth::copy() const
{
    return new Growth(*this);
}


void
core::PopulationMerge::add_events(Scheduler &scheduler, unsigned int &event_counter)
{
    scheduler.add_event(new MergePopulationsEvent(i_pop_1, i_pop_2, i_merge_time));
}

core::Epoch *
core::PopulationMerge::copy() const
{
    return new PopulationMerge(*this);
}

