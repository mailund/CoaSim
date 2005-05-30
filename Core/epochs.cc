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
	= new CoalescenceEventBottleneck(event_counter, i_scale_fraction);
    scheduler.add_event(new EpochStartEvent(i_start_point, event));
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
    assert(false); // not implemented yet!
}

core::Epoch *
core::Growth::copy() const
{
    return new Growth(*this);
}

