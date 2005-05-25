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
core::BottleNeck::add_events(Scheduler &scheduler, CoalescenceEvent &coa_event)
{
    scheduler.add_event(new BottleNeckEndPoint(&coa_event, i_start_point,
					       i_scale_fraction));
    scheduler.add_event(new BottleNeckEndPoint(&coa_event, i_end_point,
					       1.0/i_scale_fraction));
}

core::Epoch *
core::BottleNeck::copy() const
{
    return new BottleNeck(*this);
}
