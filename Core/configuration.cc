/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#include "configuration.hh"



core::Configuration::~Configuration() 
{
    for (int i = 0; i < no_markers(); ++i)
	{
	    delete i_first_markers[i];
	    delete i_plain_markers[i];
	}
    delete[] i_first_markers; 
    delete[] i_plain_markers;

    std::vector<Event*>::iterator i;
    for (i = i_events.begin(); i != i_events.end(); ++i)
	delete *i;
}

core::Event::~Event() {}
core::Epoch::~Epoch() {}

double
core::Epoch::event_time(State &s, double current_time)
{
    if (current_time < start_time())
	return std::numeric_limits<double>::max();
    return std::min(nested_event_time(s, current_time), end_time());
}

void
core::Epoch::update_state(Scheduler &scheduler, State &s, double event_time)
{
    if (event_time >= end_time())
	{
	    // remove epoch
	    scheduler.remove_event(this);
	    delete this;
	}
    else
	nested_update_state(scheduler, s, event_time);
}


core::Scheduler::~Scheduler() 
{
    std::list<Event*>::iterator i;
    for (i = i_events.begin(); i != i_events.end(); ++i)
	delete *i;
}


core::Scheduler::time_event_t
core::Scheduler::next_event(State &s, double current_time)
{
    double minimal_time = std::numeric_limits<double>::max();
    Event *earliest_event = 0;
    std::list<Event*>::iterator i;
    for (i = i_events.begin(); i != i_events.end(); ++i)
	{
	    double event_time = (*i)->event_time(s, current_time);
	    if (event_time < minimal_time)
		{
		    minimal_time = event_time;
		    earliest_event = *i;
		}
	}
    return time_event_t(std::max(current_time,minimal_time), earliest_event);
}

void
core::Scheduler::remove_event(Event *event)
{
    std::list<Event*>::iterator i;
    i = find(i_events.begin(), i_events.end(), event);
    assert(i != i_events.end());
    i_events.erase(i);
}
