/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005, 2006 by Bioinformatics ApS
 *                                    and Thomas Mailund <mailund@mailund.dk>
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

core::Scheduler::~Scheduler() 
{
    std::list<Event*>::iterator i;
    for (i = i_events.begin(); i != i_events.end(); ++i)
	delete *i;
}

void
core::Scheduler::add_event(Event *event) 
{
    i_events.push_back(event);
}

void
core::Scheduler::remove_event(Event *event)
{
    std::list<Event*>::iterator i;
    i = find(i_events.begin(), i_events.end(), event);
    assert(i != i_events.end());
    i_events.erase(i);
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
