/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#include "builder_events.hh"

#ifndef CORE__BUILDER_HH_INCLUDED
# include "builder.hh"
#endif
#ifndef CORE__DIST_FUNCTIONS_HH_INCLUDED
# include "dist_funcs.hh"
#endif
#ifndef CORE__MONITOR_HH_INCLUDED
# include "monitor.hh"
#endif
#ifndef CORE__NODE_HH_INCLUDED
# include "node.hh"
#endif

using namespace core;

Population::Population(ARG &arg, unsigned int initial_population_size)
{
    for (int i = 0; i < initial_population_size; ++i)
	push(arg.leaf());
}

Node *Population::pop()
{
    size_t index = Distribution_functions::irand(i_nodes.size());
    std::swap(i_nodes[index], i_nodes.back());
    Node *n = i_nodes.back();
    i_nodes.pop_back();
    return n;
}



double core::CoalescenceEvent::event_time(State &s, double current_time)
{
    using namespace Distribution_functions;
    unsigned int nodes_left = s.population().size();
    return current_time + expdev(nodes_left, double(nodes_left-1)/2);
}
void core::CoalescenceEvent::update_state(State &s, double event_time,
					  ARG   &arg, 
					  BuilderMonitor *callbacks)
{
    Population &population = s.population();
    unsigned int nodes_left = population.size();
    Node *child1 = population.pop();
    Node *child2 = population.pop();
    CoalescentNode *coa_node = arg.coalescence(event_time, child1, child2);
    if (callbacks) callbacks->coalescence_callback(coa_node, nodes_left);
    if (coa_node->intervals().size() > 0) population.push(coa_node);
    ++i_coal_event_counter;
}

double core::CoalescenceEventGrowth::event_time(State &s, double current_time)
{
    using namespace Distribution_functions;
    unsigned int nodes_left = s.population().size();
    double xx = expdev(nodes_left,double(nodes_left-1)/2);
    double yy = exp(-i_beta*current_time);
    return current_time + log(1.0+i_beta*xx*yy)/i_beta;
}

double
RecombinationEvent::event_time(State &s, double current_time)
{
    using namespace Distribution_functions;
    unsigned int nodes_left = s.population().size();
    return current_time + expdev(nodes_left, i_rho/2);
}

void
RecombinationEvent::update_state(State &s, double event_time,
				 ARG   &arg, BuilderMonitor *callbacks)
{
    using namespace Distribution_functions;

    double cross_over_point = uniform();
    Population &population = s.population();
    unsigned int nodes_left = s.population().size();
    Node *child = population.pop();

    try {
	ARG::recomb_node_pair_t pair
	    = arg.recombination(event_time, child, cross_over_point);

	if (pair.first->intervals().size() > 0)  population.push(pair.first); 
	if (pair.second->intervals().size() > 0) population.push(pair.second); 

	if (callbacks)
	    callbacks->recombination_callback(pair.first, pair.second,
					      nodes_left);

	++i_recomb_event_counter;

    } catch (ARG::null_event&) {
	population.push(child);
    }
}

double
GeneConversionEvent::event_time(State &s, double current_time)
{
    using namespace Distribution_functions;
    unsigned int nodes_left = s.population().size();
    return current_time + expdev(nodes_left, i_gamma/2);
}

void
GeneConversionEvent::update_state(State &s, double event_time,
				  ARG   &arg, BuilderMonitor *callbacks)
{
    using namespace Distribution_functions;

    double point = uniform();
    double length = random_sign()*expdev(i_Q);

    double start = std::max(0.0, (length < 0) ? point+length : point);
    double stop  = std::min(1.0, (length < 0) ? point : point+length);

    // it *is* technically possible to randomly choose an
    // empty length or hit one of the endpoints with the lengh
    // reaching outside the interval -- although very
    // unlikely.  If it happens, just pretend it didn't and
    // move on -- this is the same effect as if we select a
    // gene conversion outside an active interval.
    if (stop-start <= 0.0) return;

    Population &population = s.population();
    unsigned int nodes_left = s.population().size();

    Node *child = population.pop();
    try {
	ARG::gene_conv_node_pair_t pair = 
	    arg.gene_conversion(event_time, child, start, stop);

	if (pair.first->intervals().size() > 0)  population.push(pair.first); 
	if (pair.second->intervals().size() > 0) population.push(pair.second); 

	if (callbacks)
	    callbacks->gene_conversion_callback(pair.first, pair.second,
						nodes_left);

	++i_gene_conv_event_counter;

    } catch (ARG::null_event&) {
	population.push(child);
    }

}







Scheduler::~Scheduler() 
{
    std::vector<Event*>::iterator i;
    for (i = i_events.begin(); i != i_events.end(); ++i)
	delete *i;
}


Scheduler::time_event_t
Scheduler::next_event(State &s, double current_time)
{
    double minimal_time = std::numeric_limits<double>::max();
    Event *minimal_event = 0;
    std::vector<Event*>::iterator i;
    for (i = i_events.begin(); i != i_events.end(); ++i)
	{
	    double event_time = (*i)->event_time(s, current_time);
	    if (event_time < minimal_time)
		{
		    minimal_time = event_time;
		    minimal_event = *i;
		}
	}
    return time_event_t(minimal_time, minimal_event);
}

