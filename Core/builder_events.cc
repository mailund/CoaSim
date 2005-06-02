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



Population::Population(ARG &arg, int initial_population_size,
		       CoalescenceEvent *coal_event,
		       double scale_fraction)
    : i_coal_event(coal_event), i_scale_fraction(scale_fraction)
{
    for (int i = 0; i < initial_population_size; ++i)
	push(arg.leaf());
}

Node *Population::pop_random()
{
    size_t index = Distribution_functions::irand(i_nodes.size());
    std::swap(i_nodes[index], i_nodes.back());
    return pop_last();
}

Node *Population::pop_last()
{
    Node *n = i_nodes.back();
    i_nodes.pop_back();
    return n;
}


size_t
State::total_population_size() const
{
    std::vector<Population>::const_iterator i;
    size_t total = 0;
    for (i = i_populations.begin(); i != i_populations.end(); ++i)
	total += i->size();
    return total;
}

Population&
State::random_population()
{
    using namespace Distribution_functions;
    int individual = irand(total_population_size());
    std::vector<Population>::iterator i;
    size_t total = 0;
    for (i = i_populations.begin(); i != i_populations.end(); ++i)
	{
	    total += i->size();
	    if (individual <= total)
		return *i;
	}
    assert(false); // we should not reach this point since we must
		   // select an individual in one of the populations
}

core::Event *
core::CoalescenceEvent::copy() const
{
    return new CoalescenceEvent(*this);
}

double
core::CoalescenceEvent::waiting_time(State &s, double current_time)
{
    using namespace Distribution_functions;
    Population &p = s.populations().at(i_population);
    unsigned int nodes_left = p.size();
    if (nodes_left < 2) return std::numeric_limits<double>::max();

    double scale_fraction = p.scale_fraction();
    double delta_time = expdev(nodes_left, double(nodes_left-1)/2);
    return scale_fraction * delta_time;
}

double
core::CoalescenceEvent::event_time(State &s, double current_time)
{
    return current_time + waiting_time(s, current_time);
}

void
core::CoalescenceEvent::update_state(Scheduler &scheduler, State &s,
				     double event_time)
{
    ARG &arg = s.arg();
    BuilderMonitor *callbacks = s.callbacks();
    Population &p = s.populations().at(i_population);
    assert(p.size() >= 2);

    unsigned int nodes_left = s.total_population_size();

    Node *child1 = p.pop_random();
    Node *child2 = p.pop_random();

    assert(child1);
    assert(child2);

    CoalescentNode *coa_node = arg.coalescence(event_time, child1, child2);
    if (callbacks) callbacks->coalescence_callback(coa_node, nodes_left);
    if (coa_node->intervals().size() > 0) p.push(coa_node);
}


core::Event *
core::RecombinationEvent::copy() const
{
    return new RecombinationEvent(*this);
}

double
RecombinationEvent::event_time(State &s, double current_time)
{
    using namespace Distribution_functions;
    unsigned int k = s.total_population_size();
    return current_time + expdev(k, i_rho/2);
}

void
RecombinationEvent::update_state(Scheduler &scheduler, State &s,
				 double event_time)
{
    using namespace Distribution_functions;

    ARG &arg = s.arg();
    BuilderMonitor *callbacks = s.callbacks();

    double cross_over_point = uniform();
    Population &population = s.random_population();
    assert(population.size() > 0);

    unsigned int nodes_left = s.total_population_size();
    Node *child = population.pop_random();

    try {
	ARG::recomb_node_pair_t pair
	    = arg.recombination(event_time, child, cross_over_point);

	if (pair.first->intervals().size() > 0)  population.push(pair.first); 
	if (pair.second->intervals().size() > 0) population.push(pair.second); 

	if (callbacks)
	    callbacks->recombination_callback(pair.first, pair.second,
					      nodes_left);

    } catch (ARG::null_event&) {
	population.push(child);
    }
}

core::Event *
core::GeneConversionEvent::copy() const
{
    return new GeneConversionEvent(*this);
}


double
GeneConversionEvent::event_time(State &s, double current_time)
{
    using namespace Distribution_functions;
    unsigned int k = s.total_population_size();
    return current_time + expdev(k, i_gamma/2);
}

void
GeneConversionEvent::update_state(Scheduler &scheduler, State &s,
				  double event_time)
{
    using namespace Distribution_functions;

    ARG &arg = s.arg();
    BuilderMonitor *callbacks = s.callbacks();

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

    Population &population = s.random_population();
    assert(population.size() > 0);
    unsigned int nodes_left = s.total_population_size();

    Node *child = population.pop_random();
    try {
	ARG::gene_conv_node_pair_t pair = 
	    arg.gene_conversion(event_time, child, start, stop);

	if (pair.first->intervals().size() > 0)  population.push(pair.first); 
	if (pair.second->intervals().size() > 0) population.push(pair.second); 

	if (callbacks)
	    callbacks->gene_conversion_callback(pair.first, pair.second,
						nodes_left);

    } catch (ARG::null_event&) {
	population.push(child);
    }

}




Event *
PopulationMerge::copy() const
{
    return new PopulationMerge(*this);
}

double
PopulationMerge::event_time(State &s, double current_time)
{
    return i_merge_time;
}

void
PopulationMerge::update_state(Scheduler &scheduler, State &s,
			      double event_time)
{
    Population &p1 = s.populations().at(i_pop_1);
    Population &p2 = s.populations().at(i_pop_2);

    // move the p2 population to p1
    while (p2.size() > 0) p1.push(p2.pop_last());

    Event *pop_2_coal = p2.coalescence_event();
    p2.coalescence_event(0);// null the event

    scheduler.remove_event(pop_2_coal);
    scheduler.remove_event(this);

    delete pop_2_coal;
    delete this;
}

