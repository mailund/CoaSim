/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#ifndef CORE__BUILDER_EVENTS_HH_INCLUDED
#define CORE__BUILDER_EVENTS_HH_INCLUDED

#ifndef VECTOR_INCLUDED
# include <vector>
# define VECTOR_INCLUDED
#endif

namespace core {

    class ARG;
    class Node;
    class BuilderMonitor;

    class Population {
	std::vector<Node*>i_nodes;
    public:
	Population(ARG &arg, unsigned int initial_population_size);
	void push(Node *n) { i_nodes.push_back(n); }
	Node *pop();
	size_t size() const { return i_nodes.size(); }
    };


    class State {
	Population i_population;
    public:
	State(ARG &arg, unsigned int initial_population_size)
	    : i_population(arg, initial_population_size)
	{}
	Population &population() { return i_population; }
    };


    struct Event {
	virtual double event_time  (State &s, double current_time)
	    = 0;
	virtual void   update_state(State &s, double event_time,
				    ARG   &arg, BuilderMonitor *callbacks)
	    = 0;
    };

    class CoalescenceEvent : public Event {
	unsigned int &i_coal_event_counter;
    public:
	CoalescenceEvent(unsigned int &event_counter)
	    : i_coal_event_counter(event_counter)
	{}
	virtual double event_time  (State &s, double current_time);
	virtual void   update_state(State &s, double event_time,
				    ARG   &arg, BuilderMonitor *callbacks);
    };

    class CoalescenceEventGrowth : public CoalescenceEvent {
	double i_beta;
    public:
	CoalescenceEventGrowth(unsigned int &event_counter, double beta) 
	    : CoalescenceEvent(event_counter), i_beta(beta) 
	{}
	virtual double event_time  (State &s, double current_time);
    };

    class RecombinationEvent : public Event {
	unsigned int &i_recomb_event_counter;
	double i_rho;
    public:
	RecombinationEvent(unsigned int &event_counter, double rho) 
	    : i_recomb_event_counter(event_counter), i_rho(rho)
	{}
	virtual double event_time  (State &s, double current_time);
	virtual void   update_state(State &s, double event_time,
				    ARG   &arg, BuilderMonitor *callbacks);
    };

    class GeneConversionEvent : public Event {
	unsigned int &i_gene_conv_event_counter;
	double i_gamma;
	double i_Q;
    public:
	GeneConversionEvent(unsigned int &event_counter,
			    double gamma, double Q)
	    : i_gene_conv_event_counter(event_counter), i_gamma(gamma), i_Q(Q)
	{}
	virtual double event_time  (State &s, double current_time);
	virtual void   update_state(State &s, double event_time,
				    ARG   &arg, BuilderMonitor *callbacks);
    };
    


    class Scheduler {
	std::vector<Event*> i_events;

    public:
	~Scheduler();
	void add_event(Event *event) { i_events.push_back(event); }

	typedef std::pair<double,Event*> time_event_t;
	time_event_t next_event(State &s, double current_time);
    };

}

#endif
