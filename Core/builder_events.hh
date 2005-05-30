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
#ifndef LIST_INCLUDED
# include <list>
# define LIST_INCLUDED
#endif

namespace core {

    class ARG;
    class Node;
    class BuilderMonitor;
    class Scheduler;
    class CoalescenceEvent;

    class Population {
	CoalescenceEvent *i_coal_event;
	double i_scale_fraction;
	std::vector<Node*>i_nodes;
    public:
	Population(ARG &arg, unsigned int initial_population_size,
		   CoalescenceEvent *coal_event,
		   double scale_fraction = 1);
	
	void push(Node *n) { i_nodes.push_back(n); }
	Node *pop();
	size_t size() const { return i_nodes.size(); }

	double scale_fraction() const { return i_scale_fraction; }
	CoalescenceEvent *coalescence_event() { return i_coal_event; }
	void coalescence_event(CoalescenceEvent *e) { i_coal_event = e; }
    };


    class State {
	ARG &i_arg;
	Population i_population;
	BuilderMonitor *i_callbacks;

    public:
	// FIXME: This initialization is not optimal
	State(ARG &arg, BuilderMonitor *callbacks,
	      unsigned int initial_population_size,
	      unsigned int &coal_counter);

	ARG        &arg()           { return i_arg; }
	Population &population()    { return i_population; }
	BuilderMonitor *callbacks() { return i_callbacks; }
    };


    struct Event {
	virtual ~Event();
	virtual double event_time  (State &s, double current_time)
	    = 0;
	virtual void   update_state(Scheduler &scheduler, State &s,
				    double event_time)
	    = 0;
    };

    class CoalescenceEvent : public Event {
	unsigned int &i_coal_event_counter;


    public:
	CoalescenceEvent(unsigned int &event_counter)
	    : i_coal_event_counter(event_counter)
	{
	}

	virtual double waiting_time(State &s, double current_time);
	virtual double event_time  (State &s, double current_time);
	virtual void   update_state(Scheduler &scheduler, State &s,
				    double event_time);
    };

    class CoalescenceEventExtension : public CoalescenceEvent {
	CoalescenceEvent *i_underlying;

    protected:
	double basic_waiting_time(State &s, double current_time)
	{
	    assert(i_underlying);
	    return i_underlying->waiting_time(s, current_time);
	}
	virtual double waiting_time(State &s, double current_time) = 0;

    public:
	CoalescenceEventExtension(unsigned int &event_counter)
	    : CoalescenceEvent(event_counter),
	      i_underlying(0)
	{}
	~CoalescenceEventExtension();

	void push(Scheduler &scheduler, State &s);
	void pop (Scheduler &scheduler, State &s);
    };
    
    class CoalescenceEventGrowth : public CoalescenceEventExtension {
	double i_beta;
	double i_start_time;

	virtual double waiting_time(State &s, double current_time);
    public:
	CoalescenceEventGrowth(unsigned int &event_counter,
			       double beta = 0,
			       double start_time = 0)
	    : CoalescenceEventExtension(event_counter),
	      i_beta(beta), i_start_time(start_time)
	{}
    };

    class CoalescenceEventBottleneck : public CoalescenceEventExtension {
	double i_scale_fraction;

	virtual double waiting_time(State &s, double current_time);
    public:
	CoalescenceEventBottleneck(unsigned int &event_counter,
				   double scale_fraction)
	    : CoalescenceEventExtension(event_counter),
	      i_scale_fraction(scale_fraction)
	{}
    };


    class EpochStartEvent : public Event {
	double i_start;
	CoalescenceEventExtension *i_epoch;
    public:
	EpochStartEvent(double start, CoalescenceEventExtension *epoch)
	    : i_start(start), i_epoch(epoch)
	{}
	virtual double event_time  (State &s, double current_time);
	virtual void   update_state(Scheduler &scheduler, State &s,
				    double event_time);
    };

    class EpochEndEvent : public Event {
	double i_end;
	CoalescenceEventExtension *i_epoch;
    public:
	EpochEndEvent(double end, CoalescenceEventExtension *epoch)
	    : i_end(end), i_epoch(epoch)
	{}
	virtual double event_time  (State &s, double current_time);
	virtual void   update_state(Scheduler &scheduler, State &s,
				    double event_time);
    };


    class RecombinationEvent : public Event {
	unsigned int &i_recomb_event_counter;
	double i_rho;
    public:
	RecombinationEvent(unsigned int &event_counter, double rho) 
	    : i_recomb_event_counter(event_counter), i_rho(rho)
	{}
	virtual double event_time  (State &s, double current_time);
	virtual void   update_state(Scheduler &scheduler, State &s,
				    double event_time);
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
	virtual void   update_state(Scheduler &scheduler, State &s,
				    double event_time);
    };
    


    class Scheduler {
	std::list<Event*> i_events;

    public:
	~Scheduler();
	void add_event(Event *event) { i_events.push_back(event); }
	void remove_event(Event *event);

	typedef std::pair<double,Event*> time_event_t;
	time_event_t next_event(State &s, double current_time);

    };

}

#endif
