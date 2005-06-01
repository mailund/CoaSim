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
#ifndef CASSERT_INCLUDED
# include <cassert>
# define CASSERT_INCLUDED
#endif
#ifndef STDEXCEPT_INCLUDED
# include <stdexcept>
# define STDEXCEPT_INCLUDED
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
	Population(ARG &arg,
		   int initial_population_size,
		   CoalescenceEvent *coal_event,
		   double scale_fraction = 1);
	
	void push(Node *n) { i_nodes.push_back(n); }
	Node *pop_random();
	Node *pop_last();
	size_t size() const { return i_nodes.size(); }

	double scale_fraction() const { return i_scale_fraction; }
	CoalescenceEvent *coalescence_event() { return i_coal_event; }
	void coalescence_event(CoalescenceEvent *e) { i_coal_event = e; }
    };


    class State {
	ARG &i_arg;
	std::vector<Population> i_populations;
	BuilderMonitor *i_callbacks;

    public:
	// FIXME: This initialization is not optimal
	template <typename Itr>
	State(ARG &arg, BuilderMonitor *callbacks,
	      Itr sizes_begin, Itr sizes_end,
	      unsigned int &coal_counter);


	std::vector<Population> &populations() { return i_populations; }
	ARG                     &arg()         { return i_arg; }
	BuilderMonitor          *callbacks()   { return i_callbacks; }

	size_t total_population_size() const;
	// returns a random population, where each population is
	// weighted with its size.
	Population &random_population();
    };


    struct Event {
	virtual ~Event();
	virtual double event_time  (State &s, double current_time)
	    = 0;
	virtual void   update_state(Scheduler &scheduler, State &s,
				    double event_time)
	    = 0;
    };

    // thrown if a population index is given outside the allowed
    // values
    struct illegal_population : std::logic_error {
	illegal_population() : std::logic_error("Illegal populaiton id") {}
    };

    class CoalescenceEvent : public Event {
    protected:
	int i_population;
    private:
	unsigned int &i_coal_event_counter;

    public:
	CoalescenceEvent(int population, unsigned int &event_counter)
	    : i_population(population), i_coal_event_counter(event_counter)
	{
	    if (population < 0) throw illegal_population();
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
	CoalescenceEventExtension(int population, unsigned int &event_counter)
	    : CoalescenceEvent(population, event_counter),
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
	CoalescenceEventGrowth(int population,
			       unsigned int &event_counter,
			       double beta = 0,
			       double start_time = 0)
	    : CoalescenceEventExtension(population, event_counter),
	      i_beta(beta), i_start_time(start_time)
	{}
    };

    class CoalescenceEventBottleneck : public CoalescenceEventExtension {
	double i_scale_fraction;

	virtual double waiting_time(State &s, double current_time);
    public:
	CoalescenceEventBottleneck(int population,
				   unsigned int &event_counter,
				   double scale_fraction)
	    : CoalescenceEventExtension(population, event_counter),
	      i_scale_fraction(scale_fraction)
	{}
    };


    class EpochStartEvent : public Event {
	double i_start;
	Event *i_epoch;
    public:
	EpochStartEvent(double start, Event *epoch)
	    : i_start(start), i_epoch(epoch)
	{}
	virtual double event_time  (State &s, double current_time);
	virtual void   update_state(Scheduler &scheduler, State &s,
				    double event_time);
    };

    class EpochEndEvent : public Event {
	double i_end;
	Event *i_epoch;
    public:
	EpochEndEvent(double end, Event *epoch)
	    : i_end(end), i_epoch(epoch)
	{}
	virtual double event_time  (State &s, double current_time);
	virtual void   update_state(Scheduler &scheduler, State &s,
				    double event_time);
    };
	

    class CoalEpochStartEvent : public EpochStartEvent {
	CoalescenceEventExtension *i_epoch;
    public:
	CoalEpochStartEvent(double start, CoalescenceEventExtension *epoch)
	    : EpochStartEvent(start,epoch), i_epoch(epoch)
	{}
	virtual void   update_state(Scheduler &scheduler, State &s,
				    double event_time);
    };

    class CoalEpochEndEvent : public EpochEndEvent {
	CoalescenceEventExtension *i_epoch;
    public:
	CoalEpochEndEvent(double end, CoalescenceEventExtension *epoch)
	    : EpochEndEvent(end,epoch), i_epoch(epoch)
	{}
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
    

    // a merge moves the second population to the first and disables
    // all events in the second.
    class MergePopulationsEvent : public Event {
        int    i_pop_1, i_pop_2;
	double i_merge_time;
    public:
	MergePopulationsEvent(int pop_1, int pop_2, double merge_time)
  	    : i_pop_1(pop_1), i_pop_2(pop_2), i_merge_time(merge_time)
	{
	    if (pop_1 < 0) throw illegal_population();
	    if (pop_2 < 0) throw illegal_population();
	}
	
	virtual double event_time  (State &s, double current_time);
	virtual void   update_state(Scheduler &scheduler, State &s,
				    double event_time);
    };

    class MigrationEvent : public Event {
        int    i_source, i_destination;
	double i_migration_rate;
    public:
	MigrationEvent(unsigned int source,
		       unsigned int destination,
		       double migration_rate)
  	    : i_source(source), i_destination(destination),
	      i_migration_rate(migration_rate)
	{
	    if (source < 0)      throw illegal_population();
	    if (destination < 0) throw illegal_population();
	}

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





    // FIXME: This initialization is not optimal
    template <typename Itr>
    State::State(ARG &arg, BuilderMonitor *callbacks,
		 Itr sizes_begin, Itr sizes_end,
		 unsigned int &coal_counter)
        : i_arg(arg), i_callbacks(callbacks)
    {
      for (int p_no = 0; sizes_begin != sizes_end; ++p_no, ++sizes_begin)
	  {
	      Population p(arg, *sizes_begin,
			   new CoalescenceEvent(p_no, coal_counter));
	      i_populations.push_back(p);
	  }
    }

}

#endif
