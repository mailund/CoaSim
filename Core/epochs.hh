/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#ifndef CORE__EPOCHS_HH_INCLUDED
#define CORE__EPOCHS_HH_INCLUDED

// the abstract epoch class is defined in the configuration, since it
// is a configuration thingy.
#ifndef CORE__CONFIGURATION_HH_INCLUDED
# include "configuration.hh"
#endif
#ifndef CORE__BUILDER_EVENTS_HH_INCLUDED
# include "builder_events.hh"
#endif

namespace core {

    class CoalescenceEpoch : public CoalescenceEvent,
			     public Epoch {
	CoalescenceEvent *i_underlying;

    protected:
	double basic_waiting_time(State &s, double current_time)
	{
	    assert(i_underlying);
	    return i_underlying->waiting_time(s, current_time);
	}

	// override this to implement the change in waiting time for
	// this epoch.
	virtual double waiting_time(State &s, double current_time) = 0;

	// these are overridden to compose the waiting times and
	// perform a basic coalescence event
	virtual double nested_event_time(State &s, double current_time);
	virtual void   nested_update_state(Scheduler &scheduler, State &s,
					   double event_time);

	// handles the push/pop nature of coalescence epochs
	virtual void   update_state(Scheduler &scheduler, State &s,
				    double event_time);

    public:
	CoalescenceEpoch(int population, double start_time, double end_time)
	    : Epoch(start_time, end_time),
	      CoalescenceEvent(population),
	      i_underlying(0)
	{
	}
	virtual ~CoalescenceEpoch();

	
    };

    class BottleNeck : public CoalescenceEpoch {
	double i_scale_fraction;
	virtual double waiting_time(State &s, double current_time);

    public:
	BottleNeck(int population, double scale_fraction, 
		   double start_time, double end_time = -1)
	    : CoalescenceEpoch(population, start_time, end_time),
	      i_scale_fraction(scale_fraction)
	{
	    assert(scale_fraction > 0);
	}

	double scale_fraction() const { return i_scale_fraction; }

	virtual Event *copy() const;
    };


    class Growth : public CoalescenceEpoch {
	double i_beta;

	virtual double waiting_time(State &s, double current_time);

    public:
	Growth(int population, double beta, 
	       double start_time, double end_time = -1)
  	    : CoalescenceEpoch(population, start_time, end_time),
	      i_beta(beta)
	{
	    assert(beta > 0);
	}

	double beta()        const { return i_beta; }

	virtual Event *copy() const;
    };
    

    class Migration : public Epoch {
	int i_source, i_destination;
	double i_migration_rate;

	virtual double nested_event_time  (State &s, double current_time);
	virtual void   nested_update_state(Scheduler &scheduler, State &s,
					   double event_time);

    public:
	Migration(int source, int destination,
		  double migration_rate,
		  double start_time, double end_time);

	int    source()         const { return i_source; }
	int    destination()    const { return i_destination; }
	double migration_rate() const { return i_migration_rate; }

	virtual Event *copy() const;
    };

}


#endif // CORE__EPOCHS_HH_INCLUDED
