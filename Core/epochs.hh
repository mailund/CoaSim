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

namespace core {

    class BottleNeck : public Epoch {
        int    i_population;
	double i_scale_fraction;
    public:
	BottleNeck(int population,
		   double scale_fraction, 
		   double start_time, double end_time = -1)
	    : Epoch(start_time, end_time),
	      i_population(population),
	      i_scale_fraction(scale_fraction)
	{
	    assert(scale_fraction > 0);
	}

	int    population()     const { return i_population; }
	double scale_fraction() const { return i_scale_fraction; }

	virtual Epoch *copy() const;
	virtual void add_events(Scheduler &scheduler,
				unsigned int &event_counter);
    };

    class Growth : public Epoch {
        int    i_population;
	double i_beta;
    public:
	Growth(int population, double beta, 
	       double start_time, double end_time = -1)
  	    : Epoch(start_time, end_time),
	      i_population(population),
	      i_beta(beta)
	{
	    assert(beta > 0);
	}

	int    population()  const { return i_population; }
	double beta()        const { return i_beta; }

	virtual Epoch *copy() const;
	virtual void add_events(Scheduler &scheduler,
				unsigned int &event_counter);
    };
    
    class PopulationMerge : public Epoch {
	int i_pop_1, i_pop_2;
        double i_merge_time;

    public:
	PopulationMerge(int pop_1, int pop_2, double merge_time)
	    : Epoch(merge_time, -1),
	      i_pop_1(pop_1), i_pop_2(pop_2), i_merge_time(merge_time)
	{
	    assert(pop_1 >= 0);
	    assert(pop_2 >= 0);
	    assert(merge_time >= 0);
	}

	int    population1() const { return i_pop_1; }
	int    population2() const { return i_pop_2; }
	double merge_time()  const { return i_merge_time; }

	virtual Epoch *copy() const;
	virtual void add_events(Scheduler &scheduler,
				unsigned int &event_counter);
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

	virtual Epoch *copy() const;
	virtual void add_events(Scheduler &scheduler,
				unsigned int &event_counter);
    };

}


#endif // CORE__EPOCHS_HH_INCLUDED
