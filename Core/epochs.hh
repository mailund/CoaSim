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
        unsigned int i_population;
	double i_scale_fraction;
	double i_start_point;
	double i_end_point;
    public:
	BottleNeck(unsigned int population,
		   double scale_fraction, 
		   double start_point, double end_point = -1)
	    : i_population(population),
	      i_scale_fraction(scale_fraction),
	      i_start_point(start_point),
	      i_end_point(end_point)
	{
	    assert(scale_fraction > 0);
	    assert(end_point < 0 or start_point < end_point);
	}

	double scale_fraction() const { return i_scale_fraction; }
	double start_point()    const { return i_start_point; }
	double end_point()      const { return i_end_point; }

	virtual Epoch *copy() const;
	virtual void add_events(Scheduler &scheduler,
				unsigned int &event_counter);
    };

    class Growth : public Epoch {
        unsigned int i_population;
	double i_beta;
	double i_start_point;
	double i_end_point;
    public:
	Growth(unsigned int population,
	       double beta, double start_point, double end_point = -1)
  	    : i_population(population),
	      i_beta(beta),
	      i_start_point(start_point),
	      i_end_point(end_point)
	{
	    assert(beta > 0);
	    assert(end_point < 0 or start_point < end_point);
	}

	double beta()        const { return i_beta; }
	double start_point() const { return i_start_point; }
	double end_point()   const { return i_end_point; }

	virtual Epoch *copy() const;
	virtual void add_events(Scheduler &scheduler,
				unsigned int &event_counter);
    };
    
    class PopulationMerge : public Epoch {
	unsigned int i_pop_1, i_pop_2;
        double i_merge_time;

    public:
	PopulationMerge(unsigned int pop_1, unsigned int pop_2,
			double merge_time)
	    : i_pop_1(pop_1), i_pop_2(pop_2), i_merge_time(merge_time)
	{
	}

	unsigned int population1() const { return i_pop_1; }
	unsigned int population2() const { return i_pop_2; }
	double       merge_time()  const { return i_merge_time; }

	virtual Epoch *copy() const;
	virtual void add_events(Scheduler &scheduler,
				unsigned int &event_counter);
    };
}


#endif // CORE__EPOCHS_HH_INCLUDED
