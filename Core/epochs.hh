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
	double i_scale_fraction;
	double i_start_point;
	double i_end_point;
    public:
	BottleNeck(double scale_fraction, 
		   double start_point, double end_point)
	    : i_scale_fraction(scale_fraction),
	      i_start_point(start_point),
	      i_end_point(end_point)
	{
	    assert(scale_fraction > 0);
	    assert(start_point < end_point);
	}

	double scale_fraction() const { return i_scale_fraction; }
	double start_point()    const { return i_start_point; }
	double end_point()      const { return i_end_point; }

	virtual Epoch *copy() const;
	virtual void add_events(Scheduler &scheduler,
				CoalescenceEvent &coa_event);
    };
}


#endif // CORE__EPOCHS_HH_INCLUDED
