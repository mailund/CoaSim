/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#ifndef CORE__MICRO_SATELLITE_MARKER_HH_INCLUDED
#define CORE__MICRO_SATELLITE_MARKER_HH_INCLUDED

#ifndef CORE__MARKER_HH_INCLUDED
# include "marker.hh"
#endif

namespace core {

    class MicroSatelliteMarker : public Marker
    {
    public:
	MicroSatelliteMarker(double position, double theta) 
	    : Marker(position), i_theta(theta) 
	{}
	virtual Marker *copy() const;
	virtual bool run_first() const;

	virtual int default_value() const;

	virtual void add_value(int value) throw(illegal_value)
	{
	    if (value < 0) throw illegal_value();
	    i_values.push_back(value);
	}

	virtual Mutator *create_mutator(const Configuration &conf,
					const RetiredInterval &ri) const;
	double theta() const { return i_theta; }

	virtual const char * type() const;

    private:
	double i_theta; // mutation parameter
    };

}

#endif
