/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#ifndef MICRO_SATELLITE_MARKER_HH_INCLUDED
#define MICRO_SATELLITE_MARKER_HH_INCLUDED

#ifndef MARKER_HH_INCLUDED
# include "marker.hh"
#endif

class MicroSatelliteMarker : public Marker
{
public:
    MicroSatelliteMarker(double position, double mu) 
	: Marker(position), i_mu(mu) 
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
    double mu() const { return i_mu; }

    virtual void to_text(std::ostream &os) const;

private:
    double i_mu; // mutation parameter
};


#endif
