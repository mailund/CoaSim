/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005, 2006 by Bioinformatics ApS
 *                                    and Thomas Mailund <mailund@mailund.dk>
 */

#ifndef CORE__MICRO_SATELLITE_MARKER_HH_INCLUDED
#define CORE__MICRO_SATELLITE_MARKER_HH_INCLUDED

#ifndef CORE__MARKER_HH_INCLUDED
# include "marker.hh"
#endif

#ifndef SSTREAM_INCLUDED
# include <sstream>
# define SSTREAM_INCLUDED
#endif

namespace core {

    class MicroSatelliteMarker : public Marker
    {
	inline std::string i2s(int i)
	{ std::ostringstream os; os << i; return os.str(); }
	inline std::string d2s(double d)
	{ std::ostringstream os; os << d; return os.str(); }
    public:
	MicroSatelliteMarker(double position, double theta, int K) 
	    : Marker(position), i_theta(theta), i_K(K)
	{
	    if (theta <= 0) throw illegal_value(d2s(theta));
	    if (K <= 0)     throw illegal_value(i2s(K));
	}
	virtual Marker *copy() const;
	virtual bool run_first() const;

	virtual int default_value() const;

	virtual Mutator *create_mutator(const Configuration &conf,
					const RetiredInterval &ri) const;
	double theta() const { return i_theta; }
	int K() const { return i_K; }

	virtual const char * type() const;

    private:
	double i_theta; // mutation parameter
	int i_K;
    };

}

#endif
