/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#ifndef CORE__BUILDER_HH_INCLUDED
#define CORE__BUILDER_HH_INCLUDED




namespace core {

    class CoalescentNode;
    class RecombinationNode;
    class GeneConversionNode;
    class ARG;
    class Configuration;
    class SimulationMonitor;

    struct BuilderMonitor
    {
	virtual void coalescence_callback(CoalescentNode *n,
					  int k) = 0;
	virtual void recombination_callback(RecombinationNode *n1,
					    RecombinationNode *n2,
					    int k) = 0;
	virtual void gene_conversion_callback(GeneConversionNode *n1,
					      GeneConversionNode *n2,
					      int k) = 0;
    };

    class Builder
    {
    public:
	Builder(const Configuration &conf) : i_conf(conf) {};
	~Builder() {};

	// Builds an ARG.  The ARG is dynamically allocated and must be
	// deleted after use.
	ARG *build(SimulationMonitor *mon,
		   BuilderMonitor    *callbacks = 0,
		   bool keep_empty_intervals = false) const;

    private:
	const Configuration &i_conf;
    };


}

#endif
