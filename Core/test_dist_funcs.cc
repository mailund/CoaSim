/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005, 2006 by Bioinformatics ApS
 *                                    and Thomas Mailund <mailund@mailund.dk>
 */

#include "test_dist_funcs.hh"

namespace Distribution_functions
{
    double expdev_result = 0.0;
    double expdist_result = 0.0;
    double uniform_result = 0.0;
    int random_sign_result = 1;
    int uniform_part_result = 0;
    int irand_result = 1;
    int two_int_rand1 = 1;
    int two_int_rand2 = 2;

    double expdev(const double param) 
    {
	return expdev_result; 
    }
    double expdev(const int fac, const double param)
    { 
	return expdev_result; 
    }

    double expdist(const double param, const double x)
    { 
	return expdist_result; 
    }

    double uniform() 
    { 
	return uniform_result; 
    }
  
    int random_sign() 
    {
	return random_sign_result; 
    }
  
    int uniform(double part_1, double part_2, double part_3)
    { 
	return uniform_part_result; 
    }

    int irand(int n)
    { 
	return irand_result; 
    }

    void two_int_rand(int& n1, int& n2, int n)
    { 
	n1 = two_int_rand1 ; 
	n2 = two_int_rand2; 
    }
}
