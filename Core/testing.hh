/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005, 2006 by Bioinformatics ApS
 *                                    and Thomas Mailund <mailund@mailund.dk>
 */

#include <iostream>
#include <cstdlib>

static bool test_verbose = 0;
static int test_count = 0, test_failures = 0;
#define CHECK(test) \
        { test_count++; \
          if (!(test)) { \
             if (test_verbose) \
                 std::cerr << "ERROR: " << __FILE__ << " line " << __LINE__ \
                           << ": test \"" << #test << "\" failed!\n"; \
             test_failures++; \
        }}

#define ERROR(message) \
        { test_count++; test_failures++; \
          if (test_verbose) \
             std::cerr << "ERROR: " << __FILE__ << " line " << __LINE__ \
                       << " :\n---------------------------------------------------------------------\n" \
                       << message \
                       << "\n---------------------------------------------------------------------\n\n"; \
        }


#define HANDLE_TEST_OPTIONS \
        if (argc > 1 and  (0 == strcmp(argv[1],"-v")) ) test_verbose = true;
#define REPORT_RESULTS \
        if (test_failures > 0) std::cout << test_failures \
                                         << " out of "    \
                                         << test_count    \
                                         << " tests failed!\n"; \
        return test_failures ? EXIT_FAILURE : EXIT_SUCCESS;
