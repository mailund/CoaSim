/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#include "config.hh"

#ifndef GUILE__MARKER_HH_INCLUDED
# include "marker.hh"
#endif
#ifndef GUILE__EPOCHS_HH_INCLUDED
# include "epochs.hh"
#endif
#ifndef GUILE__OPTIONS_HH_INCLUDED
# include "options.hh"
#endif
#ifndef GUILE__NODES_HH_INCLUDED
# include "nodes.hh"
#endif
#ifndef GUILE__INTERVALS_HH_INCLUDED
# include "intervals.hh"
#endif
#ifndef GUILE__SIMULATE_HH_INCLUDED
# include "simulate.hh"
#endif


#ifndef POPT_H_INCLUDED
# include <popt.h>
# define POPT_H_INCLUDED
#endif

#ifndef SYS_TYPES_H_INCLUDED
# include <sys/types.h>
# define SYS_TYPES_H_INCLUDED
#endif
#ifndef SYS_STAT_H_INCLUDED
# include <sys/stat.h>
# define SYS_STAT_H_INCLUDED
#endif
#ifndef UNISTD_H_INCLUDED
# include <unistd.h>
# define UNISTD_H_INCLUDED
#endif

#ifndef IOSTREAM_INCLUDED
# include <iostream>
# define IOSTREAM_INCLUDED
#endif


namespace options {
    int version;
    int contact;
    int interactive;
};


static struct poptOption info_options[] = {
    {
	"version",
	'\0',
	POPT_ARG_NONE,
	&options::version,
	0,
	"Print version of "PACKAGE_NAME".",
	0
    },
    {
	"contact",
	'\0',
	POPT_ARG_NONE,
	&options::contact,
	0,
	"Print contact information.",
	0
    },
    { 0 } // sentinel
};


static struct poptOption main_options[] = {
    {
	0,
	'\0',
	POPT_ARG_INCLUDE_TABLE,
	&info_options,
	0,
	"Information options",
	0
    },
    {
	"verbose",
	'v',
	POPT_ARG_NONE,
	&options::verbose,
	0,
	"Toggle verbose output.",
	0
    },
    {
	"interactive",
	'i',
	POPT_ARG_NONE,
	&options::interactive,
	0,
	"Enter interactive mode after executing scripts.",
	0
    },

    POPT_AUTOHELP
    { 0 } // sentinel
};


static void
read_home_rc_file()
{
    const char *home_dir = getenv("HOME");
    if (home_dir)
	{
	    std::string home_rc(home_dir); home_rc += "/.coasim/startup.scm";
	    struct stat s;
	    if (stat(home_rc.c_str(), &s) == 0 and S_ISREG(s.st_mode))
		{
		    if (options::verbose) 
			std::cout << "Executing " << home_rc << std::endl;
		    scm_c_primitive_load(home_rc.c_str());
		}
	    else
		if (options::verbose)
		    std::cout << "No personal configuraion file "
			      << home_rc << " found." << std::endl;
	}
}

static void 
init_scheme_bindings(void *dummy)
{
    guile::install_marker();
    guile::install_nodes();
    guile::install_intervals();
    guile::install_epochs();
    guile::install_simulate();
}


int
main(int argc, const char *argv[])
{


  try {

	poptContext ctxt = poptGetContext(0, argc, argv, main_options, 0);
	poptSetOtherOptionHelp(ctxt, "scripts");

	int opt = poptGetNextOpt(ctxt);
	if (opt < -1)
	    {
		std::cerr << poptBadOption(ctxt, POPT_BADOPTION_NOALIAS)
			  << ':' << poptStrerror(opt) << std::endl;
		exit(2);
	    }

	if (options::version)
	    {
		std::cout << PACKAGE_STRING << std::endl;
		exit(0);
	    }

	if (options::contact)
	    {
		std::cout << PACKAGE_STRING << "\n\n"
			  << "For questions or comments, contact "
			  << PACKAGE_BUGREPORT << "\n";
		exit(0);
	    }

	scm_init_guile();

	// let use use :keyword instead of #:keyword
	scm_c_eval_string("(read-set! keywords 'prefix)");

	// setup path to our scheme extensions -- FIXME: should this
	// path be this hard-wired?
	scm_c_eval_string("(set! %load-path "
			  "   (cons \"/usr/local/share/coasim/scheme\""
			  "   %load-path))");
	const char *home_dir = getenv("HOME");
	if (home_dir)
	    {
		std::string home_module_path;
		home_module_path = "(set! %load-path (cons \"";
		home_module_path += home_dir;
		home_module_path += "/.coasim/\" %load-path))";
		scm_c_eval_string(home_module_path.c_str());
	    }

	
	// install bindings -- to both toplevel and module
	init_scheme_bindings(0); // FIXME: I am not sure this is safe,
				 // since I install the datatypes
				 // twice!!!
	scm_c_define_module("coasim", init_scheme_bindings, 0);

	// run global initialization scripts
	read_home_rc_file();

	// run remaining arguments as scripts
	const char *fname = poptGetArg(ctxt);
	if (!fname and !options::interactive)
	    {
		std::cerr << "At least one run-script expected!\n";
		return 1;
	    }

	while (fname)
	    {
		if (options::verbose) 
		    std::cout << "Executing " << fname << std::endl;
		scm_c_primitive_load(fname);

		fname = poptGetArg(ctxt);
	    }

	if (options::interactive)
	    {
		// setup for interactive shell...
		scm_c_eval_string("(use-modules (ice-9 readline))");
		scm_c_eval_string("(activate-readline)");
		scm_c_eval_string("(set-repl-prompt! \"CoaSim> \")");

		scm_shell(0, 0);
	    }


    } catch (std::exception &ex) {
	std::cerr << "Unexpected exception: " << ex.what() << std::endl;
	return 1;
    }

    return 0;
}
