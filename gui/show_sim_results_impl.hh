#ifndef SHOW_SIM_RESULTS_IMPL_HH_INCLUDED
#define SHOW_SIM_RESULTS_IMPL_HH_INCLUDED

#ifndef RUNSIMULATIONFORM_H_INCLUDED
# include "showsimresults.h"
# define RUNSIMULATIONFORM_H_INCLUDED
#endif

class Configuration;
class ARG;
class ShowSimResultsImpl : public ShowSimResultsForm
{
  Q_OBJECT

public:
  ShowSimResultsImpl(Configuration &conf, ARG &arg,
		     QWidget* parent = 0, 
		     const char* name = 0, WFlags fl = 0 );
  ~ShowSimResultsImpl();

  virtual void save_xml();
  virtual void save_text();

private:
  Configuration &i_conf;
  ARG &i_arg;
};

#endif
